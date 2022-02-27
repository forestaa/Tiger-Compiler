module Tiger.Semant.Level where

import Control.Monad.Error.Class
import Control.Monad.State.Class
import Data.Extensible
import Data.Extensible.Effect
import Data.Extensible.Effect.Default
import Frame qualified as F
import IR
import RIO
import RIO.List.Partial qualified as List
import RIO.Partial qualified as Partial
import Unique

data Level f = TopLevel | Level {unique :: Unique, frame :: f} deriving (Show)

newtype NestingLevel f = NestingLevel [Level f]

outermost :: NestingLevel f
outermost = NestingLevel [TopLevel]

pushNestingLevel :: Level f -> NestingLevel f -> NestingLevel f
pushNestingLevel level (NestingLevel levels) = NestingLevel $ level : levels

popNestingLevel :: NestingLevel f -> Maybe (Level f, NestingLevel f)
popNestingLevel (NestingLevel []) = Nothing
popNestingLevel (NestingLevel (level : levels)) = Just (level, NestingLevel levels)

pullInStaticLinks :: forall f. F.Frame f => Level f -> NestingLevel f -> Maybe IR.Exp
pullInStaticLinks level current = leaveEff . runMaybeDef . (`runReaderDef` level) . (`evalStateDef` IR.Temp (F.fp @f)) $ pullInStaticLinksInternal current
  where
    pullInStaticLinksInternal level = case popNestingLevel level of
      Just (current@Level {}, levels) ->
        ask >>= \case
          TopLevel -> throwError ()
          target@Level {} ->
            if target.unique == current.unique
              then get
              else do
                modify . F.exp . List.head $ F.formals current.frame
                pullInStaticLinksInternal levels
      _ -> throwError ()

takeParametersAccess :: F.Frame f => Level f -> Maybe [F.Access f]
takeParametersAccess TopLevel = Nothing
takeParametersAccess level@Level {} = Just . List.tail $ F.formals level.frame

type NestingLevelEff f = State (Record '["unique" >: Unique, "level" >: NestingLevel f])

runNestingLevelEff :: Eff (("nestingLevel" >: NestingLevelEff f) ': xs) a -> Eff xs a
runNestingLevelEff = flip evalStateEff (#unique @= uniqueSeed <: #level @= outermost <: nil)

getNestingLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => Eff xs (NestingLevel f)
getNestingLevelEff = getsEff #nestingLevel (^. #level)

pushLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => Level f -> Eff xs ()
pushLevelEff level = modifyEff #nestingLevel $ over #level (pushNestingLevel level)

popLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => Eff xs (Maybe (Level f))
popLevelEff = do
  levels <- getNestingLevelEff
  case popNestingLevel levels of
    Nothing -> pure Nothing
    Just (level, levels) -> do
      modifyEff #nestingLevel $ set #level levels
      pure $ Just level

fetchCurrentLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => Eff xs (Level f)
fetchCurrentLevelEff = fst . Partial.fromJust . popNestingLevel <$> getNestingLevelEff

modifyCurrentLevelEff :: Lookup xs "nestingLevel" (NestingLevelEff f) => (Level f -> Level f) -> Eff xs ()
modifyCurrentLevelEff f =
  popLevelEff >>= \case
    Nothing -> pure ()
    Just level -> pushLevelEff $ f level

pullInStaticLinksEff :: (Lookup xs "nestingLevel" (NestingLevelEff f), F.Frame f) => Level f -> Eff xs IR.Exp
pullInStaticLinksEff level = Partial.fromJust . pullInStaticLinks level <$> getNestingLevelEff

fetchCurrentLevelParametersAccessEff :: (F.Frame f, Lookup xs "nestingLevel" (NestingLevelEff f)) => Eff xs [F.Access f]
fetchCurrentLevelParametersAccessEff = Partial.fromJust . takeParametersAccess <$> fetchCurrentLevelEff

withNewLevelEff :: forall f xs a. (Lookup xs "temp" UniqueEff, Lookup xs "nestingLevel" (NestingLevelEff f), F.Frame f) => Label -> [Bool] -> Eff xs a -> Eff xs a
withNewLevelEff label formals a = do
  u <- getUniqueEff #nestingLevel
  frame <- F.newFrame label (True : formals)
  let level = Level u frame
  withLevelEff level a

withLevelEff :: (Lookup xs "nestingLevel" (NestingLevelEff f)) => Level f -> Eff xs a -> Eff xs a
withLevelEff level a = do
  pushLevelEff level
  ret <- a
  _ <- popLevelEff
  pure ret

allocateLocalOnCurrentLevel :: (Lookup xs "temp" UniqueEff, F.Frame f, Lookup xs "nestingLevel" (NestingLevelEff f)) => Bool -> Eff xs (F.Access f)
allocateLocalOnCurrentLevel b =
  fetchCurrentLevelEff >>= \case
    TopLevel -> undefined
    level@Level {} -> do
      (frame', access) <- F.allocLocal level.frame b
      modifyCurrentLevelEff $ const level {frame = frame'}
      pure access

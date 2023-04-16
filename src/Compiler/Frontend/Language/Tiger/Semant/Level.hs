module Compiler.Frontend.Language.Tiger.Semant.Level where

import Compiler.Intermediate.Frame qualified as F
import Compiler.Intermediate.IR qualified as IR
import Compiler.Intermediate.Unique
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Data.Extensible
import Data.Extensible.Effect
import Data.Extensible.Effect.Default
import RIO
import RIO.List.Partial qualified as List
import RIO.Partial qualified as Partial

data Level f = TopLevel | Level {unique :: Unique, frame :: f} deriving (Show)

newtype NestingLevel f = NestingLevel [Level f] deriving (Show)

outermost :: NestingLevel f
outermost = NestingLevel [TopLevel]

pushNestingLevel :: Level f -> NestingLevel f -> NestingLevel f
pushNestingLevel level (NestingLevel levels) = NestingLevel $ level : levels

popNestingLevel :: NestingLevel f -> Maybe (Level f, NestingLevel f)
popNestingLevel (NestingLevel []) = Nothing
popNestingLevel (NestingLevel (level : levels)) = Just (level, NestingLevel levels)

pullInStaticLinks :: forall f. (F.Frame f) => Level f -> NestingLevel f -> Maybe IR.Exp
pullInStaticLinks level current = leaveEff . runMaybeDef . (`runReaderDef` level) . (`evalStateDef` IR.Temp (F.fp @f)) $ pullInStaticLinksInternal current
  where
    pullInStaticLinksInternal level = case popNestingLevel level of
      Just (current@Level {}, levels) ->
        ask >>= \case
          TopLevel -> throwError ()
          target@Level {}
            | target.unique == current.unique -> get
            | otherwise -> do
                modify . F.exp . List.head $ F.formals current.frame
                pullInStaticLinksInternal levels
      _ -> throwError ()

takeParametersAccess :: (F.Frame f) => Level f -> Maybe [F.Access f]
takeParametersAccess TopLevel = Nothing
takeParametersAccess level@Level {} = Just . List.tail $ F.formals level.frame

data NestingLevelState f = NestingLevelState {unique :: Unique, level :: NestingLevel f}

instance HasUnique (NestingLevelState f) where
  getUnique s = s.unique
  putUnique u s = s {unique = u}

type NestingLevelEff f = State (NestingLevelState f)

runNestingLevelEff :: Eff (("nestingLevel" >: NestingLevelEff f) ': xs) a -> Eff xs (a, NestingLevel f)
runNestingLevelEff eff = second (\s -> s.level) <$> flip runStateEff (NestingLevelState uniqueSeed outermost) eff

getNestingLevelEff :: (Lookup xs "nestingLevel" (NestingLevelEff f)) => Eff xs (NestingLevel f)
getNestingLevelEff = getsEff #nestingLevel $ \s -> s.level

pushLevelEff :: (Lookup xs "nestingLevel" (NestingLevelEff f)) => Level f -> Eff xs ()
pushLevelEff newLevel = modifyEff #nestingLevel $ \s@NestingLevelState {level} -> s {level = (pushNestingLevel newLevel) level}

popLevelEff :: (Lookup xs "nestingLevel" (NestingLevelEff f)) => Eff xs (Maybe (Level f))
popLevelEff = do
  levels <- getNestingLevelEff
  case popNestingLevel levels of
    Nothing -> pure Nothing
    Just (level, levels) -> do
      modifyEff #nestingLevel $ \s -> s {level = levels}
      pure $ Just level

fetchCurrentLevelEff :: (Lookup xs "nestingLevel" (NestingLevelEff f)) => Eff xs (Level f)
fetchCurrentLevelEff = fst . Partial.fromJust . popNestingLevel <$> getNestingLevelEff

modifyCurrentLevelEff :: (Lookup xs "nestingLevel" (NestingLevelEff f)) => (Level f -> Level f) -> Eff xs ()
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
  withLevelEff (Level u frame) a

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

allocateTempOnCurrentLevel ::
  ( F.Frame f,
    Lookup xs "nestingLevel" (NestingLevelEff f),
    Lookup xs "temp" UniqueEff
  ) =>
  Eff xs IR.Exp
allocateTempOnCurrentLevel = do
  access <- allocateLocalOnCurrentLevel False
  F.exp access <$> (pullInStaticLinksEff =<< fetchCurrentLevelEff)

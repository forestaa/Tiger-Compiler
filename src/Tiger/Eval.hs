{-# LANGUAGE TypeApplications      #-}

module Tiger.Eval where


-- import Data.Extensible
-- import Data.Extensible.Effect
-- import Data.Extensible.Effect.Default

-- import qualified Tiger.Syntax as T

-- data Value = VNil | VInt Int | VString String

-- type IODef = "IO" >: IO
-- runIODef :: Eff '[IODef] r -> IO r
-- runIODef = retractEff

-- run :: Env -> T.Exp -> IO (Either String (), Env)
-- run env stm = runIODef $ runStateEff @ "env" (runEitherDef $ eval stm) env

-- eval :: T.Exp -> Eff '[EitherDef String, "env" >: State Env, IODef] Value
-- eval T.Nil = return VNil
-- eval (T.Int i) = return (VInt i)
-- eval (T.String s) = rerturn (VString s)
-- eval a@(ArrayCreate{}) = do
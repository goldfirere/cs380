
evalFactorR (Var fin) = get >>= \env -> return (env !!> fin)
evalFactorR (Var fin) = Reader id >>= \env -> return (env !!> fin)
evalFactorR (Var fin)
  = Reader $ \env -> case (\env -> return (env !!> fin)) (id env) of
                       Reader b -> b env
evalFactorR (Var fin)
  = Reader $ \env -> case return (env !!> fin) of
                       Reader b -> b env
evalFactorR (Var fin)
  = Reader $ \env -> case Reader $ \_ -> (env !!> fin) of
                       Reader b -> b env
evalFactorR (Var fin) = Reader $ \env -> (\_ -> (env !!> fin)) env
evalFactorR (Var fin) = Reader $ \env -> env !!> fin

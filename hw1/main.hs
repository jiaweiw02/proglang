import PA1Helper
import System.Environment (getArgs)

-- Haskell representation of lambda expression
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO ()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda _ _) = lexp
id' lexp@(Apply _ _) = lexp 

-- You will need to write a reducer that does something more than
-- return whatever it was given, of course!

betaReducer :: String -> Lexp -> Lexp -> Lexp
betaReducer i (Atom x) val
    | x == i = val
    | otherwise = Atom x
betaReducer i (Lambda x exp) val = Lambda x (betaReducer i exp val)
betaReducer i (Apply exp1 exp2) val = Apply (betaReducer i exp1 val) (betaReducer i exp2 val)

etaConverter :: String -> Lexp -> Lexp -> Lexp -> Lexp
etaConverter i lexp e (Atom x)
    | x /= i = lexp
    | otherwise = e
etaConverter i lexp e _ = lexp


reducer :: Lexp -> Lexp
reducer exp@(Atom x) = exp
reducer exp@(Lambda x y) = Lambda x (reducer y)
reducer exp@(Apply (Lambda x y) z) = betaReducer x y z
reducer exp@(Apply exp1 exp2) = Apply (reducer exp1) (reducer exp2)
-- reducer lexp = lexp

-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram inFile outFile reducer
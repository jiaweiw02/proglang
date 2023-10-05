import PA1Helper
import System.Environment (getArgs)
import qualified Data.Map as Map

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


remove :: (Eq a) => a -> [a] -> [a]
remove x = filter (\v -> v/=x)


freevars :: Lexp -> [String]
freevars (Atom s)            = [s]
freevars (Lambda v e)        = remove v (freevars e)
freevars (Apply e1 e2)       = (freevars e1)++(freevars e2)


alpha :: Lexp -> [String] -> Lexp
alpha exp@(Atom s) freeVars
    | elem s freeVars = Atom(s ++ "0")
    | otherwise = Atom s
alpha exp@(Lambda s e) freeVars
    | elem s freeVars = Lambda newS (alpha e freeVars)
    | otherwise = Lambda s (alpha e freeVars)
    where newS = s ++ "0"
alpha exp@(Apply e1 e2) freeVars = Apply (alpha e1 freeVars) (alpha e2 freeVars)


beta :: String -> Lexp -> Lexp -> Lexp -> Lexp
beta x e@(Atom tmp) m lexp
    | x == tmp = m
    | otherwise = e
beta x e@(Lambda tmp1 tmp2) m lexp
    | elem tmp1 (freevars lexp) = Lambda newtmp1 (beta x newtmp2 m lexp)
    | otherwise = Lambda tmp1 (beta x tmp2 m lexp)
    where newtmp1 = tmp1 ++ "0" 
          newtmp2 = alpha tmp2 (freevars lexp)
beta x e@(Apply tmp1 tmp2) m lexp = Apply (beta x tmp1 m lexp) (beta x tmp2 m lexp)


eta :: String -> Lexp -> Lexp -> Lexp -> Lexp
eta x e m@(Atom tmp) lexp
    | x == tmp && notElem tmp (freevars e) = e
    | otherwise = lexp
eta x e _ lexp = lexp


reducer :: Lexp -> Lexp
reducer (Atom s) = Atom s
reducer exp@(Apply (Lambda s e) v)
    | exp == reduced = exp
    | otherwise = reducer reduced
    where reduced = beta s e v exp

reducer exp@(Apply e1 e2) 
    | exp == reduced = exp
    | otherwise = reducer reduced
    where reduced = Apply (reducer e1) (reducer e2)

reducer exp@(Lambda s exp2@(Apply (Lambda s1 e) v)) 
    | exp == reduced = exp
    | otherwise = reducer reduced
    where reduced = Lambda s (beta s1 e v exp2)

reducer exp@(Lambda s (Apply e1 e2))
    | exp == reduced = exp
    | otherwise = reducer reduced
    where reduced = eta s e1 e2 exp
reducer exp@(Lambda s e)
    | exp == reduced = exp
    | otherwise = reducer reduced
    where reduced = Lambda s (reducer e)



-- Entry point of program
main = do
    args <- getArgs
    let inFile = case args of { x:_ -> x; _ -> "input.lambda" }
    let outFile = case args of { x:y:_ -> y; _ -> "output.lambda"}
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram inFile outFile reducer
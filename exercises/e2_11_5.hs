compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

add1 :: Int -> Int
add1 x = x + 1

mult2 :: Int -> Int
mult2 x = x * 2

main :: IO ()
main = do
    putStrLn("Result " ++ show (compose add1 mult2 4))
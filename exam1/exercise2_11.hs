
composition :: (b -> c) -> (a -> b) -> a -> c
composition f g x = f (g x)

main :: IO ()
main = do
    putStrLn "Hello world!"
main = do str <- getContents
          let inteiros = map read $ words str
          resp inteiros 1

resp (x:xs) n
    | drop x xs == [] = do putStrLn ("Instancia " ++ show n)
                           encontra (take x xs) 0
                           putStrLn ""
    | otherwise       = do putStrLn ("Instancia " ++ show n)
                           encontra (take x xs) 0
                           putStrLn ""
                           resp (drop x xs) (n+1)

encontra (x:xs) s
  | s == x      = do putStrLn (show x)
  | xs ==[]     = do putStrLn "nao achei"
  | otherwise   = do encontra xs (s+x)
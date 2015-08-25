main = do str <- getContents
          let inteiros = map read $ words str
          func inteiros 1


func :: [Integer] -> Integer -> IO()
func (0:0:_) n  = return()
func (a:b:xs) n = do exibe (b:(take (fromIntegral a) xs)) n
                     func (drop (fromIntegral a) xs) (n+1)


exibe (b:xs) n = do putStrLn ("Bill #" ++ show n ++ " costs " ++ show soma ++ ": each friend should pay " ++ show (div soma b) ++ "\n")
                  where
                    soma = sum xs









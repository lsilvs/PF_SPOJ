import Data.Char

main = do str <- getContents
          let entrada = words str
          resp entrada

resp (x:[]) = do imprime x
resp (x:xs) = do imprime x
                 resp xs

imprime w = do if (paprima w) then putStrLn "It is a prime word." else putStrLn "It is not a prime word."

paprima w = ehPrimo (palavraVal w) 2

palavraVal :: String -> Int
palavraVal [] = 0
palavraVal (x:xs)
  | x >= 'a'  = palavraVal xs + (ord x - 96)
  | otherwise = palavraVal xs + (ord x - 38)

ehPrimo :: Int -> Int -> Bool
ehPrimo n d
 | d > (div n 2) = True
 | otherwise = (not ((mod n d) == 0)) && (ehPrimo n next)
 		where
 			next = if d == 2 then 3 else (d+2)
 
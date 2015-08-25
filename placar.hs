main = do str <- getContents
          let entrada = words str
          resp entrada 1

resp (x:xs) n
    | drop (2 * toInt x) xs == [] = do imprime (x:xs) n
    | otherwise                   = do imprime (x:xs) n
                                       resp (drop (2 * toInt x) xs) (n+1)

imprime (a:b:c:xs) n = do putStrLn $ "Instancia " ++ show n
                      infelizreprovado (tupla $ take (2 * toInt a) xs) (b, toInt c)
                      putStrLn ""

infelizreprovado [] (aluno2, _) = do putStrLn aluno2
infelizreprovado ((aluno, nota):xs) (aluno2, nota2)
  | (nota < nota2)                      = do infelizreprovado xs (aluno, nota)
  | (nota == nota2) && (aluno > aluno2) = do infelizreprovado xs (aluno, nota)
  | otherwise                           = do infelizreprovado xs (aluno2, nota2)

tupla (a:b:y)
  | y == []   = (a, (toInt b)):[]
  | otherwise = (a, (toInt b)):(tupla y)

toInt x = (read x :: Int)
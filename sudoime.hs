main = do str <- getContents
          let entrada = map read $ words str
          resp (head entrada) (tail entrada) 1

resp 0 _ _ = return()
resp inst entr n = do imprime (take (9*9) entr) n
                      resp (inst-1) (drop (9*9) entr) (n+1)

imprime entr n = do putStrLn $ "Instancia " ++ show n
                    if ((confere linhas) && (confere colunas) && (confere regiao)) then putStrLn "SIM" else putStrLn "NAO"
                    putStrLn ""
                 where
                  linhas  = (getLinhas  entr)
                  colunas = (getColunas entr ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]) 1)
                  regiao  = (getRegiao  entr ([]:[]:[]:[]:[]:[]:[]:[]:[]:[]) 1)

confere []      = True
confere (a:xs)  = (confereParte a []) && (confere xs)

confereParte [] _ = True
confereParte (a:xs) aux
  | elem a aux    = False
  | otherwise     = (confereParte xs (a:aux)) && True

getLinhas []    = []
getLinhas entr  = (take 9 entr):(getLinhas (drop 9 entr))

getColunas [] t _ = t
getColunas (a:xs) (c1:c2:c3:c4:c5:c6:c7:c8:c9:[]) n
  | mod n 9 == 1 = getColunas xs ((c1++[a]):(c2):(c3):(c4):(c5):(c6):(c7):(c8):(c9):[]) (n+1)
  | mod n 9 == 2 = getColunas xs ((c1):(c2++[a]):(c3):(c4):(c5):(c6):(c7):(c8):(c9):[]) (n+1)
  | mod n 9 == 3 = getColunas xs ((c1):(c2):(c3++[a]):(c4):(c5):(c6):(c7):(c8):(c9):[]) (n+1)
  | mod n 9 == 4 = getColunas xs ((c1):(c2):(c3):(c4++[a]):(c5):(c6):(c7):(c8):(c9):[]) (n+1)
  | mod n 9 == 5 = getColunas xs ((c1):(c2):(c3):(c4):(c5++[a]):(c6):(c7):(c8):(c9):[]) (n+1)
  | mod n 9 == 6 = getColunas xs ((c1):(c2):(c3):(c4):(c5):(c6++[a]):(c7):(c8):(c9):[]) (n+1)
  | mod n 9 == 7 = getColunas xs ((c1):(c2):(c3):(c4):(c5):(c6):(c7++[a]):(c8):(c9):[]) (n+1)
  | mod n 9 == 8 = getColunas xs ((c1):(c2):(c3):(c4):(c5):(c6):(c7):(c8++[a]):(c9):[]) (n+1)
  | mod n 9 == 0 = getColunas xs ((c1):(c2):(c3):(c4):(c5):(c6):(c7):(c8):(c9++[a]):[]) (n+1)

getRegiao [] t _ = t
getRegiao (a:xs) (c1:c2:c3:c4:c5:c6:c7:c8:c9:[]) n
  | elem n [1,2,3,10,11,12,19,20,21]    = getRegiao xs ((c1++[a]):(c2):(c3):(c4):(c5):(c6):(c7):(c8):(c9):[]) (n+1)
  | elem n [4,5,6,13,14,15,22,23,24]    = getRegiao xs ((c1):(c2++[a]):(c3):(c4):(c5):(c6):(c7):(c8):(c9):[]) (n+1)
  | elem n [7,8,9,16,17,18,25,26,27]    = getRegiao xs ((c1):(c2):(c3++[a]):(c4):(c5):(c6):(c7):(c8):(c9):[]) (n+1)
  | elem n [28,29,30,37,38,39,46,47,48] = getRegiao xs ((c1):(c2):(c3):(c4++[a]):(c5):(c6):(c7):(c8):(c9):[]) (n+1)
  | elem n [31,32,33,40,41,42,49,50,51] = getRegiao xs ((c1):(c2):(c3):(c4):(c5++[a]):(c6):(c7):(c8):(c9):[]) (n+1)
  | elem n [34,35,36,43,44,45,52,53,54] = getRegiao xs ((c1):(c2):(c3):(c4):(c5):(c6++[a]):(c7):(c8):(c9):[]) (n+1)
  | elem n [55,56,57,64,65,66,73,74,75] = getRegiao xs ((c1):(c2):(c3):(c4):(c5):(c6):(c7++[a]):(c8):(c9):[]) (n+1)
  | elem n [58,59,60,67,68,69,76,77,78] = getRegiao xs ((c1):(c2):(c3):(c4):(c5):(c6):(c7):(c8++[a]):(c9):[]) (n+1)
  | elem n [61,62,63,70,71,72,79,80,81] = getRegiao xs ((c1):(c2):(c3):(c4):(c5):(c6):(c7):(c8):(c9++[a]):[]) (n+1)

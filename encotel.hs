main = do str <- getContents
          let telefones = words str
          func telefones


func :: [String] -> IO()
func [] = return()
func (a:xs) = do putStrLn (encontra a)
                 func xs


encontra :: String -> String
encontra [] = []
encontra (a:xs)
	| elem a ['A','B','C'] = '2':(encontra xs)
	| elem a ['D','E','F'] = '3':(encontra xs)
	| elem a ['G','H','I'] = '4':(encontra xs)
	| elem a ['J','K','L'] = '5':(encontra xs)
	| elem a ['M','N','O'] = '6':(encontra xs)
	| elem a ['P','Q','R','S'] = '7':(encontra xs)
	| elem a ['T','U','V'] = '8':(encontra xs)
	| elem a ['W','X','Y','Z'] = '9':(encontra xs)
	| otherwise = a:(encontra xs)













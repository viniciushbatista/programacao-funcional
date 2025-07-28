import Data.Char

--alfabeto
alfabeto :: String
alfabeto = ['a'..'z']

--chave = "ZYNGWQAMXPKVULCEFRIBSJDOTH"

substituir :: Char -> [(Char, Char)] -> Char
substituir c tabela =
  if c `elem` [x | (x, _) <- tabela]
    then head [y | (x, y) <- tabela, x == c]
    else c

--criptografa
monoAlphaCipherE :: String -> String -> String
monoAlphaCipherE chave texto =
  [ if c `elem` alfabeto then substituir c tabela else c | c <- normalizeTexto texto ]
  where
    tabela = zip alfabeto chave

--descripografa
monoAlphaCipherD :: String -> String -> String
monoAlphaCipherD chave texto =
  [ if c `elem` chave then substituir c tabela else c | c <- texto ]
  where
    tabela = zip chave alfabeto 

--parte de normalizaçao do texto
normalizeTexto :: String -> String
normalizeTexto xs = tiraAcentoS (paraMinusculo xs)

paraMinusculo :: String -> String
paraMinusculo xs = [toLower x | x <- xs]

tiraAcentoC :: Char -> Char
tiraAcentoC c  | c `elem` "áàâãä" = 'a'
               | c `elem` "éèêë" = 'e'
               | c `elem` "íìîï" = 'i'
               | c `elem` "óòôõö" = 'o'
               | c `elem` "úùûü" = 'u'
               | c `elem` "ç" = 'c'
               | otherwise = c

tiraAcentoS :: String -> String
tiraAcentoS xs = [tiraAcentoC x | x <- xs]
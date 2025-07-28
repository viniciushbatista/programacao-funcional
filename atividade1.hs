import Data.Char

--alfabeto
alfabeto :: String
alfabeto = ['a'..'z']

--         abcdefghijklmnopqrstuvwxyz
--chave = "ZYNGWQAMXPKVULCEFRIBSJDOTH"

substituir :: Char -> [(Char, Char)] -> Char
substituir c tabela =
  if c `elem` [x | (x, _) <- tabela]
    then head [y | (x, y) <- tabela, x == c]
    else c

--criptografa
monoAlphaCipherE :: [Char] -> String -> String
monoAlphaCipherE chave texto =
  [ if c `elem` alfabeto then substituir c tabela else c | c <- normalizeTexto texto ]
  where
    tabela = zip alfabeto chave

--descriptografa
monoAlphaCipherD :: [Char] -> String -> String
monoAlphaCipherD chave texto =
  [ if c `elem` chave then substituir c tabela else c | c <- texto ]
  where
    tabela = zip chave alfabeto 

--parte de normalização do texto
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
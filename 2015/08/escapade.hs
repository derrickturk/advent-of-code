fromHex :: Char -> Char -> Char
fromHex d1 d2 = toEnum $ hexDigit d1 * 16 + hexDigit d2 where
  hexDigit :: Char -> Int
  hexDigit '0' = 0
  hexDigit '1' = 1
  hexDigit '2' = 2
  hexDigit '3' = 3
  hexDigit '4' = 4
  hexDigit '5' = 5
  hexDigit '6' = 6
  hexDigit '7' = 7
  hexDigit '8' = 8
  hexDigit '9' = 9
  hexDigit 'a' = 10
  hexDigit 'b' = 11
  hexDigit 'c' = 12
  hexDigit 'd' = 13
  hexDigit 'e' = 14
  hexDigit 'f' = 15
  hexDigit _ = 0

unescape :: String -> String
unescape "" = ""
unescape ('"':"") = ""
unescape ('"':rest) = unescape rest
unescape ('\\':'\\':rest) = '\\':unescape rest
unescape ('\\':'"':rest) = '"':unescape rest
unescape ('\\':'x':d1:d2:rest) = fromHex d1 d2:unescape rest
unescape (c:rest) = c:unescape rest

escape :: String -> String
escape s = "\"" <> escape' s <> "\"" where
  escape' "" = ""
  escape' ('"':rest) = '\\':'"':escape' rest
  escape' ('\\':rest) = '\\':'\\':escape' rest
  escape' (c:rest) = c:escape' rest

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ sum $ fmap (\l -> length l - length (unescape l)) input
  print $ sum $ fmap (\l -> length (escape l) - length l) input

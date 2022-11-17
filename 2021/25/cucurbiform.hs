import qualified SeaFloor as SF

main :: IO ()
main = do
  floor <- SF.fromLines . lines <$> getContents
  print floor

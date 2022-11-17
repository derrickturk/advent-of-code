import qualified SeaFloor as SF

stepsToStop :: SF.SeaFloor -> Int
stepsToStop = go 0 where
  go n f = let f' = SF.step f
               n' = n + 1
            in if f == f' then n' else go n' f'

renderSteps :: SF.SeaFloor -> Int -> IO ()
renderSteps _ 0 = pure ()
renderSteps f n = do
  putStrLn (SF.pprint f)
  putStrLn ""
  renderSteps (SF.step f) (n - 1)

main :: IO ()
main = do
  sf <- SF.fromLines . lines <$> getContents
  print $ stepsToStop sf
  -- renderSteps sf 10

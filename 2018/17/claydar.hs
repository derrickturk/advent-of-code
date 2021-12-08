{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S

import FemtoParsec

data CoordSpec
  = Solo Int
  | Range Int Int
  deriving Show

data ClaySpec = ClaySpec CoordSpec CoordSpec
  deriving Show

expandCoord :: CoordSpec -> [Int]
expandCoord (Solo n) = [n]
expandCoord (Range a b) = [a..b]

expand :: ClaySpec -> [(Int, Int)]
expand (ClaySpec x y) = (,) <$> expandCoord x <*> expandCoord y

claySpec :: Parser ClaySpec
claySpec = do
  (var1, spec1) <- coordSpec
  _ <- ", "
  (var2, spec2) <- coordSpec
  case (var1, var2) of
    ('x', 'y') -> pure $ ClaySpec spec1 spec2
    ('y', 'x') -> pure $ ClaySpec spec2 spec1
    _ -> empty
  where
    coordSpec = do
      var <- charP (\c -> c == 'x' || c == 'y')
      _ <- "="
      spec <- Range <$> (unsignedIntNum <* "..") <*> unsignedIntNum
          <|> Solo <$> unsignedIntNum
      pure (var, spec)

main :: IO ()
main = do
  Just clays <- parseStdin $ many $ lexeme claySpec
  let clays' = concatMap expand clays
      xmin = minimum $ fst <$> clays'
      xmax = maximum $ fst <$> clays'
      ymin = minimum $ snd <$> clays'
      ymax = maximum $ snd <$> clays'
      clays'' = S.fromList clays'
      clayline y = (\x -> clayChar (x, y)) <$> [xmin..xmax]
      clayChar p@(x, y) = if S.member p clays''
                            then '#'
                            else if y == ymin && x == 500
                              then '+'
                              else ' '
  mapM_ (putStrLn . clayline) [ymin..ymax]

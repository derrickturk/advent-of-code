{-# LANGUAGE OverloadedRecordDot, OverloadedStrings #-}

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Data.List (foldl')

import FemtoParsec

data Game = Game { num :: Int, winning :: IS.IntSet, have :: IS.IntSet }
  deriving Show

game :: Parser Game
game = Game <$> (lexeme "Card" *> unsignedIntNum <* lexeme ":")
            <*> (IS.fromList <$> some (lexeme unsignedIntNum))
            <*> (lexeme "|" *> (IS.fromList <$> some (lexeme unsignedIntNum)))

winners :: Game -> Int
winners g = IS.size $ IS.intersection g.winning g.have

part1Score :: [Game] -> Int
part1Score = sum . fmap ((\n -> if n == 0 then 0 else 2 ^ (n - 1)) . winners)

part2Score :: [Game] -> Int
part2Score games =
  let initCopies = IM.fromList $ zip (num <$> games) (repeat 1)
      winCounts = IM.fromList $ zip (num <$> games) (winners <$> games)
      step cards k = IM.mapWithKey
        (\i val -> if i > k && i <= k + winCounts IM.! k
                     then val + cards IM.! k
                     else val
        )
        cards
      final = foldl' step initCopies $ IM.keys initCopies
   in sum $ IM.elems final

main :: IO ()
main = do
  games <- parseStdin $ some $ lexeme' game
  print $ part1Score <$> games
  print $ part2Score <$> games

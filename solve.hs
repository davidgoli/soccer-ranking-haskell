import Parse (parseSeason, Season(..), GameContestant(..), Game(..))
import Data.HashMap.Strict (HashMap, empty, insertWith, toList)

data Standing = Standing {
  team :: String
  , points :: Int
} deriving (Show)

data Ranking = Ranking {
  position :: Int
  , standing :: Standing
}

data Rankings = Rankings [Ranking]

data GameResult team = Win team team | Tie team team

gameResult :: Game -> GameResult GameContestant
gameResult (Game teamA teamB)
  | (score teamA) > (score teamB) = Win teamA teamB
  | (score teamB) > (score teamA) = Win teamB teamA
  | otherwise = Tie teamA teamB

tally :: Parse.Season -> [Standing]
tally (Parse.Season games) = collate . toList . (foldl addGame empty) $ games
  where
    collate = fmap (\(k, v) -> Standing { team = k, points = v })


addGame :: HashMap String Int -> Parse.Game -> HashMap String Int
addGame acc game = case gameResult game of
    Win winner loser -> addPoints winner 3 . addPoints loser 0 $ acc
    Tie teamA teamB -> addPoints teamA 1 . addPoints teamB 1 $ acc
  where
    addPoints t = insertWith (+) (Parse.name t)

main = do
  input <- getContents
  putStrLn $ case parseSeason input of
    Right games -> show $ tally games
    Left err -> show err
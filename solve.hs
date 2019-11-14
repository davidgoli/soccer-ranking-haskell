import Parse (parseSeason, Season(..), GameContestant(..), Game(..), trim)
import Data.HashMap.Strict (HashMap, empty, insertWith, toList)
import Data.List

data Standing = Standing {
  team :: String
  , points :: Int
} deriving (Show, Eq)

instance Ord Standing where
  compare (Standing t1 p1) (Standing t2 p2) = if p1 == p2
    then compare t1 t2 -- descending alphabetical order
    else compare p2 p1

data Ranking = Ranking {
  rank :: Int
  , standing :: Standing
} deriving (Show)

data Rankings = Rankings [Ranking]

data GameResult team = Win team team | Tie team team

gameResult :: Game -> GameResult GameContestant
gameResult (Game teamA teamB)
  | (score teamA) > (score teamB) = Win teamA teamB
  | (score teamB) > (score teamA) = Win teamB teamA
  | otherwise = Tie teamA teamB

tally :: Parse.Season -> [Ranking]
tally (Parse.Season games) = rankStandings . sort . collate . toList . (foldl addGame empty) $ games
  where
    collate = fmap (\(k, v) -> Standing { team = k, points = v })


addGame :: HashMap String Int -> Parse.Game -> HashMap String Int
addGame acc game = case gameResult game of
    Win winner loser -> addPoints winner 3 . addPoints loser 0 $ acc
    Tie teamA teamB -> addPoints teamA 1 . addPoints teamB 1 $ acc
  where
    addPoints t = insertWith (+) (Parse.name t)

rankStandings :: [Standing] -> [Ranking]
rankStandings (st:standings) = snd ranked
  where
    ranked :: (Int, [Ranking])
    ranked = foldl ranker (0, [Ranking 1 st]) standings

    ranker :: (Int, [Ranking]) -> Standing -> (Int, [Ranking])
    ranker (skipped, acc) curr =
      if points curr < points prevStanding then
        (0, acc ++ [Ranking (rank prev + skipped + 1) curr] )
      else
        (skipped + 1, acc ++ [Ranking (rank prev) curr])
      where
        prev = last acc
        prevStanding = standing prev

format :: [Ranking] -> [String]
format [] = []
format (r:rankings) = [teamRank ++ ". " ++ teamName ++ ", " ++ (show teamPoints) ++ suffix] ++ format rankings
        where
          teamRank = show $ rank r
          teamName = team (standing r)
          teamPoints = points (standing r)
          suffix = if teamPoints == 1 then " pt" else " pts"

main = do
  input <- getContents
  putStrLn $ case parseSeason input of
    Right games -> trim . unlines . format $ tally games
    Left err -> show err
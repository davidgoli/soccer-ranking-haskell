import qualified Data.Text as T
import Text.Parsec

data GameContestant = GameContestant {
  name :: String
  , score :: Int
} deriving (Show)

data Game = Game GameContestant GameContestant deriving (Show)

season :: Parsec String st [Game]
season = do
  result <- many game
  eof
  return result

game :: Parsec String st Game
game = do
  contestant1 <- team
  char ','
  contestant2 <- team
  skipMany newline
  return $ Game contestant1 contestant2

team :: Parsec String st GameContestant
team = do
  name <- many $ noneOf "0123456789"
  score <- read <$> many digit
  return $ GameContestant { name = trim name, score = score }

trim :: String -> String
trim = T.unpack . T.strip . T.pack

main = do
  input <- getContents
  putStrLn $ case parse season "" input of
    Right games -> show games
    Left err -> show err
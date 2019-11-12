import Parse (parseSeason)

main = do
  input <- getContents
  putStrLn $ case parseSeason input of
    Right games -> show games
    Left err -> show err
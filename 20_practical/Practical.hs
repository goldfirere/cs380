import System.IO

positiveWords = ["good", "happy", "nice"]

chooseResponse "bye" = Nothing
chooseResponse input
--  | or (map (flip elem positiveWords) ws) = Just "I'm happy to hear that."
  | any (flip elem positiveWords) ws = Just "I'm happy to hear that."
  | otherwise                        = Just "Do continue..."
  where
    ws = words input

loop = do
  putStr "> "
  hFlush stdout
  input <- getLine
  let response = chooseResponse input
  case response of
    Nothing -> putStrLn "Good-bye"
    Just resp -> do
      putStrLn resp
      loop

main = do
  putStrLn "Hi there!"
  loop

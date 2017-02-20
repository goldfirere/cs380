module Lab where

import Network.HTTP
import Text.HTML.TagSoup

textOnly :: Tag String -> Bool
textOnly (TagText _) = True
textOnly _           = False

getText :: Tag String -> String
getText (TagText str) = str
getText _             = ""

main = do
  e_rsp <- simpleHTTP (getRequest "http://cs.brynmawr.edu/cs380/submission.html")
  case e_rsp of
    Left err -> print err
    Right rsp -> do
      print rsp
      let body = rspBody rsp
      let tags = parseTags body
      let textParts = map getText $ filter textOnly tags
      mapM_ putStrLn textParts

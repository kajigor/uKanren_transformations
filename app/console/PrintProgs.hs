module PrintProgs where 

import qualified ExamplesToPrint.Bridge as Bridge
import           Printer.PrettyMkPrinter   (prettyMk)

run :: String -> IO ()
run nameExample = do
  case nameExample of
    "Bridge" ->
      putStrLn $ prettyMk Bridge.query
    _ -> 
      putStrLn "incorrect name of example"
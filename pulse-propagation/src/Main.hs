module Main (main) where
import Data.List.Utils (split)
import Data.Foldable (find)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (broadcaster, modules) = extractBroadcaster $ parseInput input
  print broadcaster
  putStrLn ""
  putStrLn $ unlines $ show <$> modules

data Module = Module ModuleType String [String]

data ModuleType = FlipFlop | Conjunction

instance Show Module where
  show (Module t name destinations) = show t ++ show name ++ " -> " ++ show destinations

instance Show ModuleType where
  show FlipFlop = "%"
  show Conjunction = "&"

parseInput :: String -> [Module]
parseInput input = parseModule <$> lines input
  where
    parseModule line = case split " -> " line of
      ["broadcaster", ds] -> Module FlipFlop "broadcaster" (split ", " ds)
      ['%':name, ds] -> Module FlipFlop name (split ", " ds)
      ['&':name, ds] -> Module Conjunction name (split ", " ds)
      _ -> error "Invalid module definition"

extractBroadcaster :: [Module] -> (Module, [Module])
extractBroadcaster modules = case find isBroadcaster modules of
  Just broadcaster -> (broadcaster, filter (not . isBroadcaster) modules)
  Nothing -> error "No broadcaster module found"
  where
    isBroadcaster (Module _ name _) = name == "broadcaster"


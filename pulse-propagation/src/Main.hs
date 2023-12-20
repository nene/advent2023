module Main (main) where
import Data.List.Utils (split)
import Data.Foldable (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (broadcastDests, moduleMap) = parseInput input
  putStrLn $ "Broadcast to: " ++ show broadcastDests
  putStrLn ""
  print moduleMap

data Module = FlipFlop State [String] | Conjunction [(String, Pulse)] [String] deriving (Show)

data State = On | Off deriving (Show)

data Pulse = Low | High deriving (Show)

type RawModule = (Char, String, [String])

parseInput :: String -> ([String], Map String Module)
parseInput input = case extractBroadcastDestinations $ parseToRawModules input of
  (broadcastDests, rawModules) -> (broadcastDests, buildModuleMap rawModules)

parseToRawModules :: String -> [RawModule]
parseToRawModules input = parseModule <$> lines input
  where
    parseModule line = case split " -> " line of
      ["broadcaster", ds] -> ('b', "broadcaster", split ", " ds)
      [c:name, ds] -> (c, name, split ", " ds)
      _ -> error "Invalid module definition"

extractBroadcastDestinations :: [RawModule] -> ([String], [RawModule])
extractBroadcastDestinations modules = case find isBroadcaster modules of
  Just (_, _, destinations) -> (destinations, filter (not . isBroadcaster) modules)
  Nothing -> error "No broadcaster module found"
  where
    isBroadcaster (_, name, _) = name == "broadcaster"

buildModuleMap :: [RawModule] -> Map String Module
buildModuleMap rawModules = Map.fromList $ nameModulePair <$> rawModules
  where
    nameModulePair ('%', name, ds) = (name, FlipFlop Off ds)
    nameModulePair (_, name, ds) = (name, Conjunction (toLowPair <$> inputsOf name) ds)

    toLowPair name = (name, Low)

    inputsOf name = extractName <$> filter (\(_, _, ds) -> name `elem` ds) rawModules

    extractName (_, name, _) = name

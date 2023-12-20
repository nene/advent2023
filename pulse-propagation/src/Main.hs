module Main (main) where
import Data.List.Utils (split)
import Data.Foldable (find)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (broadcastDests, moduleMap) = parseInput input
  putStrLn $ "Broadcast to: " ++ show broadcastDests
  putStrLn ""
  print moduleMap
  print $ process (moduleMap ! "a") ("broadcast", Low, "a")

type ModuleMap = Map String Module

data Module = FlipFlop State [String] | Conjunction [(String, Pulse)] [String] deriving (Show)

data State = On | Off deriving (Show, Eq)

data Pulse = Low | High deriving (Show, Eq)

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

buildModuleMap :: [RawModule] -> ModuleMap
buildModuleMap rawModules = Map.fromList $ nameModulePair <$> rawModules
  where
    nameModulePair ('%', name, ds) = (name, FlipFlop Off ds)
    nameModulePair (_, name, ds) = (name, Conjunction (toLowPair <$> inputsOf name) ds)

    toLowPair name = (name, Low)

    inputsOf name = extractName <$> filter (\(_, _, ds) -> name `elem` ds) rawModules

    extractName (_, name, _) = name



-- sendPulses :: [(String, Pulse, String)] -> ModuleMap -> [Pulse]
-- sendPulses ((from, p, to):pulses) moduleMap = process (moduleMap ! to) (from, p, to)


process :: Module -> (String, Pulse, String) -> (Module, [(String, Pulse, String)])
-- If a flip-flop module receives a high pulse, it is ignored and nothing happens.
process m@(FlipFlop _ _) (_, High, _) = (m, [])
-- if a flip-flop module receives a low pulse, it flips between on and off.
-- If it was off, it turns on and sends a high pulse.
-- If it was on, it turns off and sends a low pulse.
process (FlipFlop Off ds) (_, Low, to) = (FlipFlop On ds, [(to, High, d) | d <- ds])
process (FlipFlop On ds) (_, Low, to) = (FlipFlop Off ds, [(to, Low, d) | d <- ds])
-- When a pulse is received, the conjunction module first updates its memory for that input
-- Then, if it remembers high pulses for all inputs, it sends a low pulse;
-- otherwise, it sends a high pulse.
process (Conjunction inputs ds) (from, inPulse, to) = (Conjunction updatedInputs ds, [(to, outPulse, d) | d <- ds])
  where
    updatedInputs = (\(name, p) -> (name, if name == from then inPulse else p)) <$> inputs
    outPulse = if all ((==High) . snd) inputs then Low else High

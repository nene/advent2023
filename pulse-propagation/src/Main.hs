module Main (main) where
import Data.List.Utils (split)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "input.txt"
  let moduleMap = parseInput input
  print moduleMap
  putStrLn $ unlines $ showSignal <$> reverse (pushButton moduleMap)

type ModuleMap = Map String Module

data Module =
    Broadcast [String]
  | FlipFlop State [String]
  | Conjunction [(String, Pulse)] [String] deriving (Show)

data State = On | Off deriving (Show, Eq)

data Pulse = Low | High deriving (Show, Eq)

type Signal = (String, Pulse, String)

type RawModule = (Char, String, [String])

parseInput :: String -> ModuleMap
parseInput input = buildModuleMap $ parseToRawModules input

parseToRawModules :: String -> [RawModule]
parseToRawModules input = parseModule <$> lines input
  where
    parseModule line = case split " -> " line of
      ["broadcaster", ds] -> ('b', "broadcaster", split ", " ds)
      [c:name, ds] -> (c, name, split ", " ds)
      _ -> error "Invalid module definition"

buildModuleMap :: [RawModule] -> ModuleMap
buildModuleMap rawModules = Map.fromList $ nameModulePair <$> rawModules
  where
    nameModulePair ('b', name, ds) = (name, Broadcast ds)
    nameModulePair ('%', name, ds) = (name, FlipFlop Off ds)
    nameModulePair (_, name, ds) = (name, Conjunction (toLowPair <$> inputsOf name) ds)

    toLowPair name = (name, Low)

    inputsOf name = extractName <$> filter (\(_, _, ds) -> name `elem` ds) rawModules

    extractName (_, name, _) = name

showSignal :: Signal -> String
showSignal (from, p, to) = from ++ " -" ++ show p ++ "-> " ++ to

pushButton :: Map String Module -> [Signal]
pushButton moduleMap = sendSignals moduleMap [("button", Low, "broadcaster")] []

sendSignals :: ModuleMap -> [Signal] -> [Signal] -> [Signal]
sendSignals _moduleMap [] processedPulses = processedPulses
sendSignals moduleMap ((from, pulse, to):pulses) processedPulses = case process (moduleMap ! to) (from, pulse, to) of
  (newModule, newPulses) -> sendSignals (Map.insert to newModule moduleMap) (pulses ++ newPulses) ((from, pulse, to):processedPulses)

process :: Module -> Signal -> (Module, [Signal])
-- When broadcast module receives a pulse, it sends the same pulse to all of its destination modules.
process m@(Broadcast ds) (_, inPulse, to) = (m, [(to, inPulse, d) | d <- ds])
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
    outPulse = if all ((==High) . snd) updatedInputs then Low else High

module Code128 where

import Control.Applicative (Alternative (some))
import Data.Char
import Data.Either
import Data.IntMap qualified as Mp
import Data.List
import Data.Map (Map, lookup, (!))
import Data.Map qualified as Map
import Data.Sequence (chunksOf)
import Data.Sequence qualified as Data.List.Split
import GHC.Exts (the)
import System.Environment
import System.Exit

-- import qualified GHC.TypeLits as list

--------------------------------------------------------------------------------
-- Data Representation

data BCString -- "Barcode String"
  = BCString [Symbol] --   Start, data, checksum, and stop symbols.
  --   The final bar ("11") is implicit.
  --   No quiet zones.

type Symbol = [Module] -- Always length 11

type Module = Bool -- True/False means part of bar/space

type BC -- "Barcode" (internal representation)
  =
  ( SymbolId, --   Start symbol
    [SymbolId], --   Encoded data
    SymbolId, --   Checksum
    SymbolId --   Stop symbol
  ) --   Final bar is implicit

type SymbolId = Int

type BValue = Either Char String

type CValue = Either (Char, Char) String

data TheCodes = TheCodes
  { startB :: SymbolId,
    startC :: SymbolId,
    stop :: SymbolId,
    idsToSymbols :: Map SymbolId Symbol,
    symbolsToIds :: Map Symbol SymbolId,
    bEncodings :: Map Char SymbolId,
    cEncodings :: Map (Char, Char) SymbolId,
    bDecodings :: Map SymbolId BValue,
    cDecodings :: Map SymbolId CValue
  }
  deriving (Show)

type Error = String

--------------------------------------------------------------------------------
-- 1. Data Loading

loadTheCodes :: IO TheCodes
loadTheCodes = do
  rows <- map (splitOn ',') <$> lines <$> readFile "code128.csv"
  return $ rowsToCodes $ dropFirstAndLast rows

rowsToCodes :: [[String]] -> TheCodes
rowsToCodes rows =
  -- Perfect opportunity for NamedFieldPuns. Try it!
  TheCodes
    { startB = 104,
      startC = 105,
      stop = 106,
      idsToSymbols = idsToSymbols,
      symbolsToIds = symbolsToIds,
      bEncodings = bEncodings,
      cEncodings = cEncodings,
      bDecodings = bDecodings,
      cDecodings = cDecodings
    }
  where
    (idsToSymbols, symbolsToIds, bEncodings, cEncodings, bDecodings, cDecodings) =
      foldr
        processRow
        (Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
        rows

    processRow row accumulators =
      accumulators'
      where
        [_id, _pattern, _, _, _bValue, _cValue] =
          row

        (idsToSymbols, symbolsToIds, bEncodings, cEncodings, bDecodings, cDecodings) =
          accumulators

        accumulators' =
          (idsToSymbols', symbolsToIds', bEncodings', cEncodings', bDecodings', cDecodings')

        symbolId =
          read _id :: SymbolId

        idsToSymbols' =
          Map.insert symbolId (readSymbol _pattern) idsToSymbols

        symbolsToIds' =
          Map.insert (readSymbol _pattern) symbolId symbolsToIds

        bEncodings'
          | _bValue == "space" = Map.insert ' ' symbolId bEncodings
          | _bValue == "comma" = Map.insert ',' symbolId bEncodings
          | otherwise = Map.insert (_bValue !! 0) symbolId bEncodings

        cEncodings' =
          Map.insert ((_cValue !! 0), (_cValue !! 1)) symbolId cEncodings

        bDecodings'
          | symbolId == 0 = Map.insert symbolId (Left ' ') bDecodings
          | symbolId == 12 = Map.insert symbolId (Left ',') bDecodings
          | otherwise = Map.insert symbolId (readBValue _bValue) bDecodings

        cDecodings' =
          Map.insert symbolId (readCValue _cValue) cDecodings

-- from PA3-interval Algebra Main.hs
splitOn :: Char -> String -> [String]
splitOn splitChar [] = [[]]
splitOn splitChar (headChar : restChars)
  | splitChar == headChar = [[]] ++ splitOn splitChar restChars
  | otherwise = (headChar : currentWord) : restWords
  where
    currentWord : restWords = splitOn splitChar restChars

-- idea to use tail with init from a Stack post:
dropFirstAndLast :: [a] -> [a]
dropFirstAndLast [] = []
dropFirstAndLast [x] = []
dropFirstAndLast xs = tail (init xs)

readSymbol :: String -> Symbol
readSymbol "" = []
readSymbol (x : xs) =
  if x == '1'
    then True : readSymbol xs
    else False : readSymbol xs

readBValue :: String -> BValue
readBValue str = if length str == 1 then Left (head str) else Right str

readCValue :: String -> CValue
readCValue str = if length str == 2 then Left (head str, str !! 1) else Right str

--------------------------------------------------------------------------------
-- 2. Basic Encodings

-- helper function to check if a string contains only printable chars
isPrintableString :: String -> Bool
isPrintableString "" = True
isPrintableString (x : xs) = isPrintable x && isPrintableString xs

-- B Set: helper function to return [SymbolIDs] given a string
-- (assuming every char in string is printable)
strToSymbolIDs :: TheCodes -> String -> [SymbolId]
strToSymbolIDs theCodes str =
  case str of
    "" -> []
    (x : xs) -> do
      -- if str contains chars, lookup its bEncoding in map
      case Data.Map.lookup x (bEncodings theCodes) of
        Nothing -> []
        -- append bEncoding for each char to create [SymbolId]
        Just c -> c : strToSymbolIDs theCodes xs

encodeB :: TheCodes -> String -> Either Error BC
encodeB theCodes str = do
  -- first check if string contains all printable chars
  if isPrintableString str
    then do
      -- call helper functions to create coded word & checkSum
      let symbolIDs = strToSymbolIDs theCodes str
      let checkSum = computeChecksum (startB theCodes) symbolIDs
      Right (startB theCodes, symbolIDs, checkSum, stop theCodes)
    else -- otherwise str contains unprintable chars so return error msg
      Left "unsupported characters"

-- C Set: helper function to return [SymbolIDS] given tuples of chars
-- (assuming every char is printable)
tuplesToSymbolIDs :: TheCodes -> [(Char, Char)] -> [SymbolId]
tuplesToSymbolIDs theCodes tups =
  case tups of
    [] -> []
    ((c1, c2) : rest) -> do
      case Data.Map.lookup (c1, c2) (cEncodings theCodes) of
        Nothing -> []
        Just symbId -> symbId : tuplesToSymbolIDs theCodes rest

encodeC :: TheCodes -> String -> Either Error BC
encodeC theCodes str = do
  -- first check if string is of even length & printable
  if (Data.List.all isDigit str) && even (length str)
    then do
      -- use adjacentPairs to group [char] into [tuples]
      let tups = adjacentPairs str
      -- call helper function to create coded word from [tuples] & checkSum
      let symbolIDs = tuplesToSymbolIDs theCodes tups
      let checkSum = computeChecksum (startC theCodes) symbolIDs
      Right (startC theCodes, symbolIDs, checkSum, stop theCodes)
    else -- return appropriate error message

      if odd (length str)
        then Left "odd number of characters"
        else Left "unsupported characters"

computeChecksum :: SymbolId -> [SymbolId] -> Int
computeChecksum symbol symbols = (symbol + sum (map (uncurry (*)) (zip [1 ..] symbols))) `mod` 103

isPrintable :: Char -> Bool
isPrintable c = ord c >= 32 && ord c <= 126

adjacentPairs :: [a] -> [(a, a)] -- assuming list of even length
adjacentPairs [] = []
adjacentPairs [helmet] = undefined
adjacentPairs (helmet1 : helmet2 : rest) = (helmet1, helmet2) : adjacentPairs rest

sequenceMaybe :: [Maybe a] -> Maybe [a]
-- returns a Maybe type that may or may not contain a list inside...
sequenceMaybe maybes = do
  case maybes of
    [] -> Just []
    (Nothing : _) -> Nothing
    (Just a : as) ->
      case sequenceMaybe as of
        Nothing -> Nothing
        Just as' -> Just (a : as')

--------------------------------------------------------------------------------

-- 3. Basic Decodings

-- helper to check if each symbolId in [symbolId] is valid
areValidSymbols :: Ord k => t -> (t -> Map k (Either a b)) -> [k] -> Bool
areValidSymbols theCodes dict symbols =
  case symbols of
    [] -> True
    (symbol : rest) ->
      case Data.Map.lookup symbol (dict theCodes) of
        -- if symbol doesn't exist in map, return False
        Nothing -> False
        Just val ->
          case val of
            -- if symbol is not a char/(char, char), return False
            Left _ -> True && areValidSymbols theCodes dict rest
            Right _ -> False

-- convert a valid symbolId to a char
validSymbolIdToChar :: TheCodes -> SymbolId -> Char
validSymbolIdToChar theCodes symbol =
  -- unsafe lookup because we assume the symbol is valid
  case Data.Map.lookup symbol (bDecodings theCodes) of
    Just x ->
      case x of
        Left c -> c
        -- should technically never hit this Right case
        -- since it's only valid if symbol is a char
        Right _ -> '%'
    Nothing -> '%'

-- helper to convert [symbolId] to str assuming all
-- symbols are valid
symbolIdsToStr :: TheCodes -> [SymbolId] -> String
symbolIdsToStr _ [] = ""
symbolIdsToStr theCodes symbols =
  foldr ((:) . validSymbolIdToChar theCodes) "" symbols

decodeB :: TheCodes -> BC -> Either Error String
decodeB theCodes (start_, data_, checksum_, stop_)
  | start_ /= startB theCodes = Left "decodeB: invalid start value"
  | stop_ /= stop theCodes = Left "decodeB: invalid stop value"
  | checksum_ /= computeChecksum start_ data_ = Left "decodeB: invalid checkSum"
  | not (areValidSymbols theCodes bDecodings data_) = Left "decodeB: not valid symbols"
  | otherwise = Right (symbolIdsToStr theCodes data_)

-----------------------------------------------------------------
-- Helpers for decodeC

-- convert a valid symbolId to a [Char, Char] from cDecodings
validSymbolIdToTupStr :: TheCodes -> SymbolId -> [Char]
validSymbolIdToTupStr theCodes symbol =
  -- unsafe lookup because we assume the symbol is valid
  case Data.Map.lookup symbol (cDecodings theCodes) of
    Just x ->
      case x of
        Left (c1, c2) -> [c1, c2]
        -- should technically never hit this Right case
        -- since it's only valid if symbol is a char
        Right _ -> ['%', '%']
    Nothing -> ['%', '%']

-- helper to convert [symbolId] to str assuming all
-- symbols are valid ([(char, char)] -> [char])
symbolIdsToStrCSet :: TheCodes -> [SymbolId] -> String
symbolIdsToStrCSet _ [] = ""
symbolIdsToStrCSet theCodes symbols =
  concatMap (validSymbolIdToTupStr theCodes) symbols

-----------------------------------------------------------------
decodeC :: TheCodes -> BC -> Either Error String
decodeC theCodes (start_, data_, checksum_, stop_)
  | start_ /= startC theCodes = Left "decodeC: invalid start value"
  | stop_ /= stop theCodes = Left "decodeC: invalid stop value"
  | checksum_ /= computeChecksum start_ data_ = Left "decodeC: invalid checkSum"
  | not (areValidSymbols theCodes cDecodings data_) = Left "decodeC: not valid symbols"
  | otherwise = Right (symbolIdsToStrCSet theCodes data_)

--------------------------------------------------------------------------------
-- 4. Barcode String Manipulation

finalBar = "11"

symbolLength = 11

-- helper to convert [Symbol] -> String
symbolsToStr :: [Module] -> String
symbolsToStr mods = do
  case mods of
    [] -> ""
    (b : bs) ->
      if b
        then "1" ++ (symbolsToStr bs)
        else "0" ++ (symbolsToStr bs)

instance Show BCString where
  show :: BCString -> String
  show (BCString symbols) =
    symbolsToStr (concat symbols) ++ finalBar

instance Read BCString where
  readsPrec :: Int -> String -> [(BCString, String)]
  readsPrec _ str =
    case maybeReadBCString str of
      Just bcString -> [(bcString, "")]
      Nothing -> []

-- helper to check if a str contains only 1s and 0s
isOnlyOnesZeros :: String -> Bool
isOnlyOnesZeros "" = True
isOnlyOnesZeros (x : xs) = ((read [x]) == 1 || (read [x]) == 0) && isOnlyOnesZeros xs

-- helper from a Stack post:
groupTo :: Int -> [a] -> [[a]]
groupTo _ [] = []
groupTo n l
  | n > 0 = (take n l) : (groupTo n (drop n l))
  | otherwise = error "Negative or zero n"

-- note: a BCString = [[Module (length 11)][length 11],[length 11],[]...]
maybeReadBCString :: String -> Maybe BCString
maybeReadBCString str =
  -- checking if length of string is a multiple of 11 after subtracting final bar
  if (length str `mod` 11 == 2) && isOnlyOnesZeros str
    then do
      let removeOne = init str
      let finStr = init removeOne
      let listOfStrings = groupTo 11 finStr
      let symbols = fmap readSymbol listOfStrings

      Just (BCString symbols)
    else Nothing

--------------------------------------------------------------------------------

run :: (TheCodes -> a -> Either Error b) -> a -> IO (Either Error b)
run f a = do
  theCodes <- loadTheCodes
  pure $ f theCodes a

--------------------------------------------------------------------------------
-- User Interface

bcToBCString :: TheCodes -> BC -> BCString
bcToBCString theCodes (start, data_, checksum, stop) =
  let symbolIds = [start] ++ data_ ++ [checksum, stop]
   in --   in BCString $ map (\i -> (idsToSymbols theCodes) ! i) symbolIds

      BCString $
        map
          ( \i -> case Map.lookup i (idsToSymbols theCodes) of
              Just s -> s
              Nothing -> error ("not in map" ++ show i)
          )
          symbolIds

bcStringToBC :: TheCodes -> BCString -> BC
bcStringToBC theCodes (BCString symbols) =
  (start, data_, checksum, stop)
  where
    -- list = map (\symbol -> (symbolsToIds theCodes) ! symbol) symbols
    list =
      map
        ( \symbol -> case Map.lookup symbol (symbolsToIds theCodes) of
            Just l -> l
            Nothing -> error ("not in map " ++ show symbol)
        )
        symbols
    start = head list
    stop = head $ reverse list
    checksum = head $ tail $ reverse list
    data_ = reverse $ tail $ tail $ reverse $ tail list

encodeAndShow ::
  (TheCodes -> String -> Either Error BC) ->
  TheCodes ->
  String ->
  Either Error String
encodeAndShow f theCodes str =
  show . bcToBCString theCodes <$> f theCodes str

readAndDecode ::
  (TheCodes -> BC -> Either Error String) ->
  TheCodes ->
  String ->
  Either Error String
readAndDecode f theCodes str =
  -- Call `maybeReadBCString` instead of `read` because the latter may crash
  case maybeReadBCString str of
    Nothing -> Left "no parse"
    Just bcString ->
      let barcode = bcStringToBC theCodes $ bcString
       in f theCodes barcode

runEncoder ::
  (TheCodes -> String -> Either Error BC) ->
  String ->
  IO ()
runEncoder f str = do
  theCodes <- loadTheCodes
  case encodeAndShow f theCodes str of
    Left error -> die error
    Right s -> putStrLn s

runDecoder ::
  (TheCodes -> BC -> Either Error String) ->
  String ->
  IO ()
runDecoder f str = do
  theCodes <- loadTheCodes
  case readAndDecode f theCodes str of
    Left error -> die error
    Right str -> putStrLn str

main = do
  theCodes <- loadTheCodes
  args <- getArgs
  case args of
    ["encodeB", str] -> runEncoder encodeB str
    ["decodeB", str] -> runDecoder decodeB str
    ["encodeC", str] -> runEncoder encodeC str
    ["decodeC", str] -> runDecoder decodeC str
    _ -> die "Usage: cabal run code128 -- {en,de}code{B,C} string"

-- "Usage: ./Code128 {en,de}code{B,C} string"
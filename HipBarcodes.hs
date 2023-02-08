module HipBarcodes where

-- You may add, remove, or edit imports as you wish. Note the use of
-- qualified imports to avoid collisions between common names. For
-- example, Prelude and Data.Map and Graphics.Image all define `map`.

import Code128
import Data.Char
import Data.Map (Map, lookup, (!))
import Data.Map qualified as Map
import Graphics.Image (Image, Pixel (..), RGB, VU (VU), leftToRight, makeImageR, writeImage)
import Graphics.Image qualified as Image
import Graphics.Image.ColorSpace
import Graphics.Image.Interface (MArray)
import System.Environment
import System.Exit

--------------------------------------------------------------------------------
-- NOTE: Defining these at top-level rather than changing Code128.TheCodes
-- to include them, like the start and stop symbols.

bToC, cToB :: SymbolId
bToC = 99 -- the symbolId for [Code C] in the B code set
cToB = 100 -- the symbolId for [Code B] in the C code set

-----------------------------------------------------------------
-----------------------------------------------------------------
-- structural idea

-- decode :: CD-> Either Error String
--    Look at the start code
--    call decodeB or decodeC as appropriate
--    look at start code, use helpDecode w/ appropriate CodeSet (B or C)

-----------------------------------------------------------------
-----------------------------------------------------------------

--  [Start B] ... [Code C] ... [Code B] ... [Stop]
data CodeSet = B | C

-- helper function to decode symbolId depending on which Codeset it's in
helpDecode :: TheCodes -> CodeSet -> [SymbolId] -> String
helpDecode theCodes _ [] = ""
-- switch to other codeset if we hit a switch. Else, keep decoding in current codeset

-- how can I use "where" to make this cleaner??**
helpDecode theCodes B (x : xs) =
  if x == bToC
    then helpDecode theCodes C xs
    else case Data.Map.lookup x (bDecodings theCodes) of
      Just bVal ->
        case bVal of
          Left chr -> chr : helpDecode theCodes B xs
          Right _ -> "INVALID B VAL"
      Nothing -> "INVALID B VAL"
helpDecode theCodes C (x : xs) =
  if x == cToB
    then helpDecode theCodes B xs
    else case Data.Map.lookup x (cDecodings theCodes) of
      Just cVal ->
        case cVal of
          Left (ch1, ch2) -> ch1 : ch2 : helpDecode theCodes C xs
          Right _ -> "INVALID C VAL"
      Nothing -> "INVALID C VAL"

-- helpDecode C (x:xs) = ...

--------------------------------------------------------------------------------
-- 1. General Decoding

decode :: TheCodes -> BC -> Either Error String
decode theCodes (start_, data_, checksum_, stop_)
  | start_ /= startB theCodes && start_ /= startC theCodes = Left "invalid start code"
  | stop_ /= stop theCodes = Left "invalid stop code"
  | checksum_ /= computeChecksum start_ data_ = Left "invalid checksum"
  | start_ == startB theCodes = Right (helpDecode theCodes B data_)
  | start_ == startC theCodes = Right (helpDecode theCodes C data_)

--------------------------------------------------------------------------------
-- 2. Optimal Encodings

-- TODO: This is a placeholder definition, to be replaced.
encode :: TheCodes -> String -> Either Error BC
encode theCodes str
  | not $ all isPrintable str = Left "encode: unsupported characters"
  | all isDigit str = encodeC theCodes str
  | otherwise = encodeB theCodes str

--------------------------------------------------------------------------------
-- 3. Making Barcodes

makeBarcode :: FilePath -> Int -> Int -> BCString -> IO ()
makeBarcode filePath imageHeight moduleWidth (BCString symbols) = do
  let mods = concat symbols
  let imageLength = length mods * moduleWidth

  let barcode_img =
        makeImageR
          VU
          (imageHeight, imageLength)
          ( \(i, j) ->
              ( if mods !! (fromIntegral j `div` moduleWidth)
                  then PixelRGB (0 :: Double) 0 0
                  else PixelRGB (255 :: Double) 255 255
              )
          )

  writeImage filePath barcode_img

--------------------------------------------------------------------------------
-- 4. Scanning Barcodes

-- helper from a Stack post:
groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n l
  | n > 0 = take n l : groupBy n (drop n l)
  | otherwise = error "Negative or zero n"

scanBarcode :: FilePath -> IO BCString
scanBarcode filePath = do
  let img = Image.readImageRGB VU filePath
  return imageToBCString img

-- takeEvery moduleWidth [pixels] --> return a list of pixels from each module
-- check if each pixel isBlack
-- create a [Bool]
-- use a groupby function to group the [Bools] by 11 so that we have [[Bool]]
-- get list of bools and init twice to remove last two
-- then group the list of bools by 11

--
-- undefined

imageToBCString :: (MArray arr cs e, ToY cs e) => Image arr cs e -> BCString
imageToBCString img = do
  -- extract the top row of pixels from image
  let pixelList = head (Image.toLists img)
  -- list of bools to see if each pixel is black
  let boolList = map isBlack pixelList

  -- flip the bool list to find the width of modules in it
  let flippedBoolList = reverse boolList
  let modWidth = getModuleWidth flippedBoolList

  -- extract a pixel from each module
  let pixFromEachMod = takeEvery modWidth pixelList

  -- check if each pixel isBlack & create [Bool]
  -- remove the last 2 pixels because they're associated with the last '11' bits
  let pixelBools = init (init (map isBlack pixFromEachMod))

  -- group the pixelBools by 11 so that we have [[Bool]]
  let symbols = groupBy 11 pixelBools

  -- return the BCString
  BCString symbols

--------------------------------------------------------------------------------
-- 5. Scanning Designed Barcodes

--------------------------------------------------------------------------------
-- Main

isBlack :: (ToY cs e) => Pixel cs e -> Bool
isBlack pixel =
  let PixelY lum = toPixelY pixel
   in -- so checking if pixel is black with some noise
      lum <= 5.0

getModuleWidth :: [Bool] -> Int
getModuleWidth bools =
  let endCode = takeWhile (== False) bools
   in length endCode `div` 2

takeEvery :: Int -> [a] -> [a]
takeEvery n xs =
  let filteredTuples = filter (\(x, _) -> x `mod` n == 0) (zip [1 :: Int ..] xs)
   in map snd filteredTuples

scanDesignedBarcode :: FilePath -> IO BCString
scanDesignedBarcode filePath =
  undefined

runEncoder ::
  (TheCodes -> String -> Either Error BC) ->
  FilePath ->
  Int ->
  Int ->
  String ->
  IO ()
runEncoder f filePath height moduleWidth str = do
  theCodes <- loadTheCodes
  let result = bcToBCString theCodes <$> f theCodes str
  either (const (die "encoder failed")) printEncoding result
  where
    printEncoding bcString = do
      putStrLn $ "\nPayload:\n" ++ str
      putStrLn $ "\nEncoding:\n" ++ show bcString
      makeBarcode filePath height moduleWidth bcString

runDecoder ::
  (TheCodes -> BC -> Either Error String) ->
  String ->
  IO ()
runDecoder f filePath = do
  theCodes <- loadTheCodes
  bcString <- scanBarcode filePath
  let bc = bcStringToBC theCodes bcString
  either (const (die "decoder failed")) printDecoding (f theCodes bc)
  where
    printDecoding str = do
      putStrLn $ "\nDecoding:\n" ++ str
      putStrLn ""

main :: IO ()
main =
  getArgs >>= processArgs
  where
    processArgs ["encode", filePath, imageHeight, moduleWidth, str] =
      HipBarcodes.runEncoder
        encode
        filePath
        (read imageHeight)
        (read moduleWidth)
        str
    processArgs ["decode", file] =
      HipBarcodes.runDecoder decode file
    processArgs _ =
      die $
        "\nUsage:\n\n"
          ++ "  cabal run hip-barcodes -- encode imageFilePath imageHeight moduleWidth string\n"
          ++ "  cabal run hip-barcodes -- decode imageFilePath\n"

module HipBarcodes where

-- You may add, remove, or edit imports as you wish. Note the use of
-- qualified imports to avoid collisions between common names. For
-- example, Prelude and Data.Map and Graphics.Image all define `map`.

import           Code128
import           Data.Char
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Graphics.Image (Image, Pixel(..), RGB, VU(VU))
import           Graphics.Image.ColorSpace
import           Graphics.Image.Interface (MArray)
import qualified Graphics.Image as Image
import           System.Environment
import           System.Exit


--------------------------------------------------------------------------------
-- NOTE: Defining these at top-level rather than changing Code128.TheCodes
-- to include them, like the start and stop symbols.

bToC, cToB :: SymbolId
bToC = 99
cToB = 100  


--------------------------------------------------------------------------------
-- 1. General Decoding

decode :: TheCodes -> BC -> Either Error String
decode theCodes (start_, data_, checksum_, stop_) =
  undefined


--------------------------------------------------------------------------------
-- 2. Optimal Encodings

-- TODO: This is a placeholder definition, to be replaced.
encode :: TheCodes -> String -> Either Error BC
encode theCodes str
  | not $ all isPrintable str = Left "encode: unsupported characters"
  | all isDigit str           = encodeC theCodes str
  | otherwise                 = encodeB theCodes str


--------------------------------------------------------------------------------
-- 3. Making Barcodes

makeBarcode :: FilePath -> Int -> Int -> BCString -> IO ()
makeBarcode filePath imageHeight moduleWidth (BCString symbols) =
  undefined


--------------------------------------------------------------------------------
-- 4. Scanning Barcodes

scanBarcode :: FilePath -> IO BCString
scanBarcode filePath =
  undefined

imageToBCString :: (MArray arr cs e, ToY cs e) => Image arr cs e -> BCString
imageToBCString img =
  undefined

isBlack :: (ToY cs e) => Pixel cs e -> Bool
isBlack pixel =
  undefined

getModuleWidth :: [Bool] -> Int
getModuleWidth bools =
  undefined

takeEvery :: Int -> [a] -> [a]
takeEvery n xs =
  undefined


--------------------------------------------------------------------------------
-- 5. Scanning Designed Barcodes

scanDesignedBarcode :: FilePath -> IO BCString
scanDesignedBarcode filePath =
  undefined


--------------------------------------------------------------------------------
-- Main

runEncoder
  :: (TheCodes -> String -> Either Error BC) -> FilePath -> Int -> Int -> String
  -> IO ()
runEncoder f filePath height moduleWidth str = do
  theCodes <- loadTheCodes
  let result = bcToBCString theCodes <$> f theCodes str
  either (const (die "encoder failed")) printEncoding result
    where
      printEncoding bcString = do
        putStrLn $ "\nPayload:\n" ++ str
        putStrLn $ "\nEncoding:\n" ++ show bcString
        makeBarcode filePath height moduleWidth bcString

runDecoder
  :: (TheCodes -> BC -> Either Error String) -> String
  -> IO ()
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
        encode filePath (read imageHeight) (read moduleWidth) str
    processArgs ["decode", file] =
      HipBarcodes.runDecoder decode file
    processArgs _ =
      die $ "\nUsage:\n\n"
        ++ "  cabal run hip-barcodes -- encode imageFilePath imageHeight moduleWidth string\n"
        ++ "  cabal run hip-barcodes -- decode imageFilePath\n"

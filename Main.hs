{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Codec.Picture
import Codec.Picture.Types
import Control.Lens
import Control.Monad.ST
import Data.List
import Data.List.Split
import Data.Random hiding (sample)
import Data.Random.Extras
import Options.Applicative
import System.FilePath.Lens


main :: IO ()
main = do
  cli@CLI {..} <- either (error . show) id . validateCLI <$> execParser cliParser
  src1 <- either (error "Couldn't read first image") convertRGBA8 <$> readImage cImgPath1
  src2 <- either (error "Couldn't read second image") convertRGBA8 <$> readImage cImgPath2
  let canvas = makeCroppedCanvas src1 src2
  swapOpts <- makeSwapOpts cli canvas
  let output = makeImage swapOpts src1 src2 canvas
  writePng (makeFileName cli) output
  where
    makeFileName CLI {..} =
      let [name1, _] = case splitOn "." $ cImgPath1 ^. filename of
            (n:x:_) -> [n, x]
            _       -> error $ "Invalid filename/extension: " <> cImgPath1
          [name2, _] = case splitOn "." $ cImgPath2 ^. filename of
            (n:x:_) -> [n, x]
            _       -> error $ "Invalid filename/extension: " <> cImgPath2
      in name1 <> "-" <> name2 <> ".png"

    validateCLI cli@(CLI _ _ _ cProb1 cProb2) =
      if cProb1 < 1 || cProb1 > 99 || cProb2 < 1 || cProb2 > 99 || cProb1 + cProb2 /= 100
      then Left "Options should be integers in the range [1, 99], and should add up to 100."
      else Right cli

    cliParser = info (helper <*> parseCLI) (header "img-swap")

    parseCLI = CLI
      <$> strOption (long "first-path" <> help "Path the the first image.")
      <*> strOption (long "second-path" <> help "Path the the second image.")
      <*> switch (short 'c' <> help "Switch columns instead of rows.")
      <*> option auto (long "first-prob" <> help "Integer in the range 1-99 representing the probability of choosing a row / col from the first image.")
      <*> option auto (long "second-prob" <> help "Integer in the range 1-99 representing the probability of choosing a row / col from the second image.")


-- | CLI.
data CLI = CLI
  { cImgPath1 :: FilePath -- ^ Path to the first image.
  , cImgPath2 :: FilePath -- ^ Path to the second image.
  , cCols     :: Bool -- ^ Swap cols instead of rows.
  , cProb1    :: Int -- ^ Probability of choosing a row  / col from the first image.
  , cProb2    :: Int -- ^ Probability of choosing a row / col from the second image.
  } deriving (Eq, Show)

-- | Rows or cols
data Direction = Row | Col deriving (Eq, Show)

-- | Configuration for image generation.
data SwapOpts = SwapOpts
  { soImg1 :: [Int] -- ^ List of indicies to take from the first image.
  , soImg2 :: [Int] -- ^ List of indicies to take from the second image.
  , soDir  :: Direction -- ^ Rows or cols
  } deriving (Eq, Show)


-- | Make a canvas cropped to the minimum of the dimensions of the input images.
makeCroppedCanvas
  :: Image PixelRGBA8 -- ^ First image.
  -> Image PixelRGBA8 -- ^ Second image.
  -> Image PixelRGBA8
makeCroppedCanvas src1 src2 = runST $ do
  mimg <- newMutableImage w h
  unsafeFreezeImage mimg
  where w = min (imageWidth src1) (imageWidth src2)
        h = min (imageHeight src1) (imageHeight src2)


-- | Create the configuration for image generation from CLI options.
makeSwapOpts
  :: CLI -- ^ User-provided options.
  -> Image PixelRGBA8 -- ^ Canvas.
  -> IO SwapOpts
makeSwapOpts CLI {..} Image {..} = do
  let (dim, soDir) = case cCols of
        False -> (imageHeight, Row)
        True  -> (imageWidth, Col)
  let n' = floor $ fromIntegral (dim * cProb1) / 100
      n = case n' `mod` 2 of
        0 -> n'
        1 -> n' + 1
        _ -> error "Mathematics is broken."
  soImg1 <- runRVar (sample n [0..dim - 1]) StdRandom :: IO [Int]
  let soImg2 = [0..dim - 1] \\ soImg1

  return SwapOpts {..}


-- | Make the final output image.
makeImage
  :: SwapOpts -- ^ Randomness options.
  -> Image PixelRGBA8 -- ^ First image.
  -> Image PixelRGBA8 -- ^ Second image.
  -> Image PixelRGBA8 -- ^ Canvas.
  -> Image PixelRGBA8
makeImage SwapOpts {..} src1 src2 canvas =
  case soDir of
    Row -> writeRows soImg2 src2 $ writeRows soImg1 src1 canvas
    Col -> writeCols soImg2 src2 $ writeCols soImg1 src1 canvas


-- | Write a list of rows from @src@ into @canvas@.
writeRows
  :: [Int] -- ^ List of rows to write.
  -> Image PixelRGBA8 -- ^ Image to write rows from.
  -> Image PixelRGBA8 -- ^ Canvas.
  -> Image PixelRGBA8
writeRows [] _ acc           = acc
writeRows (rix:rixs) src acc = writeRows rixs src (writeRow rix src acc)


-- | Write one row from the source image.
writeRow
  :: Int -- ^ The row index.
  -> Image PixelRGBA8 -- ^ Image to write row from.
  -> Image PixelRGBA8 -- ^ Canvas.
  -> Image PixelRGBA8
writeRow rix src canvas = runST $ do
  mimg <- unsafeThawImage canvas
  go 0 mimg
  where go !c !mimg
          | c >= imageWidth canvas = unsafeFreezeImage mimg
          | otherwise = do
              writePixel mimg c rix (pixelAt src c rix)
              go (c + 1) mimg

-- | Write a list of cols from @src@ into @canvas@.
writeCols
  :: [Int] -- ^ List of rows to write.
  -> Image PixelRGBA8 -- ^ Image to write rows from.
  -> Image PixelRGBA8 -- ^ Canvas.
  -> Image PixelRGBA8
writeCols [] _ acc           = acc
writeCols (cix:cixs) src acc = writeCols cixs src (writeCol cix src acc)

-- | Write one col from the source image.
writeCol
  :: Int -- ^ The column index.
  -> Image PixelRGBA8 -- ^ Image to write row from.
  -> Image PixelRGBA8 -- ^ Canvas.
  -> Image PixelRGBA8
writeCol cix src canvas = runST $ do
  mimg <- unsafeThawImage canvas
  go 0 mimg
  where go !r !mimg
          | r >= imageHeight canvas = unsafeFreezeImage mimg
          | otherwise = do
              writePixel mimg cix r (pixelAt src cix r)
              go (r + 1) mimg

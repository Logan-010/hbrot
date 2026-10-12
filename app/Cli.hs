module Cli (Cli (..), parse, help) where

import Colormaps (Colormap (..))
import Data.Char (toLower)
import Text.Read (readMaybe)

data Cli = Cli
  { dimensions :: (Int, Int),
    imageCenter :: Maybe (Double, Double),
    viewSize :: Maybe (Double, Double),
    stepLimits :: (Int, Int),
    colormap :: Maybe Colormap,
    rngSeed :: Maybe Int,
    threads :: Maybe Int,
    bailoutNum :: Double,
    fileName :: String,
    noInfo :: Bool
  }
  deriving (Show, Eq)

defaultDimensions :: (Int, Int)
defaultDimensions = (1920, 1080)

defaultStepLimits :: (Int, Int)
defaultStepLimits = (150, 1024)

defaultBailoutNum :: Double
defaultBailoutNum = 15.0

help :: String
help =
  unlines
    [ "Usage: hbrot [OPTIONS]",
      "",
      "Options:",
      "  -d, --dimensions WIDTH HEIGHT   Output image dimensions (default: 1920 1080)",
      "  -i, --image-center X Y          Coordinates for the image center",
      "  -v, --view-size WIDTH HEIGHT    View size for the image",
      "  -s, --step-limits MIN MAX       Generation step limits (default: 150 1024)",
      "  -c, --colormap COLORMAP         Colormap to use",
      "  -r, --rng-seed SEED             Random number generator seed",
      "  -t, --threads COUNT             Number of threads to use",
      "  -b, --bailout-num NUMBER        Bailout exponent, 10^NUMBER (default: 15.0)",
      "  -f, --file-name FILE            JPEG output file (default: out.jpg)",
      "  -n, --no-info                   Suppress informational output",
      "  -h, --help                      Show this help message",
      "",
      "Short flags can be combined: -nr 12345",
      "Options taking values consume the following arguments."
    ]

parse :: [String] -> Either String Cli
parse = go initial
  where
    initial =
      Cli
        { dimensions = defaultDimensions,
          imageCenter = Nothing,
          viewSize = Nothing,
          stepLimits = defaultStepLimits,
          colormap = Nothing,
          rngSeed = Nothing,
          threads = Nothing,
          bailoutNum = defaultBailoutNum,
          fileName = "out.jpg",
          noInfo = False
        }

    go cli []
      | fst (dimensions cli) <= 0 || snd (dimensions cli) <= 0 = Left "Image dimensions must be positive"
      | maybe False (<= 0) (threads cli) = Left "Thread count must be positive"
      | fst (stepLimits cli) < 1 || fst (stepLimits cli) >= snd (stepLimits cli) = Left "Step limits must satisfy 1 <= MIN < MAX"
      | otherwise = Right cli
    go _ ("--help" : _) = Left help
    go _ ("-h" : _) = Left help
    go cli ["--"] = go cli []
    go _ ("--" : xs) =
      Left ("Unexpected positional arguments: " ++ unwords xs ++ "\n\n" ++ help)
    go cli (arg : xs)
      | take 2 arg == "--" = longOption cli arg xs
      | length arg > 1 && take 1 arg == "-" = shortOptions cli (drop 1 arg) xs
      | otherwise = Left ("Unexpected argument: " ++ arg ++ "\n\n" ++ help)

    longOption cli arg xs =
      let (name, inline) = splitLong (drop 2 arg)
       in case name of
            "dimensions" -> pairInt cli inline xs name (\v c -> c {dimensions = v})
            "image-center" -> pairDouble cli inline xs name (\v c -> c {imageCenter = Just v})
            "view-size" -> pairDouble cli inline xs name (\v c -> c {viewSize = Just v})
            "step-limits" -> pairInt cli inline xs name (\v c -> c {stepLimits = v})
            "colormap" -> value cli inline xs name parseColormap (\v c -> c {colormap = Just v})
            "rng-seed" -> value cli inline xs name readInt (\v c -> c {rngSeed = Just v})
            "threads" -> value cli inline xs name readInt (\v c -> c {threads = Just v})
            "bailout-num" -> value cli inline xs name readDouble (\v c -> c {bailoutNum = v})
            "file-name" -> value cli inline xs name Right (\v c -> c {fileName = v})
            "no-info" ->
              case inline of
                Nothing -> go (cli {noInfo = True}) xs
                Just _ -> Left "--no-info does not accept a value"
            _ -> Left ("Unknown option: --" ++ name ++ "\n\n" ++ help)

    splitLong s =
      case break (== '=') s of
        (name, []) -> (name, Nothing)
        (name, _ : v) -> (name, Just v)

    shortOptions cli [] xs = go cli xs
    shortOptions cli (flag : rest) xs =
      case flag of
        'n' -> shortOptions (cli {noInfo = True}) rest xs
        'd' -> shortPairInt cli rest xs "dimensions" (\v c -> c {dimensions = v})
        'i' -> shortPairDouble cli rest xs "image-center" (\v c -> c {imageCenter = Just v})
        'v' -> shortPairDouble cli rest xs "view-size" (\v c -> c {viewSize = Just v})
        's' -> shortPairInt cli rest xs "step-limits" (\v c -> c {stepLimits = v})
        'c' -> shortValue cli rest xs "colormap" parseColormap (\v c -> c {colormap = Just v})
        'r' -> shortValue cli rest xs "rng-seed" readInt (\v c -> c {rngSeed = Just v})
        't' -> shortValue cli rest xs "threads" readInt (\v c -> c {threads = Just v})
        'b' -> shortValue cli rest xs "bailout-num" readDouble (\v c -> c {bailoutNum = v})
        'f' -> shortValue cli rest xs "file-name" Right (\v c -> c {fileName = v})
        _ -> Left ("Unknown option: -" ++ [flag] ++ "\n\n" ++ help)

    shortPairInt cli rest xs name update =
      case rest of
        [] -> pairInt cli Nothing xs name update
        _ -> pairInt cli (Just rest) xs name update

    shortPairDouble cli rest xs name update =
      case rest of
        [] -> pairDouble cli Nothing xs name update
        _ -> pairDouble cli (Just rest) xs name update

    shortValue cli rest =
      value
        cli
        (if null rest then Nothing else Just rest)

    pairInt cli inline xs name update = do
      (a, b, remaining) <- getPair inline xs name
      x <- parseAs name readInt a
      y <- parseAs name readInt b
      go (update (x, y) cli) remaining

    pairDouble cli inline xs name update = do
      (a, b, remaining) <- getPair inline xs name
      x <- parseAs name readDouble a
      y <- parseAs name readDouble b
      go (update (x, y) cli) remaining

    getPair inline xs name =
      case inline of
        Just values ->
          case words (map (\c -> if c == ',' then ' ' else c) values) of
            [a, b] -> Right (a, b, xs)
            [a] ->
              case xs of
                b : remaining -> Right (a, b, remaining)
                [] -> Left ("Missing second value for --" ++ name)
            _ -> Left ("Expected two values for --" ++ name)
        Nothing ->
          case xs of
            a : b : remaining -> Right (a, b, remaining)
            _ -> Left ("Expected two values for --" ++ name)

    value ::
      Cli ->
      Maybe String ->
      [String] ->
      String ->
      (String -> Either String a) ->
      (a -> Cli -> Cli) ->
      Either String Cli
    value cli inline xs name reader update = do
      (v, remaining) <-
        case inline of
          Just s -> Right (s, xs)
          Nothing ->
            case xs of
              s : rest -> Right (s, rest)
              [] -> Left ("Missing value for --" ++ name)
      parsed <- parseAs name reader v
      go (update parsed cli) remaining

    parseAs name reader s =
      case reader s of
        Left err -> Left ("Invalid value for --" ++ name ++ ": " ++ err)
        Right v -> Right v

    readInt s =
      maybe (Left ("expected an integer, got " ++ show s)) Right (readMaybe s)

    readDouble s =
      maybe (Left ("expected a number, got " ++ show s)) Right (readMaybe s)

    parseColormap s =
      case lookup (map toLower s) colormapNames of
        Just v -> Right v
        Nothing -> Left ("unknown colormap " ++ show s)

    colormapNames =
      [ ("acton", Acton),
        ("bamako", Bamako),
        ("batlow", Batlow),
        ("berlin", Berlin),
        ("bilbao", Bilbao),
        ("broc", Broc),
        ("broco", Broco),
        ("buda", Buda),
        ("cork", Cork),
        ("corko", Corko),
        ("davos", Davos),
        ("devon", Devon),
        ("grayc", Grayc),
        ("hawaii", Hawaii),
        ("imola", Imola),
        ("lajolla", Lajolla),
        ("lapaz", Lapaz),
        ("lisbon", Lisbon),
        ("nuuk", Nuuk),
        ("oleron", Oleron)
      ]

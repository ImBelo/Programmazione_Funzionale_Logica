module Main where
data Mese = Febbraio | Marzo | Aprile
          deriving (Eq, Enum, Bounded, Show, Read)
type Data = (Int, Mese)
m_greg = 24
n_greg = 5
calcolo_pasqua :: Int -> Data
calcolo_pasqua anno = (giorno,mese) 
  where giorno | z < 10 = z + 22
          | otherwise = z - 9 
        mese | z < 10 = Marzo
          | otherwise = Aprile
        z = d + e 
        d = (19 * a + m_greg) `mod` 30
        e = (2*b+4*c + 6 * d + n_greg) `mod` 7
        a = anno `mod` 19
        b = anno `mod` 4
        c = anno `mod` 7
controlla_bisestile :: Int->Bool
controlla_bisestile anno = (anno `mod` 4 == 0 && anno `mod` 100 /= 0) || (anno `mod` 400 == 0)
calcola_martedi_giovedi_grasso :: Bool -> Data -> Data 
calcola_martedi_giovedi_grasso False pasqua = (giorno,mese)
                                               where 
                                                 giorno = fst pasqua - 47
printAsciiDate :: Data -> IO ()
printAsciiDate giorno_mese = do
    putStrLn $ "(" ++ show ( fst giorno_mese) ++ "," ++ show (snd giorno_mese) ++ ") in ASCII art:"
    mapM_ putStrLn (renderDate giorno_mese)
-- Renders the complete date as ASCII art
renderDate :: Data -> [String]
renderDate giorno_mese =
    [ combineRows (dayRows !! i) (monthRows !! i) | i <- [0..4] ]
  where
    dayDigits = splitNumber $ fst giorno_mese
    monthDigits = splitNumber $ snd giorno_mese
    dayRows = combineDigits dayDigits
    monthRows = combineDigits monthDigits
    
    combineRows :: String -> String -> String
    combineRows dayRow monthRow = dayRow ++ "   " ++ monthRow

-- Splits a number into its individual digits
splitNumber :: Int -> [Int]
splitNumber n
    | n < 10 = [n]
    | otherwise = splitNumber (n `div` 10) ++ [n `mod` 10]

-- Combines multiple digits horizontally
combineDigits :: [Int] -> [String]
combineDigits digits = foldl1 combineAll (map digitToAscii digits)
  where
    combineAll acc next = [ combineRows a n | (a, n) <- zip acc next ]
    combineRows a n = a ++ " " ++ n

-- Converts a digit (0-9) to its 5x5 ASCII representation
digitToAscii :: Int -> [String]
digitToAscii n
    | n >= 0 && n <= 9 = digitsAscii !! n
    | otherwise = ["  ?  ", "  ?  ", "  ?  ", "  ?  ", "  ?  "]
-- Database di cifre ASCII 5x5
digitsAscii :: [[String]]
digitsAscii =
    [ -- 0
      [" *** ",
       "*   *",
       "*   *",
       "*   *",
       " *** "],
      -- 1
      ["  *  ",
       " **  ",
       "  *  ",
       "  *  ",
       " *** "],
      -- 2
      [" *** ",
       "    *",
       " *** ",
       "*    ",
       "*****"],
      -- 3
      [" *** ",
       "    *",
       " *** ",
       "    *",
       " *** "],
      -- 4
      ["*   *",
       "*   *",
       "*****",
       "    *",
       "    *"],
      -- 5
      ["*****",
       "*    ",
       "**** ",
       "    *",
       "**** "],
      -- 6
      [" *** ",
       "*    ",
       "**** ",
       "*   *",
       " *** "],
      -- 7
      ["*****",
       "    *",
       "   * ",
       "  *  ",
       " *   "],
      -- 8
      [" *** ",
       "*   *",
       " *** ",
       "*   *",
       " *** "],
      -- 9
      [" *** ",
       "*   *",
       " ****",
       "    *",
       " *** "]
    ]

main :: IO ()
main = do
  printAsciiDate(calcolo_pasqua 2003)
  printAsciiDate(calcolo_pasqua 2004)
  printAsciiDate(calcolo_pasqua 2005)
  printAsciiDate(calcolo_pasqua 2006)
  printAsciiDate(calcolo_pasqua 2007)

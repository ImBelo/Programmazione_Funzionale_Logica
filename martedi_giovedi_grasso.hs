module Main where
import qualified Data.Map as Map
import Data.Char (digitToInt)
data Mese = Febbraio | Marzo | Aprile
              deriving (Eq,Ord, Enum, Bounded, Show, Read)
data Calendario = Calendario
  { giorno :: Int, 
    mese :: Mese,
    anno :: Int
  } deriving (Show) 
crea_data :: Int->Mese->Int->Calendario
crea_data x Febbraio anno | x>0 && x<=(28 + fromEnum(controlla_bisestile anno)) = Calendario{giorno = x,mese = Febbraio,anno = anno}
                          | otherwise = error $ show x ++ " il giorno deve essere tra 1-" ++ show ( 28 + fromEnum(controlla_bisestile anno)) 
crea_data x Marzo anno    | x>0 && x<=31 = Calendario{giorno = x,mese = Marzo,anno = anno}
                          | otherwise = error $ show x ++ " il giorno deve essere tra 1-31"  
crea_data x Aprile anno   | x>0 && x<=30 = Calendario{giorno = x,mese = Aprile,anno = anno}
                          | otherwise = error $ show x ++ " il giorno deve essere tra 1-30"  

calcolo_pasqua :: Int -> Calendario
calcolo_pasqua anno = crea_data giorno mese anno
  where giorno | z < 10 = z + 22
               | otherwise = z - 9 
        mese | z < 10 = Marzo
             | otherwise = Aprile
        z = d + e 
        d = (19 * a + m_greg) `mod` 30
        e = (2*b+4*c + 6 * d + n_greg) `mod` 7
        m_greg = 24
        n_greg = 5
        a = anno `mod` 19
        b = anno `mod` 4
        c = anno `mod` 7
controlla_bisestile :: Int -> Bool
controlla_bisestile anno = (anno `mod` 4 == 0 && anno `mod` 100 /= 0) || (anno `mod` 400 == 0)

calcola_martedi_giovedi_grasso :: Bool -> Calendario -> Calendario
calcola_martedi_giovedi_grasso _ pasqua = pasqua

mostra_data :: Calendario -> String
mostra_data giorno_calendario = unlines combinedArt
      where
        digits = map digitToInt $ show $ giorno giorno_calendario
        artNum1 = prendiCifreAscii (digits !! 0)
        artNum2 = prendiCifreAscii (digits !! 1)
        space = [" "," "," "," "," "]
        artLetters1 = (mesiAsciiMap(mese giorno_calendario)) !! 0  
        artLetters2 = (mesiAsciiMap(mese giorno_calendario)) !! 1  
        artLetters3 = (mesiAsciiMap(mese giorno_calendario)) !! 2  
        combinedArt = foldl1 (zipWith (++)) [artNum1 , artNum2 ,space , space, artLetters1 , artLetters2 , artLetters3]
--
--
-- Converts a digit (0-9) to its 5x5 ASCII representation
prendiCifreAscii :: Int -> [String]
prendiCifreAscii n
    | n >= 0 && n <= 9 = cifreAscii !! n
    | otherwise = ["  ?  ", "  ?  ", "  ?  ", "  ?  ", "  ?  "]

mesiAsciiMap ::Mese -> [[String]]
mesiAsciiMap Febbraio =
     [["*****",
       "*    ",
       "*****",
       "*    ",
       "*    "],
      ["*****",
       "*    ",
       "*****",
       "*    ",
       "*****"],
      ["**** ",
       "*   *",
       "**** ",
       "*   *",
       "**** "]]
mesiAsciiMap Marzo =
     [["*   *",
       "* * *",
       "*   *",
       "*   *",
       "*   *"],
      [" *** ",
       "*   *",
       "*****",
       "*   *",
       "*   *"],
      ["**** ",
       "*   *",
       "**** ",
       "*  * ",
       "*   *"]]
mesiAsciiMap Aprile =
     [[" *** ",
       "*   *",
       "*****",
       "*   *",
       "*   *"],
      ["**** ",
       "*   *",
       "**** ",
       "*    ",
       "*    "],
      ["****",
       "*   *",
       "**** ",
       "*  * ",
       "*   *"]]
     
-- Database di cifre ASCII 5x5
cifreAscii :: [[String]]
cifreAscii =
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
  putStrLn$ mostra_data(calcolo_pasqua 2003)
  putStrLn$ mostra_data(calcolo_pasqua 2004)
  putStrLn$ mostra_data(calcolo_pasqua 2005)
  putStrLn$ mostra_data(calcolo_pasqua 2006)

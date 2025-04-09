module Main where
import qualified Data.Map as Map
import Data.Char (digitToInt)
data Mese = Febbraio | Marzo | Aprile
              deriving (Eq,Ord, Enum, Show, Read)
data Calendario = Calendario
  { giorno :: Int, 
    mese :: Mese,
    anno :: Int
  }
instance Show Calendario where
  show cal = formatta_due_cifre(giorno cal) ++ " " ++ show (mese cal) ++ " " ++ show (anno cal)
giorniDelMese :: Mese -> Int -> Int
giorniDelMese Febbraio anno
  | controlla_bisestile anno = 29
  | otherwise        = 28
giorniDelMese mese _
  | mese == Aprile = 30
  | mese == Marzo = 31
formatta_due_cifre :: Int ->String
formatta_due_cifre n | n < 10 = "0" ++ show n 
                     | n > 99 || n < 0 = error $ show (n) ++ "il numero deve essere tra 0 e 99" 
                     | otherwise = show n
crea_data :: Int->Mese->Int->Calendario
crea_data x Febbraio anno | x<=(28 + fromEnum(controlla_bisestile anno)) = Calendario{giorno = x,mese = Febbraio,anno = anno}
                          | otherwise = error $ show x ++ " il giorno deve essere tra 1-" ++ (show $ giorniDelMese Febbraio anno) 
crea_data x Marzo anno    | x<=31 = Calendario{giorno = x,mese = Marzo,anno = anno}
                          | otherwise = error $ show x ++ " il giorno deve essere tra 1-" ++ (show $ giorniDelMese Marzo anno)  
crea_data x Aprile anno   | x<=30 = Calendario{giorno = x,mese = Aprile,anno = anno}
                          | otherwise = error $ show x ++ " il giorno deve essere tra 1-" ++ (show $ giorniDelMese Aprile anno)

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
        (giorno', mese') = case (d, e) of 
        -- 1. Se d = 29 ed e = 6 -> Pasqua il 19 aprile
          (29, 6) -> (19, Aprile)
        
        -- 2. Se d = 28 ed e = 6 e a > 10 -> Pasqua il 18 aprile
          (28, 6) | a > 10 -> (18, Aprile)
        
          _ -> (giorno, mese)
controlla_bisestile :: Int -> Bool
controlla_bisestile anno = (anno `mod` 4 == 0 && anno `mod` 100 /= 0) || (anno `mod` 400 == 0)
{-! DA FARE !-}
calcola_martedi_giovedi_grasso :: Bool -> Calendario -> Calendario
calcola_martedi_giovedi_grasso turno_calcolo pasqua 
        -- calcolo il martedi grasso o il giovedi grasso
  | turno_calcolo == False = calcola_giorno_mese 47 pasqua
  | otherwise = calcola_giorno_mese 52 pasqua

calcola_giorno_mese :: Int -> Calendario -> Calendario
calcola_giorno_mese giorni_scalare pasqua 
  | (giorno pasqua) - giorni_scalare <= 0 = calcola_giorno_mese 0 calendario
  | otherwise = Calendario{giorno = giorno pasqua, mese = mese pasqua,anno = anno pasqua}
    where
      calendario = Calendario{giorno = giorni_rimasti, mese = mese_precedente,anno = anno_corrente} 
      giorni_rimasti = (giorno pasqua - giorni_scalare) + (giorniDelMese mese_precedente anno_corrente)
      mese_precedente = pred (mese pasqua) 
      anno_corrente = anno pasqua
mostra_data :: Calendario -> String
mostra_data giorno_calendario = unlines data_combinata
      where
        -- lista di lista contenente le cifre del giorno 
        cifre_ascii :: [[String]]
        cifre_ascii = map (prendiCifreAscii . digitToInt) (formatta_due_cifre $ giorno giorno_calendario)
        -- lista di lista contentente le 3 lettere del mese
        lettere_ascii :: [[String]]
        lettere_ascii = (mesiAsciiMap (mese giorno_calendario))
        spazio = foldl1 (zipWith(++)) (replicate 5 $ replicate 5 " ")
        -- Combina le cifre affiancate (es: ["1"] ++ ["2"] → ["12"])
        cifre_combinate :: [String]
        cifre_combinate = foldl1 (zipWith (++)) cifre_ascii  
        lettere_combinate :: [String]
        -- Combina le lettere affiancate
        lettere_combinate = foldl1 (zipWith (++)) lettere_ascii 
        -- Combina cifre spazio e lettere insieme 
        data_combinata = zipWith3 (\cifre spazio lettere -> cifre ++ spazio ++ lettere) cifre_combinate spazio lettere_combinate
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
      ["**** ",
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
  putStrLn $ mostra_data $ calcolo_pasqua 2003
  putStrLn ( mostra_data (calcola_martedi_giovedi_grasso False (calcolo_pasqua 2003)))
  putStrLn (mostra_data (calcola_martedi_giovedi_grasso True (calcolo_pasqua 2003)))

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
calcola_martedi_giovedi_grasso turno_calcolo pasqua 
        -- calcolo il martedi grasso o il giovedi grasso
       | turno_calcolo == 1 = calcola_giorno_mese 47 pasqua
       | otherwise = calcola_giorno_mese 52 pasqua

calcola_giorno_mese :: Int -> Calendario -> Calendario
calcola_giorno_mese giorni_scalare pasqua 
        | (mese pasqua) == Aprile = calcola_aprile (giorno pasqua) - giorni_scalare + 31 (anno pasqua)
                                    where 
                                      calcola_aprile risultato_calcolo anno_pasqua = 
                                      anno_bisestile = controlla_bisestile anno_pasqua 
                                      | numero < 0 && anno_bisestile == True = Calendario{giorno = risultato_calcolo + 29, mese = Febbraio, anno = anno_pasqua}
                                      | numero < 0 && anno_bisestile == False = Calendario{giorno = risultato_calcolo + 28, mese =  Febbraio, anno = anno_pasqua}
                                      | otherwise = Calendario{giorno = risultato_calcolo, mese = Marzo, anno = anno_pasqua}
                                      
        | (mese pasqua) == Marzo = calcola_marzo (giorno pasqua) - giorni_scalare (anno pasqua) 
                                    where
                                        calcola_marzo risultato_calcolo anno_pasqua = 
                                          anno_bisestile = controlla_bisestile anno_pasqua
                                          | anno_bisestile == True = Calendario{giorno = risultato_calcolo + 29, mese = Febbraio, anno = anno_pasqua}
                                          | otherwise = Calendario{giorno = risultato_calcolo + 28, mese = Febbraio, anno = anno_pasqua}
        

mostra_data :: Calendario -> String
mostra_data giorno_calendario = unlines data_combinata
      where
        -- lista di lista contenente le cifre del giorno 
        cifre_ascii :: [[String]]
        cifre_ascii = map (prendiCifreAscii . digitToInt) (show $ giorno giorno_calendario)
        -- lista di lista contentente le 3 lettere del mese
        lettere_ascii :: [[String]]
        lettere_ascii = (mesiAsciiMap (mese giorno_calendario))
        spazio = replicate 5 " "    
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
  putStrLn$ mostra_data(calcolo_pasqua 2007)
  putStrLn$ mostra_data(calcolo_pasqua 2008)
  putStrLn$ mostra_data(calcolo_pasqua 2009)
  putStrLn$ mostra_data(calcolo_pasqua 2010)

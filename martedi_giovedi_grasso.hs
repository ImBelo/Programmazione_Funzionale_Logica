{-
 - Progetto di Programmazione Logica e Funzionale
 - Studenti: Elia Renzoni e Gianmarco Beligni
 - N. Matricola: 319978 321069 
 - Sessione Estiva a.a 2024/2025
 -}


module Main where

-- inclusione delle librerie
import Text.Read (readMaybe)
import Data.Char (digitToInt)

type Giorno = Int
type Anno = Int
-- definizione dei tipi Mese, Giorno e Calendario
data Mese = Febbraio | Marzo | Aprile
              deriving (Eq,Ord, Enum, Bounded, Show, Read)
data GiornoGrasso = MartedìGrasso | GiovedìGrasso
              deriving (Eq,Ord,Enum,Show,Read)
data Calendario = Calendario
  { giorno :: Giorno, 
    mese :: Mese,
    anno :: Anno
  } deriving (Show) 
{- Funzione per formattare un numero tra 1-99 incluso per avere sempre due cifre:
 - l'argomento è un numero tra 1-99 incluso-}
formattaADueCifre :: Int -> String
formattaADueCifre n | n < 10 = "0" ++ show n 
                    | n > 99 || n < 0 = error $ show (n) ++ "il numero deve essere tra 0 e 99" 
                    | otherwise = show n
{- Funzione per la creazione del tipo Data: 
 - il primo argomento è il giorno
 - il secondo argomento è il Mese
 - il terzo argomento è l'anno-}
creaData :: Giorno->Mese->Anno->Calendario
creaData x mese anno | x>0 && x<=giorniDelMese mese anno = Calendario{giorno = x,mese = Febbraio,anno = anno}

{- Funzione che restituisce quanti giorno ci sono in un mese:
 - il primo argomento è il mese
 - il secondo argomento è l'anno (importante per Febbraio) -}
giorniDelMese :: Mese -> Anno -> Giorno
giorniDelMese Febbraio anno | controllaBisestile anno = 29
                            | otherwise = 28
giorniDelMese mese _ | mese == Aprile = 30
                     | mese == Marzo = 31                     

{- Funzione che restituisce la data di Pasqua:
 - il primo argomento è l'anno -}
calcoloPasqua :: Anno -> Calendario
calcoloPasqua anno 
    -- Caso speciale 25 aprile -> 18 aprile
    | giorno == 25 && mese == Aprile && d == 28 && e == 6 && a > 10 = creaData 18 Aprile anno  
    -- Caso speciale 26 aprile -> 19 aprile
    | giorno == 26 && mese == Aprile = creaData 19 Aprile anno                                  
    | otherwise = creaData giorno mese anno
  where giorno | z < 10 = z + 22
               | otherwise = z - 9 
        mese | z < 10 = Marzo
             | otherwise = Aprile
        z = d + e 
        d = (19 * a + mGreg) `mod` 30
        e = (2*b+4*c + 6 * d + nGreg) `mod` 7
        mGreg = 24
        nGreg = 5
        a = anno `mod` 19
        b = anno `mod` 4
        c = anno `mod` 7
{- Funzione che restituisce se l'anno è bisestile:
 - l'argomento è l'anno da verificare -}
controllaBisestile :: Anno -> Bool
controllaBisestile anno = (anno `mod` 4 == 0 && anno `mod` 100 /= 0) || (anno `mod` 400 == 0)

{- Funzione che calcola la data del Martedì o Giovedì grasso 
 - il primo argomento è il giorno grasso
 - il secondo argomento è la data di pasqua dell'anno in cui si vuole calcolare i giorni grassi -}
calcolaGiornoGrasso :: GiornoGrasso -> Calendario -> Calendario
calcolaGiornoGrasso giornoGrasso pasqua 
  | giornoGrasso == MartedìGrasso = sottraiGiorniDaData 47 pasqua
  | giornoGrasso == GiovedìGrasso = sottraiGiorniDaData 52 pasqua

{- Funzione che calcola la sottrazione di giorni ad una data:
 - il primo argomento è il numero di giorno da sottrarre
 - il secondo argomento è la data a cui sottrarre i giorni -}
sottraiGiorniDaData :: Giorno -> Calendario -> Calendario
sottraiGiorniDaData giorniScalare calendario
  -- caso base: i giorni da scalare sono meno del giorno del calendario  
  | (giorno calendario) - giorniScalare > 0 = creaData (giornoCorrente-giorniScalare) meseCorrente annoCorrente
  -- caso ricorsivo: aggiunge i giorni del mese precedente finché la sottrazione sia maggiore di 0
  | otherwise = sottraiGiorniDaData 0 dataAggiornata
    where
      dataAggiornata = Calendario{giorno = giorniRimasti, mese = mesePrecedente, anno = annoCorrente}
      giorniRimasti = (giornoCorrente - giorniScalare) + (giorniDelMese mesePrecedente annoCorrente)
      mesePrecedente = pred (mese calendario) 
      annoCorrente = (anno calendario)
      giornoCorrente = (giorno calendario)
      meseCorrente = (mese calendario)

{- Funzione che trasforma una data nella rappesentazione ASCII art:
 - l'argomento è una data -}
formattaDataAscii :: Calendario -> String
formattaDataAscii giornoCalendario = unlines dataCombinata
      where
        spazioTraCaratteri = [replicate 5 " "]
        -- lista di lista contenente le cifre del giorno se è ad una cifra aggiunge uno zero davanti
        cifreAsci :: [[String]]
        cifreAsci = map (cifraInAsciiArt . digitToInt) (formattaADueCifre $ giorno giornoCalendario)
        giornoAscii = take 1 cifreAsci ++ spazioTraCaratteri ++ drop 1 cifreAsci
        -- lista di lista contentente le 3 lettere del mese
        meseAscii :: [[String]]
        meseAscii = (meseInAsciiArt (mese giornoCalendario))
        spazioTraGiornoMese = replicate 5 $ replicate 5 " "
        -- Combina cifre spazio e lettere insieme 
        dataCombinata =  foldl1 (zipWith(++)) $ giornoAscii ++ spazioTraGiornoMese ++ meseAscii

acquisisciAnno :: IO Int 
acquisisciAnno = do 
                anno <- getLine 
                -- validazione dei dati acquisiti
                case readMaybe anno of
                  (Just anno) | controllaAnno anno -> return anno
                  _ -> do 
                     putStrLn "Input non valido. L'anno deve essere tra 1900 e 2099."
                     acquisisciAnno
{- Funzione per controllare che l'anno sia tra 1900-2099
 - l'argomento è l'anno -}
controllaAnno :: Anno -> Bool
controllaAnno anno = anno >= 1900 && anno <= 2099


{- Funzione che converte Mese nella sua rappresentazione ASCII art:
 - l'argomento è il mese da cui prendere l'ASCII art-}
meseInAsciiArt ::Mese -> [[String]]
meseInAsciiArt mese | mese == Febbraio = [["***** ",
                                           "*     ",
                                           "***** ",
                                           "*     ",
                                           "*     "],
                                          ["***** ",
                                           "*     ",
                                           "***** ",
                                           "*     ",
                                           "***** "],
                                          ["**** ",
                                           "*   *",
                                           "**** ",
                                           "*   *",
                                           "**** "]]
                    | mese == Marzo =    [["*   * ",
                                           "** ** ",
                                           "* * * ",
                                           "*   * ",
                                           "*   * "],
                                          ["***** ",
                                           "*   * ",
                                           "***** ",
                                           "*   * ",
                                           "*   * "],
                                          ["*****",
                                           "*   *",
                                           "*****",
                                           "*  * ",
                                           "*   *"]]
                    | mese == Aprile =   [["***** ",
                                           "*   * ",
                                           "***** ",
                                           "*   * ",
                                           "*   * "],
                                          ["***** ",
                                           "*   * ",
                                           "***** ",
                                           "*     ",
                                           "*     "],
                                          ["**** ",
                                           "*   *",
                                           "**** ",
                                           "*  * ",
                                           "*   *"]]
                    | otherwise =         [["  ?  "],
                                           ["  ?  "],
                                           ["  ?  "],
                                           ["  ?  "],
                                           ["  ?  "]]
   
{- Funzione che restituisce ASCII art di una cifra:
 - l'argomento è la cifra da cui prendere l'ASCII art -}
cifraInAsciiArt :: Int -> [String]
cifraInAsciiArt n | n == 0 = ["*****",
                              "*   *",
                              "*   *",
                              "*   *",
                              "*****"]
                  | n == 1 = ["  *  ",
                              " **  ",
                              "* *  ",
                              "  *  ",
                              "*****"]
                  | n == 2 = ["*****",
                              "    *",
                              "*****",
                              "*    ",
                              "*****"]
                  | n == 3 = ["*****",
                              "    *",
                              "*****",
                              "    *",
                              "*****"]
                  | n == 4 = ["*   *",
                              "*   *",
                              "*****",
                              "    *",
                              "    *"]
                  | n == 5 = ["*****",
                              "*    ",
                              "*****",
                              "    *",
                              "*****"]
                  | n == 6 = ["*****",
                              "*    ",
                              "*****",
                              "*   *",
                              "*****"]
                  | n == 7 = ["*****",
                              "    *",
                              "    *",
                              "    *",
                              "    *"]
                  | n == 8 = ["*****",
                              "*   *",
                              "*****",
                              "*   *",
                              "*****"]
                  | n == 9 = ["*****",
                              "*   *",
                              "*****",
                              "    *",
                              "*****"]
                  | otherwise = ["  ?  ", "  ?  ", "  ?  ", "  ?  ", "  ?  "]


main :: IO ()

-- definizione della funzione principale
main = do

  putStrLn("Programma per il calcolo di Giovedì e Martedì Grasso secondo il calendario Gregoriano")
  -- acquisizione dei due anni 
  putStrLn("Inserisci l'anno per il Martedì Grasso")
  primoAnno <- acquisisciAnno 
  putStrLn("inserisci l'anno per il Giovedì Grasso")
  secondoAnno <- acquisisciAnno 
  -- stampa della data del Martedì e Giovedì Grasso
  putStrLn (formattaDataAscii (calcolaGiornoGrasso MartedìGrasso (calcoloPasqua primoAnno)))
  putStrLn (formattaDataAscii (calcolaGiornoGrasso GiovedìGrasso (calcoloPasqua secondoAnno)))

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

-- definizione dei tipi Mese, Giorno e Calendario
data Mese = Febbraio | Marzo | Aprile
              deriving (Eq,Ord, Enum, Bounded, Show, Read)
data Giorno = Martedi | Giovedi
              deriving (Eq,Ord,Enum,Show,Read)
data Calendario = Calendario
  { giorno :: Int, 
    mese :: Mese,
    anno :: Int
  } deriving (Show) 

-- dichiarazione della funzione per formattare la data
crea_data :: Int->Mese->Int->Calendario
crea_data x Febbraio anno | x>0 && x<=(28 + fromEnum(controlla_bisestile anno)) = Calendario{giorno = x,mese = Febbraio,anno = anno}
                          | otherwise = error $ show x ++ " il giorno deve essere tra 1-" ++ show ( 28 + fromEnum(controlla_bisestile anno)) 
crea_data x Marzo anno    | x>0 && x<=31 = Calendario{giorno = x,mese = Marzo,anno = anno}
                          | otherwise = error $ show x ++ " il giorno deve essere tra 1-31"  
crea_data x Aprile anno   | x>0 && x<=30 = Calendario{giorno = x,mese = Aprile,anno = anno}
                          | otherwise = error $ show x ++ " il giorno deve essere tra 1-30"  

-- dichiarazione della funzione per la restituzione dei giorni
-- dei mesi
giorniDelMese :: Mese -> Int -> Int
giorniDelMese Febbraio anno
  | controlla_bisestile anno = 29
  | otherwise        = 28
giorniDelMese mese _
  | mese == Aprile = 30
  | mese == Marzo = 31                     

-- dichiarazione della funzione per il calcolo della pasqua
-- il paramentro in ingresso indica l'anno di riferimento
-- la funzione restituisce in uscita la data della pasqua
-- formattata secondo il tipo dato Calendario
calcolo_pasqua :: Int -> Calendario
calcolo_pasqua anno 
    -- Caso speciale 25 aprile -> 18 aprile
    | giorno == 25 && mese == Aprile && d == 28 && e == 6 && a > 10 = crea_data 18 Aprile anno  
    -- Caso speciale 26 aprile -> 19 aprile
    | giorno == 26 && mese == Aprile = crea_data 19 Aprile anno                                  
    | otherwise = crea_data giorno mese anno
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
-- dichiarazione della funzione per controllare la 
-- bisestilità di un determinato anno
-- la funzione prende in ingresso l'anno di riferimento
-- e restituisce in uscita un valore indicante se 
-- l'anno è bisestile o meno
controlla_bisestile :: Int -> Bool
controlla_bisestile anno = (anno `mod` 4 == 0 && anno `mod` 100 /= 0) || (anno `mod` 400 == 0)

-- dichiarazione della funzione per il calcolo del martedi e
-- giovedi grasso. 
-- Come valore in ingresso prende un tipo Bool indicante
-- il turno del calcolo, quindi se è turno del primo o del
-- secondo anno. Inoltre prende in ingresso anche la data della
-- pasqua formattata secondo il tipo dato Calendario. 
-- La funzione restituisce la data del  martedi o del giovedi
-- grasso formattato come tipo dato Calendario
calcola_martedi_giovedi_grasso :: Giorno -> Calendario -> Calendario
calcola_martedi_giovedi_grasso giorno pasqua 
  -- calcolo il martedi grasso o il giovedi grasso
  | giorno == Martedi = calcola_giorno_mese 47 pasqua
  | giorno == Giovedi = calcola_giorno_mese 52 pasqua

-- dichiarazione della funzione pe il calcolo effetivo
-- del martedi e del giovedi grasso.
-- In ingresso prende il giorno e la data della pasqua
-- formattata con il tipo dato Calendario.
-- In uscita restituisce la data del martedì o del giovedì grasso. 
-- formattata con il tipo dato Calendario
calcola_giorno_mese :: Int -> Calendario -> Calendario
calcola_giorno_mese giorni_scalare pasqua 
  | (giorno pasqua) - giorni_scalare <= 0 = calcola_giorno_mese 0 calendario
  | otherwise = Calendario{giorno = giorno pasqua, mese = mese pasqua,anno = anno pasqua}
    where
      calendario = Calendario{giorno = giorni_rimasti, mese = mese_precedente,anno = anno_corrente} 
      giorni_rimasti = (giorno pasqua - giorni_scalare) + (giorniDelMese mese_precedente anno_corrente)
      mese_precedente = pred (mese pasqua) 
      anno_corrente = anno pasqua

-- dichiarazione della funzione per la stampa 
-- a caratteri giganti del giorno e del mese in cui 
-- cade il martedi o il giovedi grasso
mostra_data :: Calendario -> String
mostra_data giorno_calendario = unlines data_combinata
      where
        spazioTraCaratteri = replicate 5 " "
        -- lista di lista contenente le cifre del giorno se è ad una cifra aggiunge uno zero davanti
        cifre_ascii :: [[String]]
        cifre_ascii | giorno giorno_calendario >= 10 = [prendiCifreAscii $ digitToInt (head (show $ giorno giorno_calendario)),                                                        spazioTraCaratteri,
                                                        prendiCifreAscii $ digitToInt (last (show $ giorno giorno_calendario))]
                    | otherwise = [prendiCifreAscii 0,
                                   spazioTraCaratteri,
                                   prendiCifreAscii (giorno giorno_calendario)]
        -- lista di lista contentente le 3 lettere del mese
        lettere_ascii :: [[String]]
        lettere_ascii = (mesiAsciiMap (mese giorno_calendario))
        -- Combina le cifre affiancate (es: ["1"] ++ ["2"] → ["12"])
        cifre_combinate :: [String]
        cifre_combinate = foldl1 (zipWith (++)) cifre_ascii  
        lettere_combinate :: [String]
        -- Combina le lettere affiancate
        lettere_combinate = foldl1 (zipWith (++)) lettere_ascii 
        spazioTraGiornoMese = foldl1 (zipWith(++)) $ replicate 5 $ replicate 5 " "
        -- Combina cifre spazio e lettere insieme 
        data_combinata = zipWith3 (\cifre spazio lettere -> cifre ++ spazio ++ lettere) cifre_combinate spazioTraGiornoMese lettere_combinate

-- dichiarazione della funzione per l'acquisizione
-- da tastiera dei dati d'ingresso del problema.
-- La funzione restituisce come risultato una tupla
-- contenente i due anni.
acquisisci_anni :: IO (Int, Int)
acquisisci_anni = do 
  putStrLn "Inserisci il primo anno di cui calcolare il martedi grasso >>"
  primo_anno <- getLine
  putStrLn "Inserisci il secondo anno di cui calcolare il giovedi grasso >>"
  secondo_anno <- getLine


  -- validazione dei dati acquisiti
  case (readMaybe primo_anno :: Maybe Int, readMaybe secondo_anno :: Maybe Int) of
	  -- se entrambi i dati acquisiti sono cifre
    (Just primo, Just secondo)  -> do 
	  -- controlla se sono cifre valide secondo la specifica del problema
      if not (controlla_acquisizione (read primo_anno :: Int)) || not (controlla_acquisizione (read secondo_anno :: Int))
        then do
          putStrLn "Input non validi, riprova!!"
          acquisisci_anni
        else return (read primo_anno, read secondo_anno)
	    -- se entrambi i dati acquisiti non sono cifre, ma lettere
    (Nothing, Nothing) -> do
      putStrLn "Gli anni devono essere dei numeri interi, riprova!"
      acquisisci_anni
	  -- se solo il secondo anno è costituito da lettere
    (Just primo, Nothing) -> do 
      putStrLn "Il secondo anno deve essere un intero, riprova!"
      acquisisci_anni
    -- se sono il primo anno è costuituito da lettere
    (Nothing, Just secondo) -> do
      putStrLn "il primo anno deve essere un intero, riprova!"
      acquisisci_anni


-- dichiarazione della funzione per effettuare il controllo
-- di validità degli anni acquisiti. 
-- Controlla se le date non siano più piccole del 1900 e più
-- grandi del 2099. 
-- Riceve in ingresso l'anno appena acquisito e restistuisce
-- l'esito della verifica
controlla_acquisizione :: Int -> Bool
controlla_acquisizione anno_acquisito
  | anno_acquisito < 1900 || anno_acquisito > 2099 = False
  | otherwise = True

-- Converte una cifra (0-9) nella rappresentazione ASCII
prendiCifreAscii :: Int -> [String]
prendiCifreAscii n
    | n >= 0 && n <= 9 = cifreAscii !! n
    | otherwise = ["  ?  ", "  ?  ", "  ?  ", "  ?  ", "  ?  "]

-- Converte mese nella rappresentazione ASCII
mesiAsciiMap ::Mese -> [[String]]
mesiAsciiMap Febbraio =
     [["***** ",
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
mesiAsciiMap Marzo =
     [["*   * ",
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
mesiAsciiMap Aprile =
     [["***** ",
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
     
-- Database di cifre ASCII 5x5
cifreAscii :: [[String]]
cifreAscii =
    [ -- 0
      ["*****",
       "*   *",
       "*   *",
       "*   *",
       "*****"],
      -- 1
      ["  *  ",
       " **  ",
       "* *  ",
       "  *  ",
       "*****"],
      -- 2
      ["*****",
       "    *",
       "*****",
       "*    ",
       "*****"],
      -- 3
      ["*****",
       "    *",
       "*****",
       "    *",
       "*****"],
      -- 4
      ["*   *",
       "*   *",
       "*****",
       "    *",
       "    *"],
      -- 5
      ["*****",
       "*    ",
       "*****",
       "    *",
       "*****"],
      -- 6
      ["*****",
       "*    ",
       "*****",
       "*   *",
       "*****"],
      -- 7
      ["*****",
       "    *",
       "    *",
       "    *",
       "    *"],
      -- 8
      ["*****",
       "*   *",
       "*****",
       "*   *",
       "*****"],
      -- 9
      ["*****",
       "*   *",
       "*****",
       "    *",
       "*****"]
    ]

main :: IO ()

-- definizione della funzione principale
main = do

  -- acquisizione dei due anni
  anni_acquisiti <- acquisisci_anni

  -- prendo il primo anno dalla tupla restituita
  -- prendo il secondo anno dalla tupla restituita
  let primo_anno = fst anni_acquisiti 
  let secondo_anno = snd anni_acquisiti
  -- calcolo e stampo il valore del martedi e del giovedi grasso
  putStrLn (mostra_data (calcola_martedi_giovedi_grasso Martedi (calcolo_pasqua primo_anno)))
  putStrLn (mostra_data (calcola_martedi_giovedi_grasso Giovedi (calcolo_pasqua secondo_anno)))

/*
 * Progetto di Programmazione Logica e Funzionale
 * Studenti: Elia Renzoni e Gianmarco Beligni
 * N. Matricola: 319978 321069 
 * Sessione Estiva a.a 2024/2025
*/

mese(febbraio).
mese(marzo).
mese(aprile).
precedente(febbraio, marzo).
precedente(marzo, aprile).
precedente(aprile, maggio).
successivo(Mese1, Mese2):-
  precedente(Mese2, Mese1).

/* regola che restituisce la data di Pasqua:
   gli argomenti sono il giorno, mese e anno */
pasqua(Giorno, Mese, Anno) :-
    A is Anno mod 19,
    B is Anno mod 4,
    C is Anno mod 7,
    D is (19*A + 24) mod 30,
    E is (2*B + 4*C + 6*D + 5) mod 7,
    Z is D + E,

    % Calcolo della Data
    (Z < 10 -> 
        BaseG is 22 + Z, BaseM = marzo
    ;   
        BaseG is Z - 9, BaseM = aprile
    ),

    % Applica Eccezioni sulla data calcolata
    (BaseG =:= 26, BaseM = aprile -> 
        Giorno = 19, Mese = aprile
    ; BaseG =:= 25, BaseM = aprile, D =:= 28, E =:= 6, A > 10 -> 
        Giorno = 18, Mese = aprile
    ; 
        Giorno = BaseG, Mese = BaseM
    ).


/* regola per controllare che l'anno sia bisestile:
   l'argomento è l'anno 
*/
controlla_bisestile(Anno) :-
    (Anno mod 400 =:= 0; 
     (Anno mod 100 =\= 0, Anno mod 4 =:= 0)).


crea_data(Giorno, Mese, Anno) :-
    integer(Anno), Anno >= 1900, Anno =< 2099,  
    integer(Mese), Mese >= 1, Mese =< 12,
    integer(Giorno), Giorno >= 1,
    giorni_del_mese(GiornoMax, Mese, Anno),
    Giorno =< GiornoMax.

/* regola che restituisce quanti giorno ci sono in un mese:
   il primo argomento è il giorno
   il secondo argomento è il mese
   il terzo argomento è l'anno (importante per Febbraio) 
*/
giorni_del_mese(28, febbraio, Anno) :- \+ controlla_bisestile(Anno).
giorni_del_mese(29, febbraio, Anno) :- controlla_bisestile(Anno).
giorni_del_mese(31, Mese, _) :- Mese = marzo.
giorni_del_mese(30, Mese, _) :- Mese = aprile.


/* regola che calcola la sottrazione di giorni ad una data:
   il primo argomento è il numero di giorno da sottrarre
   il secondo argomento è la data a cui sottrarre i giorni 
   il terzo argomento è il risultato della sottrazione 
*/
sottrai_giorni(Sottraendo, data(Giorno, Mese, Anno), DataArrivo) :-
    GiornoSufficiente is Giorno - Sottraendo,
    (GiornoSufficiente > 0 -> 
        DataArrivo = data(GiornoSufficiente, Mese, Anno)
    ;
       precedente(NuovoMese, Mese),
       giorni_del_mese(MaxGiorni, NuovoMese, Anno),
       NuovoGiorno is Giorno - Sottraendo + MaxGiorni,
       sottrai_giorni(0, data(NuovoGiorno, NuovoMese, Anno), DataArrivo)
    ).

/* regola che calcola la data del Martedì o Giovedì grasso:
   il primo argomento è il giorno grasso
   il secondo argomento è la data di pasqua dell'anno in cui si vuole calcolare i giorni grassi 
*/
calcola_martedi_grasso(Anno, MartediGrasso) :-
    pasqua(PGiorno, PMese, Anno),
    sottrai_giorni(47, data(PGiorno, PMese, Anno), MartediGrasso).
    
calcola_giovedi_grasso(Anno, GiovediGrasso) :-
    pasqua(PGiorno, PMese, Anno),
    sottrai_giorni(52, data(PGiorno, PMese, Anno), GiovediGrasso).

calcola_martedi_giovedi_grasso(martedi, Anno, MartediGrasso, _) :-
    calcola_martedi_grasso(Anno, MartediGrasso).

calcola_martedi_giovedi_grasso(giovedi, Anno, _, GiovediGrasso) :-
    calcola_giovedi_grasso(Anno, GiovediGrasso).       

/* regola che restituisce ASCII art di una cifra:
   l'argomento è la cifra da cui prendere l'ASCII art 
*/
giorno_ascii(Num, GiornoCodificato) :-
    (Num == 0 -> GiornoCodificato = ['*****',
                                     '*   *',
                                     '*   *',
                                     '*   *',
                                     '*****']);
    (Num == 1 -> GiornoCodificato = ['  *  ',
                                     ' **  ',
                                     '* *  ',
                                     '  *  ',
                                     '*****']);
    (Num == 2 -> GiornoCodificato = ['*****',
                                     '    *',
                                     '*****',
                                     '*    ',
                                     '*****']);
    (Num == 3 -> GiornoCodificato = ['*****',
                                     '    *',
                                     '*****',
                                     '    *',
                                     '*****']);
    (Num == 4 -> GiornoCodificato = ['*   *',
                                     '*   *',
                                     '*****',
                                     '    *',
                                     '    *']);
    (Num == 5 -> GiornoCodificato = ['*****',
                                     '*    ',
                                     '*****',
                                     '    *',
                                     '*****']);
    (Num == 6 -> GiornoCodificato = ['*****',
                                     '*    ',
                                     '*****',
                                     '*   *',
                                     '*****']);
    (Num == 7 -> GiornoCodificato = ['*****',
                                     '    *',
                                     '    *',
                                     '    *',
                                     '    *']);
    (Num == 8 -> GiornoCodificato = ['*****',
                                     '*   *',
                                     '*****',
                                     '*   *',
                                     '*****']);
    (Num == 9 -> GiornoCodificato = ['*****',
                                     '*   *',
                                     '*****',
                                     '    *',
                                     '*****']).

/* regola che converte Mese nella sua rappresentazione ASCII art:
   il primo argomento è il mese da cui prendere l'ASCII art 
   il secondo argomento è il mese codificato in ASCII art
*/
mese_ascii(Mese, MeseCodificato) :- 
  (Mese == febbraio -> MeseCodificato = [['*****',
                                          '*    ',
                                          '*****',
                                          '*    ',
                                          '*    '],
                                         ['*****',
                                          '*    ',
                                          '*****',
                                          '*    ',
                                          '*****'],
                                         ['**** ',
                                          '*   *',
                                          '**** ',
                                          '*   *',
                                          '**** ']]);
  (Mese == marzo -> MeseCodificato = [['*   *',
                                       '* * *',
                                       '*   *',
                                       '*   *',
                                       '*   *'],
                                      ['*****',
                                       '*   *',
                                       '*****',
                                       '*   *',
                                       '*   *'],
                                      ['*****',
                                       '*   *',
                                       '*****',
                                       '*  * ',
                                       '*   *']]);
  (Mese == aprile -> MeseCodificato = [['*****',
                                        '*   *',
                                        '*****',
                                        '*   *',
                                        '*   *'],
                                       ['*****',
                                        '*   *',
                                        '*****',
                                        '*    ',
                                        '*    '],
                                       ['*****',
                                        '*   *',
                                        '*****',
                                        '*  * ',
                                        '*   *']]).

/* regola per acquisire l'anno dall'utente: 
   l'argomento è l'anno che rientra nei limiti definiti dalla regola controlla_anno
   questa regola viene chiamata ricorsivamete fino a quando non è soddisfatta la regola controlla_anno
*/
acquisisci_anno(AnnoScelto) :-
  catch(
    (
      read(Anno),
      controlla_anno(Anno, AnnoScelto)
    ),
    _Errore,
    (
      stampa_errore,
      acquisisci_anno(AnnoScelto)
    )
  ).

    
/* regola per controllare l'anno:
   gli utlimi due parametri indicano l'input inserito dall'utente
   e l'anno da restituire.
*/
controlla_anno(AnnoLetto, AnnoRestituire) :- 
  (   AnnoLetto >= 1900,
      AnnoLetto =< 2099 
  ->  AnnoRestituire = AnnoLetto
  ;   stampa_errore, 
      acquisisci_anno(AnnoRestituire)
  ).
    
/* regola per controllare l'anno
   il primo argomento è l'anno da controllare,
   il secondo argomento è l'anno 
*/
stampa_errore :-
  write('Input non valido. L''anno deve essere tra 1900 e 2099.\n').
   
/* regola per stampare caratteri in formato gigante:
   il primo argomento è la lista dei caratteri della prima parola,
   il secondo argomento è la lista dei caratteri della seconda parola
   esempio: stampa_gigante("CIAO", "MONDO")
*/
stampa_gigante(Caratteri1, Caratteri2) :-
  stampa_righe_giganti(0, Caratteri1, Caratteri2).
/* regola ricorsiva per stampare righe di caratteri giganti:
   il primo argomento è l'indice della riga corrente (0-4),
   il secondo argomento è la lista dei caratteri della prima parola,
   il terzo argomento è la lista dei caratteri della seconda parola,
   caso base: quando raggiunge la 5a riga (indice 5) termina con cut (!)
*/
stampa_righe_giganti(5, _, _) :- !.

/* caso ricorsivo per stampare una riga di caratteri giganti:
   il primo argomento è l'indice della riga corrente,
   il secondo argomento è la lista dei caratteri della prima parola,
   il terzo argomento è la lista dei caratteri della seconda parola,
   - stampa la riga per la prima parola
   - aggiunge 4 spazi
   - stampa la riga per la seconda parola
   - va a capo
   - incrementa l'indice e chiama ricorsivamente
*/
stampa_righe_giganti(RigaIndice, Caratteri1, Caratteri2) :-
  stampa_riga(RigaIndice, Caratteri1),
  write('    '),  % Spazio tra primo e secondo gruppo
  stampa_riga(RigaIndice, Caratteri2),
  nl,
  Successivo is RigaIndice + 1,
  stampa_righe_giganti(Successivo, Caratteri1, Caratteri2).
/* regola per stampare una singola riga di caratteri:
   caso base: lista vuota, termina
*/
stampa_riga(_, []).
/* caso ricorsivo per stampare una riga di caratteri:
   il primo argomento è l'indice della riga da stampare,
   il secondo argomento è la lista dei caratteri,
   - estrae la riga specifica dal carattere corrente
   - stampa la riga con uno spazio
   - procede ricorsivamente con il resto della lista
*/
stampa_riga(Index, [Lettera | Resto]) :-
  nth0(Index, Lettera, Riga),
  write(Riga), write(' '),
  stampa_riga(Index, Resto).


/* regola che trasforma una data nella rappesentazione ASCII art:
   l'argomento è una data 
*/
stampa_caratteri_giganti(Giorno, Mese) :-
  Unita is Giorno // 10,
  Decina is Giorno mod 10,
  giorno_ascii(Unita, UnitaCodificata),
  giorno_ascii(Decina, DecinaCodificata),
  mese_ascii(Mese, MeseCodificato),
  stampa_gigante([UnitaCodificata, DecinaCodificata], MeseCodificato).


% definizione della regola principale
programma :-
  write('Programma per il calcolo di Giovedì e Martedì Grasso secondo il calendario Gregoriano'), nl,
  write('Inserire l'' anno per calcolare il Martedì Grasso\n'),
  acquisisci_anno(PrimoAnno),
  write('Inserire l'' anno per calcolare il Giovedì Grasso\n'),
  acquisisci_anno(SecondoAnno),
  calcola_martedi_grasso(PrimoAnno, MartediGrasso),
  calcola_giovedi_grasso(SecondoAnno, GiovediGrasso),
  data(Giorno, Mese, _) = MartediGrasso,
  stampa_caratteri_giganti(Giorno, Mese),
  data(GiornoG, MeseG, _) = GiovediGrasso,
  stampa_caratteri_giganti(GiornoG, MeseG).

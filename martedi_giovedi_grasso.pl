mese(febbraio).
mese(marzo).
mese(aprile).
precedente(febbraio,marzo).
precedente(marzo,aprile).
precedente(aprile,maggio).
successivo(Mese1,Mese2):-
  precedente(Mese2,Mese1).

pasqua(Giorno, Mese, Anno) :-
    A is Anno mod 19,
    B is Anno mod 4,
    C is Anno mod 7,
    D is (19*A + 24) mod 30,
    E is (2*B + 4*C + 6*D + 5) mod 7,
    Z is D + E,

    % Determine base date
    (Z < 10 -> 
        BaseG is 22 + Z, BaseM = marzo
    ;   
        BaseG is Z - 9, BaseM = aprile
    ),

    % Apply exceptions
    (BaseG =:= 26, BaseM = aprile -> 
        Giorno = 19, Mese = aprile
    ; BaseG =:= 25, BaseM = aprile, D =:= 28, E =:= 6, A > 10 -> 
        Giorno = 18, Mese = aprile
    ; 
        Giorno = BaseG, Mese = BaseM
    ).
controlla_bisestile(Anno) :-
    (Anno mod 400 =:= 0; 
     (Anno mod 100 =\= 0, Anno mod 4 =:= 0)).  
crea_data(Giorno, Mese, Anno) :-
    integer(Anno), Anno >= 1900, Anno =< 2099,  
    integer(Mese), Mese >= 1, Mese =< 12,
    integer(Giorno), Giorno >= 1,
    giorni_del_mese(GiornoMax, Mese, Anno),
    Giorno =< GiornoMax.

giorni_del_mese(28, febbraio, Anno) :- \+ controlla_bisestile(Anno).
giorni_del_mese(29, febbraio, Anno) :- controlla_bisestile(Anno).
giorni_del_mese(31, Mese, _) :-Mese = marzo.
giorni_del_mese(30, Mese, _) :-Mese = aprile.
sottrai_giorni(Sottraendo, data(Giorno, Mese, Anno), DataArrivo) :-
    GiornoSufficiente is Giorno - Sottraendo,
    (GiornoSufficiente > 0 -> 
        DataArrivo = data(GiornoSufficiente, Mese, Anno)
    ;
       precedente(NuovoMese,Mese),
       giorni_del_mese(MaxGiorni, NuovoMese, Anno),
       NuovoGiorno is Giorno - Sottraendo + MaxGiorni,
       sottrai_giorni(0, data(NuovoGiorno, NuovoMese, Anno), DataArrivo)
    ).


calcola_martedi_grasso(Anno,MartediGrasso) :-
    pasqua(PGiorno, PMese, Anno),
    sottrai_giorni(47, data(PGiorno, PMese, Anno), MartediGrasso).
    
calcola_giovedi_grasso(Anno, GiovediGrasso) :-
    pasqua(PGiorno,PMese,Anno),
    sottrai_giorni(52,data(PGiorno,PMese,Anno), GiovediGrasso).

calcola_martedi_giovedi_grasso(martedi, Anno, MartediGrasso, _) :-
    calcola_martedi_grasso(Anno, MartediGrasso).

calcola_martedi_giovedi_grasso(giovedi, Anno, _, GiovediGrasso) :-
    calcola_giovedi_grasso(Anno, GiovediGrasso).       

giorno_ascii(Num, GiornoCodificato) :-
    (Num == 0 -> GiornoCodificato = [' *** ', 
                                      '*   *', 
                                      '*   *', 
                                      '*   *', 
                                      ' *** ']);
    (Num == 1 -> GiornoCodificato = ['   * ', 
                                      '  ** ', 
                                      '   * ',
                                      '   * ',
                                      '  ***']);
    (Num == 2 -> GiornoCodificato = ['  ***', 
                                      '    *', 
                                      ' *** ', 
                                      '*    ', 
                                      '*****']);
    (Num == 3 -> GiornoCodificato = [' *** ', 
                                      '    *', 
                                      ' *** ', 
                                      '    *', 
                                      ' *** ']);
    (Num == 4 -> GiornoCodificato = ['*   *', 
                                      '*   *', 
                                      '*****', 
                                      '    *', 
                                      '    *']);
    (Num == 5 -> GiornoCodificato = ['*****', 
                                      '*    ', 
                                      '**** ', 
                                      '    *', 
                                      '**** ']);
    (Num == 6 -> GiornoCodificato = [' *** ', 
                                      '*    ', 
                                      '**** ', 
                                      '*   *', 
                                      ' *** ']);
    (Num == 7 -> GiornoCodificato = ['*****', 
                                      '    *', 
                                      '   * ', 
                                      '  *  ', 
                                      ' *   ']);
    (Num == 8 -> GiornoCodificato = [' *** ', 
                                      '*   *', 
                                      ' *** ', 
                                      '*   *', 
                                      ' *** ']);
    (Num == 9 -> GiornoCodificato = [' *** ', 
                                      '*   *', 
                                      ' ****', 
                                      '    *', 
                                      ' *** ']).
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
                                   [' *** ',
                                    '*   *',
                                    '*****',
                                    '*   *',
                                    '*   *'],
                                   ['**** ',
                                    '*   *',
                                    '**** ',
                                    '*  * ',
                                    '*   *']]);
     (Mese == aprile -> MeseCodificato = [[' *** ',
                                     '*   *',
                                     '*****',
                                     '*   *',
                                     '*   *'],
                                    ['**** ',
                                     '*   *',
                                     '**** ',
                                     '*    ',
                                     '*    '],
                                    ['**** ',
                                     '*   *',
                                     '**** ',
                                     '*  * ',
                                     '*   *']]).

acquisisci_primo_anno(AnnoScelto) :-
    write('Inserisci l''anno per calcolare il Martedì Grasso >> '),
    catch(
        (
            read(PrimoAnno),
            controlla_anno(true, PrimoAnno, AnnoScelto)
        ),
        _Errore,
        (
            stampa_errore,
            nl,
            acquisisci_primo_anno(AnnoScelto)
        )
    ).

acquisisci_secondo_anno(AnnoScelto) :-
    write('Inserisci l''anno per calcolare il Giovedì Grasso >> '),
    catch(
        (
            read(SecondoAnno),
            controlla_anno(false, SecondoAnno, AnnoScelto)
        ),
        _Errore,
        (
            stampa_errore,
            nl,
            acquisisci_secondo_anno(AnnoScelto)
        )
    ).

    
controlla_anno(Acquisizione, AnnoLetto, AnnoRestituire) :- 
    ( (AnnoLetto < 1900 ; AnnoLetto > 2099) ->
        ( Acquisizione == true ->
            stampa_errore, nl, acquisisci_primo_anno(AnnoRestituire)
        ;
            stampa_errore, nl, acquisisci_secondo_anno(AnnoRestituire)
        )
    ;
        AnnoRestituire = AnnoLetto
    ).


stampa_errore :-
    write('Input non valido. L''anno deve essere tra 1900 e 2099.\n').
    
stampa_gigante(CaratteriCodificati) :-
    stampa_righe_giganti(0, CaratteriCodificati).

stampa_righe_giganti(5, _) :- !.
stampa_righe_giganti(RigaIndex, Caratteri) :-
    stampa_riga(RigaIndex, Caratteri),
    nl,
    Next is RigaIndex + 1,
    stampa_righe_giganti(Next, Caratteri).

stampa_riga(_, []).
stampa_riga(Index, [Lettera | Resto]) :-
    nth0(Index, Lettera, Riga),
    write(Riga), write(' '),
    stampa_riga(Index, Resto).


stampa_caratteri_gigati(Giorno, Mese) :-
      Unita is Giorno // 10,
      Decina is Giorno mod 10,
      giorno_ascii(Unita, UnitaCodificata),
      giorno_ascii(Decina, DecinaCodificata),
      mese_ascii(Mese, MeseCodificato),
      stampa_gigante([UnitaCodificata, DecinaCodificata]),
      stampa_gigante(MeseCodificato).
    


programma :-
    write('Programma per il calcolo di Giovedì e Martedì Grasso secondo il calendario Gregoriano'), nl,
    acquisisci_primo_anno(PrimoAnno),
    calcola_martedi_grasso(PrimoAnno, MartediGrasso),
    data(Giorno, Mese, _) = MartediGrasso,
    stampa_caratteri_gigati(Giorno, Mese),
    
    acquisisci_secondo_anno(SecondoAnno),
    calcola_giovedi_grasso(SecondoAnno, GiovediGrasso),
    data(GiornoG, MeseG, _) = GiovediGrasso,
    stampa_caratteri_gigati(GiornoG, MeseG).
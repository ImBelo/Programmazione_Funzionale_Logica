pasqua(Giorno, Mese, Anno) :-
    A is Anno mod 19,
    B is Anno mod 4,
    C is Anno mod 7,
    M_greg is 24,
    N_greg is 5,
    D is (19 * A + M_greg) mod 30,
    E is (2*B+4*C + 6 * D + N_greg) mod 7,
    Z is D + E,
    (Z < 10 -> 
      Giorno is Z + 22,
      Mese = 3
    ;
      Giorno is Z - 9,
      Mese = 4
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

giorni_del_mese(28, 2, Anno) :- \+ controlla_bisestile(Anno).
giorni_del_mese(29, 2, Anno) :- controlla_bisestile(Anno).
giorni_del_mese(31, Mese, _) :- member(Mese, [1,3,5,7,8,10,12]).
giorni_del_mese(30, Mese, _) :- member(Mese, [4,6,9,11]).
sottrai_giorni(Sottraendo, data(Giorno, Mese, Anno), DataArrivo) :-
    GiornoSufficiente is Giorno - Sottraendo,
    (GiornoSufficiente > 0 -> 
        DataArrivo = data(GiornoSufficiente, Mese, Anno)
    ;
        (Mese =:= 1 ->
            NuovoMese is 12,
            NuovoAnno is Anno - 1
        ;
            NuovoMese is Mese - 1,
            NuovoAnno is Anno
        ),
        giorni_del_mese(MaxGiorni, NuovoMese, NuovoAnno),
        NuovoGiorno is Giorno - Sottraendo + MaxGiorni,
        sottrai_giorni(0, data(NuovoGiorno, NuovoMese, NuovoAnno), DataArrivo)
    ).


calcola_martedi_grasso(Anno,MartediGrasso) :-
    % 1. Calcola la data di Pasqua per l'anno specificato
    pasqua(PGiorno, PMese, Anno),
    
    sottrai_giorni(47, data(PGiorno, PMese, Anno), MartediGrasso).
    
calcola_giovedi_grasso(Anno, GiovediGrasso) :-
    pasqua(PGiorno,PMese,Anno),
    sottrai_giorni(52,data(PGiorno,PMese,Anno), GiovediGrasso).

calcola_martedi_giovedi_grasso(martedi, Anno, MartediGrasso, _) :-
    calcola_martedi_grasso(Anno, MartediGrasso).

calcola_martedi_giovedi_grasso(giovedi, Anno, _, GiovediGrasso) :-
    calcola_giovedi_grasso(Anno, GiovediGrasso).       

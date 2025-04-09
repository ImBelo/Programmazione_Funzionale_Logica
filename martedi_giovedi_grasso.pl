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

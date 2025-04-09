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
    Pasqua = pasqua(_,_,Anno),
    sottrai_giorni(52,Pasqua, GiovediGrasso).

calcola_martedi_giovedi_grasso(martedi, Anno, MartediGrasso, _) :-
    calcola_martedi_grasso(Anno, MartediGrasso).

calcola_martedi_giovedi_grasso(giovedi, Anno, _, GiovediGrasso) :-
    calcola_giovedi_grasso(Anno, GiovediGrasso).       
% Main predicate: prints ASCII art for a date (Day, Month, _)
print_ascii_date((Day, Month, _)) :-
    member(Month, [2, 3, 4]),  % Solo febbraio (2), marzo (3), aprile (4)
    day_to_chars(Day, FormattedDayChars),
    month_abbreviation(Month, MonthAbbrChars),
    write('Data: '), write(Day), write(' '), write_month(Month), nl,
    day_to_ascii(FormattedDayChars, DayArt),
    month_to_ascii(MonthAbbrChars, MonthArt),
    combine_art(DayArt, MonthArt, CombinedArt),
    print_art(CombinedArt).

% Formatta il giorno come lista di caratteri (es. 5 → ['0','5'], 15 → ['1','5'])
day_to_chars(Day, Chars) :-
    (Day < 10 
     -> number_chars(Day, [DChar]), Chars = ['0', DChar]
     ;  number_chars(Day, Chars)
    ).

% Nomi dei mesi per la stampa testuale
write_month(2) :- write('feb').
write_month(3) :- write('mar').
write_month(4) :- write('apr').

% Abbreviazioni dei mesi come liste di caratteri
month_abbreviation(2, ['f','e','b']).
month_abbreviation(3, ['m','a','r']).
month_abbreviation(4, ['a','p','r']).

% ASCII art per cifre (usando '*' come atomi)
digit_ascii('0', [' *** ', '*   *', '*   *', '*   *', ' *** ']).
digit_ascii('1', ['  *  ', ' **  ', '  *  ', '  *  ', ' *** ']).
digit_ascii('2', [' *** ', '    *', '  *  ', '*    ', '*****']).
digit_ascii('3', ['**** ', '    *', ' *** ', '    *', '**** ']).
digit_ascii('4', ['*   *', '*   *', '*****', '    *', '    *']).
digit_ascii('5', ['*****', '*    ', '**** ', '    *', '**** ']).
digit_ascii('6', [' *** ', '*    ', '**** ', '*   *', ' *** ']).
digit_ascii('7', ['*****', '    *', '   * ', '  *  ', ' *   ']).
digit_ascii('8', [' *** ', '*   *', ' *** ', '*   *', ' *** ']).
digit_ascii('9', [' *** ', '*   *', ' ****', '    *', ' *** ']).

% ASCII art per lettere (come atomi)
letter_ascii('f', ['*****', '*    ', '**** ', '*    ', '*    ']).
letter_ascii('e', ['*****', '*    ', '*****', '*    ', '*****']).
letter_ascii('b', ['**** ', '*   *', '**** ', '*   *', '**** ']).
letter_ascii('m', ['*   *', '** **', '* * *', '*   *', '*   *']).
letter_ascii('a', [' *** ', '*   *', '*****', '*   *', '*   *']).
letter_ascii('r', ['**** ', '*   *', '**** ', '*  * ', '*   *']).
letter_ascii('p', ['**** ', '*   *', '**** ', '*    ', '*    ']).

% Converte il giorno in ASCII art (atomi già corretti)
day_to_ascii([D1, D2], [Line1, Line2, Line3, Line4, Line5]) :-
    digit_ascii(D1, [D1L1, D1L2, D1L3, D1L4, D1L5]),
    digit_ascii(D2, [D2L1, D2L2, D2L3, D2L4, D2L5]),
    combine_lines(D1L1, D2L1, Line1),
    combine_lines(D1L2, D2L2, Line2),
    combine_lines(D1L3, D2L3, Line3),
    combine_lines(D1L4, D2L4, Line4),
    combine_lines(D1L5, D2L5, Line5).

% Combina due linee con uno spazio (usa atomi)
combine_lines(Line1, Line2, Combined) :-
    atom_concat(Line1, ' ', Temp),
    atom_concat(Temp, Line2, Combined).

% Converte il mese in ASCII art (atomi già corretti)
month_to_ascii([M1, M2, M3], [Line1, Line2, Line3, Line4, Line5]) :-
    letter_ascii(M1, [M1L1, M1L2, M1L3, M1L4, M1L5]),
    letter_ascii(M2, [M2L1, M2L2, M2L3, M2L4, M2L5]),
    letter_ascii(M3, [M3L1, M3L2, M3L3, M3L4, M3L5]),
    combine_3lines(M1L1, M2L1, M3L1, Line1),
    combine_3lines(M1L2, M2L2, M3L2, Line2),
    combine_3lines(M1L3, M2L3, M3L3, Line3),
    combine_3lines(M1L4, M2L4, M3L4, Line4),
    combine_3lines(M1L5, M2L5, M3L5, Line5).

% Combina tre linee con spazi (usa atomi)
combine_3lines(L1, L2, L3, Combined) :-
    atom_concat(L1, ' ', Temp1),
    atom_concat(Temp1, L2, Temp2),
    atom_concat(Temp2, ' ', Temp3),
    atom_concat(Temp3, L3, Combined).

% Combina giorno e mese con 5 spazi in mezzo
combine_art(DayArt, MonthArt, CombinedArt) :-
    DayArt = [D1, D2, D3, D4, D5],
    MonthArt = [M1, M2, M3, M4, M5],
    combine_with_spaces(D1, M1, Combined1),
    combine_with_spaces(D2, M2, Combined2),
    combine_with_spaces(D3, M3, Combined3),
    combine_with_spaces(D4, M4, Combined4),
    combine_with_spaces(D5, M5, Combined5),
    CombinedArt = [Combined1, Combined2, Combined3, Combined4, Combined5].

% Combina due linee con 5 spazi (usa atomi)
combine_with_spaces(Line1, Line2, Combined) :-
    atom_concat(Line1, '     ', Temp),
    atom_concat(Temp, Line2, Combined).

% Stampa l'ASCII art
print_art([]).
print_art([Line|Rest]) :-
    write(Line), nl,
    print_art(Rest).  

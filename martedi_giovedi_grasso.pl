% ==============================
% Calcolo della Pasqua
% ==============================
pasqua(Anno, Giorno, Mese) :-
    A is Anno mod 19,
    B is Anno // 100,
    C is Anno mod 100,
    D is B // 4,
    E is B mod 4,
    F is (B + 8) // 25,
    G is (B - F + 1) // 3,
    H is (19 * A + B - D - G + 15) mod 30,
    I is C // 4,
    K is C mod 4,
    L is (32 + 2 * E + 2 * I - H - K) mod 7,
    M is (A + 11 * H + 22 * L) // 451,
    N is (H + L - 7 * M + 114),
    Mese is N // 31,
    Giorno is (N mod 31) + 1.

% ==============================
% Giorni in ciascun mese
% ==============================
giorni_mese(_, 1, 31).
giorni_mese(Anno, 2, Giorni) :- % febbraio e bisestili
    (Anno mod 4 =:= 0, (Anno mod 100 =\= 0 ; Anno mod 400 =:= 0)) -> Giorni = 29 ; Giorni = 28.
giorni_mese(_, 3, 31).
giorni_mese(_, 4, 30).
giorni_mese(_, 5, 31).
giorni_mese(_, 6, 30).
giorni_mese(_, 7, 31).
giorni_mese(_, 8, 31).
giorni_mese(_, 9, 30).
giorni_mese(_, 10, 31).
giorni_mese(_, 11, 30).
giorni_mese(_, 12, 31).

% ==============================
% Sottrai giorni da una data
% ==============================
sottrai_giorni(Anno, Mese, Giorno, N, NewAnno, NewMese, NewGiorno) :-
    N =< 0, !,
    NewAnno = Anno, NewMese = Mese, NewGiorno = Giorno.

sottrai_giorni(Anno, Mese, Giorno, N, NewAnno, NewMese, NewGiorno) :-
    N > 0,
    Giorno > 1,
    Giorno1 is Giorno - 1,
    N1 is N - 1,
    sottrai_giorni(Anno, Mese, Giorno1, N1, NewAnno, NewMese, NewGiorno).

sottrai_giorni(Anno, Mese, 1, N, NewAnno, NewMese, NewGiorno) :-
    Mese > 1,
    M1 is Mese - 1,
    giorni_mese(Anno, M1, GiorniPrecedente),
    N1 is N - 1,
    sottrai_giorni(Anno, M1, GiorniPrecedente, N1, NewAnno, NewMese, NewGiorno).

sottrai_giorni(Anno, 1, 1, N, NewAnno, NewMese, NewGiorno) :-
    A1 is Anno - 1,
    giorni_mese(A1, 12, GiorniDicembre),
    N1 is N - 1,
    sottrai_giorni(A1, 12, GiorniDicembre, N1, NewAnno, NewMese, NewGiorno).

% ==============================
% Martedì e Giovedì Grasso
% ==============================
martedi_grasso(Anno, Giorno, Mese) :-
    pasqua(Anno, GP, MP),
    sottrai_giorni(Anno, MP, GP, 47, Anno, Mese, Giorno).

giovedi_grasso(Anno, Giorno, Mese) :-
    martedi_grasso(Anno, GM, MM),
    sottrai_giorni(Anno, MM, GM, 6, Anno, Mese, Giorno).

% ==============================
% Caratteri giganti
% ==============================
char_pattern('0', ["*****", "*   *", "*   *", "*   *", "*****"]).
char_pattern('1', ["  *  ", " **  ", "  *  ", "  *  ", "*****"]).
char_pattern('2', ["*****", "    *", "*****", "*    ", "*****"]).
char_pattern('3', ["*****", "    *", " *** ", "    *", "*****"]).
char_pattern('4', ["*   *", "*   *", "*****", "    *", "    *"]).
char_pattern('5', ["*****", "*    ", "*****", "    *", "*****"]).
char_pattern('6', ["*****", "*    ", "*****", "*   *", "*****"]).
char_pattern('7', ["*****", "    *", "   * ", "  *  ", "  *  "]).
char_pattern('8', ["*****", "*   *", "*****", "*   *", "*****"]).
char_pattern('9', ["*****", "*   *", "*****", "    *", "*****"]).
char_pattern('G', ["*****", "*    ", "* ***", "*   *", "*****"]).
char_pattern('E', ["*****", "*    ", "*****", "*    ", "*****"]).
char_pattern('N', ["*   *", "**  *", "* * *", "*  **", "*   *"]).
char_pattern('F', ["*****", "*    ", "*****", "*    ", "*    "]).

month_abbr(1, ['G','E','N']).
month_abbr(2, ['F','E','B']).
month_abbr(3, ['M','A','R']).
month_abbr(4, ['A','P','R']).
month_abbr(5, ['M','A','G']).
month_abbr(6, ['G','I','U']).
month_abbr(7, ['L','U','G']).
month_abbr(8, ['A','G','O']).
month_abbr(9, ['S','E','T']).
month_abbr(10, ['O','T','T']).
month_abbr(11, ['N','O','V']).
month_abbr(12, ['D','I','C']).


% ==============================
% Concatenazione delle stringhe
% ==============================
concatena_lista([], '').
concatena_lista([H|T], Risultato) :-
    concatena_lista(T, RisultatoRestante),
    string_concat(H, RisultatoRestante, Risultato).

% ==============================
% Stampa blocchi
% ==============================
stampa_blocchi([]).
stampa_blocchi(Codici) :-
    between(1, 5, N),
    findall(R,
        (
            member(C, Codici),
            char_pattern(C, Pattern),
            nth1(N, Pattern, R)
        ),
        RigaCodice),
    string_concat(RigaCodice, '  ', Riga),
    writeln(Riga),
    fail.
stampa_blocchi(_).

% ==============================
% Programma principale
% ==============================
programma :-
    write('Inserisci il primo anno (Martedi Grasso): '), read(Anno1),
    write('Inserisci il secondo anno (Giovedi Grasso): '), read(Anno2),
    martedi_grasso(Anno1, Giorno1, Mese1),
    giovedi_grasso(Anno2, Giorno2, Mese2),
    format('\nMartedi Grasso ~w: ~w/~w\n', [Anno1, Giorno1, Mese1]),
    format('Giovedi Grasso ~w: ~w/~w\n\n', [Anno2, Giorno2, Mese2]),
    number_chars(Giorno1, GiornoChars1),
    month_abbr(Mese1, LettereMese1),
    append(GiornoChars1, LettereMese1, Blocchi1),
    write('--- MARTEDI GRASSO ---'), nl, stampa_blocchi(Blocchi1),
    number_chars(Giorno2, GiornoChars2),
    month_abbr(Mese2, LettereMese2),
    append(GiornoChars2, LettereMese2, Blocchi2),
    write('\n--- GIOVEDI GRASSO ---'), nl, stampa_blocchi(Blocchi2).
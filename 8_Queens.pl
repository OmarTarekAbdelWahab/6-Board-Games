:- use_module(library(clpfd)).
queens(Qs) :-
        length(Qs, 8),
        Qs ins 1..8,
        safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :-
        safe_queens(Qs, Q, 1),
        safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
        Q0 #\= Q,
        abs(Q0 - Q) #\= D0,
        D1 #= D0 + 1,
        safe_queens(Qs, Q0, D1).
write_on_file(File, Text) :-
    open(File, write, Stream),
    write(Stream, Text),
    close(Stream).
    

read_from_file(File) :-
    open(File, read, Stream),
    read(Stream, House1),
    %get_char(Stream, Char1),
    %process_the_stream(Char1, Stream),
    close(Stream),
    queens(House1),
    label(House1),nl,
    write_on_file('sol.txt', House1).

process_the_stream(end_of_file, _) :- !.

process_the_stream(Char, Stream) :-
    write(Char),
    get_char(Stream, Char2),
    process_the_stream(Char2, Stream).
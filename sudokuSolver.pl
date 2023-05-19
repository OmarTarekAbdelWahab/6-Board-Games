:- use_module(library(clpfd)).
sudoku(Rows) :- 
    length(Rows, 9),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs),
    Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    squares(As,Bs,Cs),
    squares(Ds,Es,Fs),
    squares(Gs,Hs,Is).

squares([],[],[]).
squares([N1,N2,N3|Ns1],
        [N4,N5,N6|Ns2],
        [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    squares(Ns1,Ns2,Ns3).

    
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
    sudoku(House1),
    maplist(label, House1),nl,
    write_on_file('sol.txt', House1).

process_the_stream(end_of_file, _) :- !.

process_the_stream(Char, Stream) :-
    write(Char),
    get_char(Stream, Char2),
    process_the_stream(Char2, Stream).
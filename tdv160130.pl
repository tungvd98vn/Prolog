% 1. Odd multiple of 3

% Helpers
% Check if number is odd
checkodd(I):-
    M is I mod 2,
    M is 1 ->  true
    ; false.

% Check if multiple of 3
multiple(I):-
    M is I mod 3,
    M is 0 -> N is I/3, checkodd(N)
    ; false.

oddMultOf3(I):-
    not(integer(I)) ->  print("ERORR: The given parameter is not an integer")
    ; multiple(I).

% 2. List Product

list_prod([], 0).
list_prod([H], H).
list_prod([H|T], P):-
    list_prod(T, R),
    P is R * H.

% 3. Palindrome

palindrome([]).
palindrome([_]).
palindrome(L) :-
    append([H|T], [H], L),
    palindrome(T).

% 4. Second Minumum

% Helpers
% Find the length of the list
len([] ,0).
len([_|T], R):-
       len(T, R1),
       R is R1+1.

% Check valid inputs
check([]):- true.
check([H|T]):-
      not(number(H)) -> print("Error: "), print(H), print(" is not a number."),false
      ; check(T).

% Find the min of first 2 numbers of the list
find([], []).
find([_,H2|_], M2):-
    M2 is H2.

secondMin(Lst, M2):-
    sort(Lst, Lst2),
    len(Lst2, L),
    L < 2 ->  print("ERROR: :List has fewer than two unique elements.")
    ; check(Lst) ->  
    sort(Lst, Lst2),
    find(Lst2, M2),
    !
    ;!.

%5. Classify

classify([], [], []). % Even = Odd = []
classify([H|T], E, O):-
    not(checkodd(H)),
    classify(T, E1, O),
    append(E1, [H], E),
    !.
classify([H|T], E, O):-
    checkodd(H),
    classify(T, E, O1),
    append(O1, [H], O),
    !.

%6. Bookends

%Helpers
%Prefix
pre([], _).
pre([H|T], [H|A]):-
	pre(T, A).

%Suffix
suf(L, L).
suf(S, [_|T]):-
	suf(S, T),
	!.

bookends(P, S, L):-
	pre(P, L),
	suf(S, L).

% 7. Subslice

subslice([], _).
subslice([H|T], [A|B]):-
	H is A,
	subslice(T, B),
	!.
subslice([H|T], [A|B]) :-
	not(H is A),
	subslice([H|T], B),
	!.

% 8. Shift

% Helpers
% Reverse
rev([], L2, L2).
rev([H|T], L2, R):-
    rev(T, [H|L2], R).
rev(L,R):-
    rev(L, [], R).

% Concatenate 2 lists
con([],L2,L2).
con([H1|T1], L2, [H1|T3]):-
    con(T1, L2, T3).

% Create a sublist size N
sublist(L, 0, L).
sublist([_|T], N, X):-
    N2 is N-1,
    sublist(T, N2, X),
    !.

% Shift left N
shiftl(List, N, Shifted):-
	len(List, L),
	rev(List, F),
	sublist(F, L - N, Frev),
	sublist(List, N, S),
	rev(S, Srev),
	con(Frev, Srev, R1),
	rev(R1, Shifted).
    
% Shift right N
shiftr(List, N, Shifted):-
    len(List, L),
	rev(List,F),
	sublist(F, N, Frev),
	sublist(List, L - N, S),
	rev(S, Srev),
	con(Frev, Srev, R1),
	rev(R1, Shifted).

shift(List, N, Shifted):-
    N < 0 ->   N1 is (-1 * N), shiftr(List, N1, Shifted)
    ; shiftl(List, N, Shifted).
                     
% 9. Luhn Algorithm

% Helpers
% Sum of 2 digit number.
sumh(N, R):-
    M is mod(N, 10),
    D is div(N, 10),
    R is M + D,
    !.

% Sum of a 2 digit number.
sum(N, R):-
    M is mod(N, 10),
    D is div(N, 10),
    E is D * 2,
    E > 9 -> sumh(E, X), R is X + M,
    !;
    M is mod(N, 10),
    D is div(N, 10),
    E is D * 2,
    R is M + E,
    !.

% Sum the digits
sumd(0, S, R):-
    R is S,
    !.
sumd(N, S, R):-
    M is mod(N, 100),
    D is div(N, 100),
    sum(M, S2),
    K is S + S2,
    sumd(D, K, R),
    !.

luhn(N):-
    sumd(N, 0, R),
    M is mod(R, 10),
    M = 0.

% 10. Graph

% Knowledge Base
% edge(a,b).
% edge(b,c).
% edge(c,d).
% edge(d,a).
% edge(d,e).
% edge(b,a).

% Path
path(Node,Node).
path(Node1, Node2):-
    edge(Node1, X),
    path(X, Node2),
    !.

% Cycle
cycle(Node):-
    edge(Node,X),
    path(X,Node),
    !.

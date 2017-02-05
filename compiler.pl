% SWI-Prolog version 6.6.4

% Lexer
tokens(Z) --> [40, 42], !, restOfComment, tokens(Y), { Z = Y }.
% Assignment
tokens(Z) --> ":=", !, tokens(Y), { Z = [:= | Y] }.

% Comparison
tokens(Z) --> "=", !, tokens(Y), { Z = [= | Y] }.
tokens(Z) --> "<>", !, tokens(Y), { Z = [<> | Y] }.
tokens(Z) --> "<=", !, tokens(Y), { Z = [<= | Y] }.
tokens(Z) --> ">=", !, tokens(Y), { Z = [>= | Y] }.
tokens(Z) --> "<", !, tokens(Y), { Z = [< | Y] }.
tokens(Z) --> ">", !, tokens(Y), { Z = [> | Y] }.

% Parenthesis
tokens(Z) --> [40], !, tokens(Y), { Z = [ '(' | Y] }.
tokens(Z) --> [41], !, tokens(Y), { Z = [ ')' | Y] }.

tokens(Z) --> ",", !, tokens(Y), { Z = [',' | Y] }.

% Colon
tokens(Z) --> ";", !, tokens(Y), { Z = [; | Y] }.

% Math
tokens(Z) --> "+", !, tokens(Y), { Z = [+ | Y] }.
tokens(Z) --> "-", !, tokens(Y), { Z = [- | Y] }.
tokens(Z) --> "*", !,tokens(Y), { Z = [* | Y] }.

% Numbers
tokens(Z) --> digit(D), number(D, N), !, tokens(Y), { R = tokNumber(N), Z = [R | Y] }.

% Whitespaces
tokens(Z) --> whiteSpace(_), !, tokens(Y), { Z = Y }.

% Words & alphanums
tokens(Z) --> letter(L), identifier(L, Id), !, tokens(Y), { member(Id, [program,
                                                                        begin,
                                                                        end,
                                                                        local,
                                                                        procedure,
                                                                        value,
                                                                        if,
                                                                        then,
                                                                        fi,
                                                                        else,
                                                                        while,
                                                                        call,
                                                                        return,
                                                                        write,
                                                                        read,
                                                                        div,
                                                                        mod,
                                                                        and,
                                                                        do,
                                                                        done,
                                                                        or,
                                                                        not]) ->
                                                                        Z = [Id | Y] ;
                                                                        Tok = tokVar(Id),
                                                                        Z = [Tok | Y]}.


tokens(Z) --> [_], !, tokens(Y), { Z = [tokenUnknown | Y] }.
tokens(Z) --> [], !, {Z = []}.


restOfComment --> [42, 41], !.
restOfComment --> [_], restOfComment.

whiteSpace(S) --> [S], { code_type(S, space) }.

% Numbers stuff by TWI
digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { A = 39 }, !, alphanum(T).
alphanum([A|T]) -->
   [A], { A = 95 }, !, alphanum(T).
alphanum([A|T]) -->
   [A], { code_type(A, alnum) }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.

% Parser
program(Ast) -->
  [program], [tokVar(_)], body(Ast).
body(Body) -->
  declarations(D), [begin], compoundStatement(M), [end], !, { Body = block(D, M) }.

empty([]) --> [].
declarations(D) --> declaration(H), declarations(R), { D = [H | R] }, !;
                    empty(D).

declaration(D) -->
  declarator(D);
  procedure(D).

declarator(D) -->
  [local], variables(V), { D = local(V) }.
variables(Vars) -->
  variable(H), ([','], !, variables(V), { Vars = [H | V] } ;
  { Vars = [H] }).
variable(V) --> [tokVar(Name)], { V = variable(Name) }.

procedureName(Pname) -->
  [tokVar(Name)], { Pname = procedureName(Name) }.

procedure(P) -->
  [procedure], procedureName(PName), ['('], formalArguments(A), [')'], body(B), { P = procedure(PName, A, B) }.

formalArguments(A) -->
  formalArgument(R), ([','], !, formalArguments(As), { A = [R | As] } ;
  { A = [R] }).
formalArguments([]) --> [].


formalArgument(A) -->
  variable(A), !;
  [tokNumber(N)], { A = value(N) }, !;
  [value], variable(V), { A = value(V) }.

compoundStatement(S) --> statement(T), ([;], !, compoundStatement(R), { S = [T | R] };
    { S = [T] }).

procedureCall(E) -->
  procedureName(PName), ['('], factualArguments(A), [')'], { E = procedureCall(PName, A) }.

statement(S) -->
  [tokVar(V), :=], !, arithExpr(Expr), { S = assign(tokVar(V), Expr) } ;
  [if], !, boolExpr(Bool), [then], compoundStatement(IfBody), (
    [else], !, compoundStatement(ElseBody), [fi], { S = if(Bool, IfBody, ElseBody) };
    [fi], { S = if(Bool, IfBody) }
  );
  [while], !, boolExpr(Bool), [do], compoundStatement(Body), [done], { S = while(Bool, Body) };
  [call], !, procedureCall(S);
  [return], !, arithExpr(Expr), { S = return(Expr) };
  [read], !, variable(V), { S = read(V) };
  [write], !, arithExpr(Expr), { S = write(Expr) }.

factualArguments(A) -->
    factualArgument(H), ([','], !, factualArguments(R), { A = [H | R] } ;
    { A = [H] }, ! ) ;
    { A = [] }.
factualArgument(A) -->
    arithExpr(A).

arithExpr(Expr) --> summand(Summand), arithExpr(Summand, Expr).
arithExpr(Acc, Expr) --> addOp(Op), !, summand(Summand), { Acc1 = [Op, Acc, Summand] }, arithExpr(Acc1, Expr).
arithExpr(Acc, Acc) --> [].

summand(Expr) --> factor(Factor), summand(Factor, Expr).
summand(Acc, Expr) --> multOp(Op), !, factor(Factor), { Acc1 = [Op, Acc, Factor] }, summand(Acc1, Expr).
summand(Acc, Acc) --> [].

factor(Expr) -->
  [-], posFactor(Ex), { Expr = minus(Ex) }, !;
  posFactor(Expr).
posFactor(Expr) -->
    (
        ['('], !, arithExpr(Expr), [')'];
        [tokNumber(N)], !, { Expr = constant(N) };
  procedureCall(Expr), ! ;
        [tokVar(Var)], { Expr = variable(Var) }
    ).


boolExpr(Bool) --> disjunct(Disjunct), boolExpr(Disjunct, Bool).
boolExpr(Acc, Bool) --> [or], !, disjunct(Disjunct), { Acc1 = [or, Acc, Disjunct] }, boolExpr(Acc1, Bool).
boolExpr(Acc, Acc) --> [].

disjunct(Disjunct) --> conjunct(Conjunct), disjunct(Conjunct, Disjunct).
disjunct(Acc, Disjunct) --> [and], !, conjunct(Conjunct), { Acc1 = [and, Acc, Conjunct] }, disjunct(Acc1, Disjunct).
disjunct(Acc, Acc) --> [].

conjunct(Conjunct) -->
  [not], !, posConjuct(C), { Conjunct = not(C) } ;
  posConjuct(Conjunct).
posConjuct(Conjunct) -->
    (
    arithExpr(LExpr), !, relOp(Op), arithExpr(RExpr), { Conjunct = [Op, LExpr, RExpr] };
    ['('], !, boolExpr(Conjunct), [')']
   ).

multOp(*) -->
  [*], !.
multOp(div) -->
  [div], !.
multOp(mod) -->
  [mod], !.

addOp(+) -->
  [+], !.
addOp(-) -->
  [-], !.

relOp(<=) -->
  [<=], !.
relOp(>=) -->
  [>=], !.
relOp(<) -->
  [<], !.
relOp(>) -->
  [>], !.
relOp(<>) -->
  [<>], !.
relOp(=) -->
  [=].

% End of Parser
parse(CharCodeList, Absynt) :-
    phrase(tokens(TokList), CharCodeList),
    phrase(program(Absynt), TokList).

% Konertowanie linii na liczbę
toDecimal(line(_, [V]), V) :- integer(V), V >= 0, !.
toDecimal(line(_, [V]), VI) :- integer(V), V < 0, !, VI is V + 65536.
toDecimal(line(_, [A, B, C, D]), N) :-
  command(A, AC),
  command(B, BC),
  command(C, CC),
  command(D, DC), !,
  N is DC + CC * 16 + BC * 256 + AC * 4096.

command(nop, 0).
command(syscall, 1).
command(load, 2).
command(store, 3).
command(swapa, 4).
command(swapd, 5).
command(branchz, 6).
command(branchn, 7).
command(jump, 8).
command(const, 9).
command(add, 10).
command(sub, 11).
command(mul, 12).
command(div, 13).
command(shift, 14).
command(nand, 15).

add_to_dict([], Acc, C, Acc, C).
add_to_dict([H | T], Acc, Counter, NAcc, NCounter) :-
  Dcounter is Counter - 1,
  add_to_dict(T, [(H, Counter) | Acc], Dcounter, NAcc, NCounter).

dict(Decs, Dict) :-
  dict(Decs, [], Dict, 65534), !.
dict([], A, A, _) :- !.
dict([local(Body) | T], Acc, Dict, Counter) :-
  !, add_to_dict(Body, Acc, Counter, Nacc, NCounter),
  dict(T, Nacc, Dict, NCounter).
dict([_ | T], Acc, Dict, Counter) :-
  dict(T, Acc, Dict, Counter).

algol16(CharCodeList, Ret) :-
  parse(CharCodeList, AST),
  AST = block(Decls, Ins),
  dict(Decls, Dict),
  (Dict = [(_, LastInd) | _], ! ; LastInd = 65534, !),
  StackB is LastInd - 1,
  phrase(instructions(Ins, StackB, Dict), Commands, [const, 0, syscall]),
  toLines(Commands, Lines),
  maplist(toDecimal, Lines, Ret).

takeN(R, 0, [], R) :- !.
takeN([], _, [], []).
takeN([H | T1], C, [H | T2], R) :-
  DC is C - 1,
  takeN(T1, DC, T2, R).

count(X, L, N) :-
  count(L, X, 0, N), !.
count([], _, C, C) :- !.
count([H | T], E, C, N) :-
  not(H \= E),
  !, C1 is C+1,
  count(T, E, C1, N).
count([_ | T], E, C, N) :-
  count(T, E, C, N).

addLineNumber([], Ret, Ret, NC, NC) :- !.
addLineNumber([H | T], Ret - Nend, Ret - NNend, C, NC) :-
  Line = line(C, [H]),
  Nend = [Line | X],
  C1 is C + 1,
  !, addLineNumber(T, Ret - X, Ret - NNend, C1, NC).

newList(_, N, []) :- 0 is N, !.
newList(E, C, [E | T]) :-
  DC is C - 1,
  !, newList(E, DC, T).

toLines(Commands, Ret) :-
  toLines(Commands, Ret - Ret, [], [], 0), !.
toLines([], _ - [], [], [], _) :- !.
toLines([], Ret - End, AccC, AccI, C) :-
  !, length(AccC, I),
  (I > 0 -> newList(nop, 4 - I, O),
  append(AccC, O, NO),
  T = [line(C, NO) | Nend],
  End = T,
  C1 is C + 1; C1 = C, Nend = End),
  !, addLineNumber(AccI, Ret - Nend, Ret - NNend, C1, _),
  toLines([], Ret - NNend, [], [], C1).
toLines([H | T], Ret - E, AccC, AccI, C) :-
  (integer(H) ; var(H)), !,
  (AccC = [] -> !, E = [line(C, [H]) | X], C1 is C + 1, toLines(T, Ret - X, AccC, AccI, C1);
  (append(AccI, [H], NAccI),
    toLines(T, Ret - E, AccC, NAccI, C))).
toLines([jump | T], Ret - End, AccC, AccI, C) :-
  !, append(AccC, [jump], NAccC), length(NAccC, I),
  newList(nop, 4 - I, O),
  append(NAccC, O, NO),
  End = [line(C, NO) | Nend],
  C1 is C + 1,
  addLineNumber(AccI, Ret - Nend, Ret - NNend, C1, H),
  toLines(T, Ret - NNend, [], [], H).
toLines([marker(H) | T], Ret - End, AccC, AccI, C) :-
  !, length(AccC, I),
  (I > 0 -> newList(nop, 4 - I, O),
  append(AccC, O, NO),
  End = [line(C, NO) | Nend],
  C1 is C + 1; C1 = C, Nend = End),
  addLineNumber(AccI, Ret - Nend, Ret - NNend, C1, H),
  toLines(T, Ret - NNend, [], [], H).
toLines([H | T], Ret - End, AccC, AccI, C) :-
  !, append(AccC, [H], NAccC),
  (length(NAccC, 4) -> !, End = [line(C, NAccC) | Nend], count(const, NAccC, O), !, C1 is C + 1,
    takeN(AccI, O, LAccI, NAccI), !, addLineNumber(LAccI, Ret - Nend, Ret - NNend, C1, NC1),
    toLines(T, Ret - NNend, [], NAccI, NC1) ;
    toLines(T, Ret - End, NAccC, AccI, C)).


instructions([], _, _) --> [].
instructions([read(V) | T], S, Dict) -->
  !, {member((V, Addr), Dict)}, !,
  [const, Addr, swapa, const, 1, syscall, store], instructions(T, S, Dict).
instructions([write(Arith_Expr) | T], S, Dict) -->
  !, translateArithExpr(Arith_Expr, S, Dict), [swapd, const, 2, syscall], instructions(T, S, Dict).
instructions([assign(tokVar(Name), Arith_Expr) | T], S, Dict) -->
  !, translateArithExpr(Arith_Expr, S, Dict), { member((variable(Name), Addr), Dict), ! },
  [swapa, const, Addr, swapa, store], instructions(T, S, Dict).
instructions([if(BoolExpr, Body) | T], S, Dict) -->
  !, translateBoolExpr(BoolExpr, S, Dict), [swapa, const, No, swapa, branchz],
  instructions(Body, S, Dict),
  [marker(No)],
  instructions(T, S, Dict).
instructions([if(BoolExpr, TrueBody, FalseBody) | T], S, Dict) -->
  !, translateBoolExpr(BoolExpr, S, Dict), [swapa, const, No, swapa, branchz],
  instructions(TrueBody, S, Dict),
  [const, True, jump],
  [marker(No)],
  instructions(FalseBody, S, Dict),
  [marker(True)],
  instructions(T, S, Dict).
instructions([while(BoolExpr, Body) | T], S, Dict) -->
  [marker(Bool)],
  translateBoolExpr(BoolExpr, S, Dict), [swapa, const, End, swapa, branchz],
  instructions(Body, S, Dict),
  [const, Bool, jump], % Skocz do warunku
  [marker(End)], % Zakończ pętle
  instructions(T, S, Dict).

translateArithExpr(minus(constant(X)), _, _) -->
  !, {NegX is -X}, [const, NegX].
translateArithExpr(minus(X), S, D) -->
  !, translateArithExpr(X, S, D), [swapd, const, -1, mul].
translateArithExpr(constant(X), _, _) -->
  !, [const, X].
translateArithExpr(tokVar(X), _, Dict) -->
  !, translateArithExpr(variable(X), _, Dict).
translateArithExpr(variable(X), _, Dict) -->
  !, { member((variable(X), Addr), Dict), ! }, [const, Addr, swapa, load].
translateArithExpr([+, L, R], S, Dict) -->
  !, translateComplexArithExpr(L, R, S, Dict),
  [add].
translateArithExpr([-, L, R], S, Dict) -->
  !, translateComplexArithExpr(L, R, S, Dict),
  [sub].
translateArithExpr([*, L, R], S, Dict) -->
  !, translateComplexArithExpr(L, R, S, Dict),
  [mul].
translateArithExpr(['div', L, R], S, Dict) -->
  !, translateComplexArithExpr(L, R, S, Dict),
  [div].
translateArithExpr(['mod', L, R], S, Dict) -->
  !, translateComplexArithExpr(L, R, S, Dict),
  [div, swapd, const, -16, swapd, shift].

translateComplexArithExpr(L, R, S, Dict) -->
  !, translateArithExpr(L, S, Dict), [swapa, const, S, swapa, store], % L na stos
  { S1 is S - 1 },
  translateArithExpr(R, S1, Dict), [swapd], % R do argumentu
  [const, S, swapa, load]. % L do Acc


translateBoolExpr([=, L, R], S, Dict) -->
  !, translateBoolExpr(not([<>, L, R]), S, Dict).
translateBoolExpr([<>, L, R], S, Dict) -->
  !, translateArithExpr([-, L, R], S, Dict),
  [swapa, const, Jump0, swapa, branchz, const, 1, marker(Jump0)].
translateBoolExpr([<, L, R], S, Dict) -->
  !, translateArithExpr(L, S, Dict),
  [swapa, const, S, swapa, store],
  { SD is S - 1 },
  translateArithExpr(R, SD, Dict),
  [swapa, const, SD, swapa, store],
  [swapa, const, RNeg, swapa, branchn],
  [const, S, swapa, load, swapa, const, LNeg, swapa, branchn],
  [const, Both, jump], % Obie nieujemne
  [marker(LNeg), const, 1, swapa, const, End, jump], % Lewa ujemna
  [marker(RNeg), const, S, swapa, load, swapa, const, Both, swapa, branchn],
  [const, 0, swapa, const, End, jump], % Prawa ujemna
  [marker(Both), const, SD, swapa, load, swapd, const, S, swapa, load, sub],
  [swapa, const, Jump0, swapa, branchn, const, 0, swapa, const, Jump1, jump, marker(Jump0), const, 1, swapa, marker(Jump1)],
  [marker(End), swapa].
translateBoolExpr([>, L, R], S, Dict) -->
  !, translateBoolExpr([<, R, L], S, Dict).
translateBoolExpr([>=, L, R], S, Dict) -->
  !, translateBoolExpr(not([<, L, R]), S, Dict).
translateBoolExpr([<=, L, R], S, Dict) -->
  !, translateBoolExpr([>=, R, L], S, Dict).

translateBoolExpr(([and, L, R]), S, Dict) -->
  !,
  translateBoolExpr(L, S, Dict),
  [swapa, const, End, swapa, branchz, swapa],
  translateBoolExpr(R, S, Dict),
  [marker(End)].
translateBoolExpr(([or, L, R]), S, Dict) -->
  !,
  translateBoolExpr(L, S, Dict),
  [swapd, const, -1, mul, swapa, const, True, swapa, branchn],
  translateBoolExpr(R, S, Dict),
  [swapa, const, End, jump],
  [marker(True), const, 1, swapa],
  [marker(End), swapa].
translateBoolExpr(not(L), S, Dict) -->
  !, translateBoolExpr(L, S, Dict),
  [swapd, const, 1, sub].

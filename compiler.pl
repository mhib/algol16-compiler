% SWI-Prolog version 6.6.4

% Lexer
tokens(Z) --> [40, 42], !, rest_of_comment, tokens(Y), { Z = Y }.
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
tokens(Z) --> white_space(_), !, tokens(Y), { Z = Y }.

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


rest_of_comment --> [42, 41], !.
rest_of_comment --> [_], rest_of_comment.

white_space(S) --> [S], { code_type(S, space) }.

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
   [A] ,{ A = 39 }, !, alphanum(T).
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
  declarations(D), [begin], compound_statement(M), [end], !, { Body = block(D, M) }.

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

procedure_name(Pname) -->
  [tokVar(Name)], { Pname = procedure_name(Name) }.

procedure(P) -->
  [procedure], procedure_name(PName), ['('], formal_arguments(A), [')'], body(B), { P = procedure(PName, A, B) }.

formal_arguments(A) -->
  formal_argument(R), ([','], !, formal_arguments(As), { A = [R | As] } ;
  { A = [R] }).
formal_arguments([]) --> [].


formal_argument(A) -->
  variable(A), !;
  [tokNumber(N)], { A = value(N) }, !;
  [value], variable(V), { A = value(V) }.

compound_statement(S) --> statement(T), ([;], !, compound_statement(R), { S = [T | R] };
    { S = [T] }).

procedure_call(E) -->
  procedure_name(PName), ['('], factualArguments(A), [')'], { E = procedure_call(PName, A) }.

statement(S) -->
  [tokVar(V), :=], !, arith_expr(Expr), { S = assign(tokVar(V), Expr) } ;
  [if], !, bool_expr(Bool), [then], compound_statement(IfBody), (
    [else], !, compound_statement(ElseBody), [fi], { S = if(Bool, IfBody, ElseBody) };
    [fi], { S = if(Bool, IfBody) }
  );
  [while], !, bool_expr(Bool), [do], compound_statement(Body), [done], { S = while(Bool, Body) };
  [call], !, procedure_call(S);
  [return], !, arith_expr(Expr), { S = return(Expr) };
  [read], !, variable(V), { S = read(V) };
  [write], !, arith_expr(Expr), { S = write(Expr) }.

factualArguments(A) -->
    factualArgument(H), ([','], !, factualArguments(R), { A = [H | R] } ;
    { A = [H] }, ! ) ;
    { A = [] }.
factualArgument(A) -->
    arith_expr(A).

arith_expr(Expr) --> summand(Summand), arith_expr(Summand, Expr).
arith_expr(Acc, Expr) --> add_op(Op), !, summand(Summand), { Acc1 = [Op, Acc, Summand] }, arith_expr(Acc1, Expr).
arith_expr(Acc, Acc) --> [].

summand(Expr) --> factor(Factor), summand(Factor, Expr).
summand(Acc, Expr) --> mult_op(Op), !, factor(Factor), { Acc1 = [Op, Acc, Factor] }, summand(Acc1, Expr).
summand(Acc, Acc) --> [].

factor(Expr) -->
  [-], pos_factor(Ex), { Expr = minus(Ex) }, !;
  pos_factor(Expr).
pos_factor(Expr) -->
    (
	['('], !, arith_expr(Expr), [')'];
	[tokNumber(N)], !, { Expr = constant(N) };
  procedure_call(Expr), ! ;
	[tokVar(Var)], { Expr = variable(Var) }
    ).


bool_expr(Bool) --> disjunct(Disjunct), bool_expr(Disjunct, Bool).
bool_expr(Acc, Bool) --> [or], !, disjunct(Disjunct), { Acc1 = [or, Acc, Disjunct] }, bool_expr(Acc1, Bool).
bool_expr(Acc, Acc) --> [].

disjunct(Disjunct) --> conjunct(Conjunct), disjunct(Conjunct, Disjunct).
disjunct(Acc, Disjunct) --> [and], !, conjunct(Conjunct), { Acc1 = [and, Acc, Conjunct] }, disjunct(Acc1, Disjunct).
disjunct(Acc, Acc) --> [].

conjunct(Conjunct) -->
  [not], !, pos_conjunct(C), { Conjunct = not(C) } ;
  pos_conjunct(Conjunct).
pos_conjunct(Conjunct) -->
    (
    arith_expr(LExpr), !, rel_op(Op), arith_expr(RExpr), { Conjunct = [Op, LExpr, RExpr] };
    ['('], !, bool_expr(Conjunct), [')']
   ).

mult_op(*) -->
  [*], !.
mult_op(div) -->
  [div], !.
mult_op(mod) -->
  [mod], !.

add_op(+) -->
  [+], !.
add_op(-) -->
  [-], !.

rel_op(<=) -->
  [<=], !.
rel_op(>=) -->
  [>=], !.
rel_op(<) -->
  [<], !.
rel_op(>) -->
  [>], !.
rel_op(<>) -->
  [<>], !.
rel_op(=) -->
  [=].

% End of Parser
parse(CharCodeList, Absynt) :-
    phrase(tokens(TokList), CharCodeList),
    phrase(program(Absynt), TokList).

% Konertowanie linii na liczbę
to_decimal(line(_, [V]), V) :- integer(V), V >= 0, !.
to_decimal(line(_, [V]), VI) :- integer(V), V < 0, !, VI is V + 65536.
to_decimal(line(_, [A, B, C, D]), N) :-
  command(A, AC),
  command(B, BC),
  command(C, CC),
  command(D, DC), !,
  N is DC + CC * 16 + BC * 256 + AC * 4096.

command([78, 79, 80], 0).
command([83, 89, 83, 67, 65, 76, 76], 1).
command([76, 79, 65, 68], 2).
command([83, 84, 79, 82, 69], 3).
command([83, 87, 65, 80, 65], 4).
command([83, 87, 65, 80, 68], 5).
command([66, 82, 65, 78, 67, 72, 90], 6).
command([66, 82, 65, 78, 67, 72, 78], 7).
command([74, 85, 77, 80], 8).
command([67, 79, 78, 83, 84], 9).
command([65, 68, 68], 10).
command([83, 85, 66], 11).
command([77, 85, 76], 12).
command([68, 73, 86], 13).
command([83, 72, 73, 70, 84], 14).
command([78, 65, 78, 68], 15).

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
  phrase(instructions(Ins, StackB, Dict), Commands, ["CONST", 0, "SYSCALL"]),
  to_lines(Commands, Lines),
  maplist(to_decimal, Lines, Ret).

take_n(L, C, L, []) :- length(L, O), O < C, !.
take_n(R, 0, [], R) :- !.
take_n([H | T1], C, [H | T2], R) :-
  DC is C - 1,
  take_n(T1, DC, T2, R).

count(X, L, N) :-
  count(L, X, 0, N), !.
count([], _, C, C) :- !.
count([H | T], E, C, N) :-
  not(H \= E),
  !, C1 is C+1,
  count(T, E, C1, N).
count([_ | T], E, C, N) :-
  count(T, E, C, N).

add_integers([], Ret, Ret, NC, NC) :- !.
add_integers([H | T], Ret - Nend, Ret - NNend, C, NC) :-
  Line = line(C, [H]),
  Nend = [Line | X],
  C1 is C + 1,
  !, add_integers(T, Ret - X, Ret - NNend, C1, NC).

new_list(_, N, []) :- 0 is N, !.
new_list(E, C, [E | T]) :-
  DC is C - 1,
  !, new_list(E, DC, T).

to_lines(Commands, Ret) :-
  to_lines(Commands, Ret - Ret, [], [], 0), !.
to_lines([], _ - [], [], [], _) :- !.
to_lines([], Ret - End, AccC, AccI, C) :-
  !, length(AccC, I),
  (I > 0 -> new_list([78,79,80], 4 - I, O),
  append(AccC, O, NO),
  T = [line(C, NO) | Nend],
  End = T,
  C1 is C + 1; C1 = C, Nend = End),
  !, add_integers(AccI, Ret - Nend, Ret - NNend, C1, _),
  to_lines([], Ret - NNend, [], [], C1).
to_lines([H | T], Ret - E, AccC, AccI, C) :-
  (integer(H) ; var(H)), !,
  (AccC = [] -> !, E = [line(C, [H]) | X], C1 is C + 1, to_lines(T, Ret - X, AccC, AccI, C1);
  (append(AccI, [H], NAccI),
    to_lines(T, Ret - E, AccC, NAccI, C))).
to_lines(["JUMP" | T], Ret - End, AccC, AccI, C) :-
  !, append(AccC, ["JUMP"], NAccC), length(NAccC, I),
  new_list([78,79,80], 4 - I, O),
  append(NAccC, O, NO),
  End = [line(C, NO) | Nend],
  C1 is C + 1,
  add_integers(AccI, Ret - Nend, Ret - NNend, C1, H),
  to_lines(T, Ret - NNend, [], [], H).
to_lines([marker(H) | T], Ret - End, AccC, AccI, C) :-
  !, length(AccC, I),
  (I > 0 -> new_list([78,79,80], 4 - I, O),
  append(AccC, O, NO),
  End = [line(C, NO) | Nend],
  C1 is C + 1; C1 = C, Nend = End),
  add_integers(AccI, Ret - Nend, Ret - NNend, C1, H),
  to_lines(T, Ret - NNend, [], [], H).
to_lines([H | T], Ret - End, AccC, AccI, C) :-
  !, append(AccC, [H], NAccC),
  (length(NAccC, 4) -> !, End = [line(C, NAccC) | Nend], count([67, 79, 78, 83, 84], NAccC, O), !, C1 is C + 1,
    take_n(AccI, O, LAccI, NAccI), !, add_integers(LAccI, Ret - Nend, Ret - NNend, C1, NC1),
    to_lines(T, Ret - NNend, [], NAccI, NC1) ;
    to_lines(T, Ret - End, NAccC, AccI, C)).


instructions([], _, _) --> [].
instructions([read(V) | T], S, Dict) -->
  !, {member((V, Addr), Dict)}, !,
  ["CONST", Addr, "SWAPA", "CONST", 1, "SYSCALL", "STORE"], instructions(T, S, Dict).
instructions([write(Arith_Expr) | T], S, Dict) -->
  !, translateArithExpr(Arith_Expr, S, Dict), ["SWAPD", "CONST", 2, "SYSCALL"], instructions(T, S, Dict).
instructions([assign(tokVar(Name), Arith_Expr) | T], S, Dict) -->
  !, translateArithExpr(Arith_Expr, S, Dict), { member((variable(Name), Addr), Dict), ! },
  ["SWAPA", "CONST", Addr, "SWAPA", "STORE"], instructions(T, S, Dict).
instructions([if(BoolExpr, Body) | T], S, Dict) -->
  !, translateBoolExpr(BoolExpr, S, Dict), ["SWAPA", "CONST", No, "SWAPA", "BRANCHZ"],
  instructions(Body, S, Dict),
  [marker(No)],
  instructions(T, S, Dict).
instructions([if(BoolExpr, TrueBody, FalseBody) | T], S, Dict) -->
  !, translateBoolExpr(BoolExpr, S, Dict), ["SWAPA", "CONST", No, "SWAPA", "BRANCHZ"],
  instructions(TrueBody, S, Dict),
  ["CONST", True, "JUMP"],
  [marker(No)],
  instructions(FalseBody, S, Dict),
  [marker(True)],
  instructions(T, S, Dict).
instructions([while(BoolExpr, Body) | T], S, Dict) -->
  [marker(Bool)],
  translateBoolExpr(BoolExpr, S, Dict), ["SWAPA", "CONST", End, "SWAPA", "BRANCHZ"],
  instructions(Body, S, Dict),
  ["CONST", Bool, "JUMP"], % Skocz do warunku
  [marker(End)], % Zakończ pętle
  instructions(T, S, Dict).

translateArithExpr(minus(constant(X)), _, _) -->
  !, {NegX is -X}, ["CONST", NegX].
translateArithExpr(minus(X), S, D) -->
  !, translateArithExpr(X, S, D), ["SWAPD", "CONST", -1, "MUL"].
translateArithExpr(constant(X), _, _) -->
  !, ["CONST", X].
translateArithExpr(tokVar(X), _, Dict) -->
  !, { member((variable(X), Addr), Dict), ! }, ["CONST", Addr, "SWAPA", "LOAD"].
translateArithExpr(variable(X), _, Dict) -->
  !, { member((variable(X), Addr), Dict), ! }, ["CONST", Addr, "SWAPA", "LOAD"].
translateArithExpr([+, L, R], S, Dict) -->
  !, translateComplexArithExpr(L, R, S, Dict),
  ["ADD"].
translateArithExpr([-, L, R], S, Dict) -->
  !, translateComplexArithExpr(L, R, S, Dict),
  ["SUB"].
translateArithExpr([*, L, R], S, Dict) -->
  !, translateComplexArithExpr(L, R, S, Dict),
  ["MUL"].
translateArithExpr(['div', L, R], S, Dict) -->
  !, translateComplexArithExpr(L, R, S, Dict),
  ["DIV"].
translateArithExpr(['mod', L, R], S, Dict) -->
  !, translateComplexArithExpr(L, R, S, Dict),
  ["DIV", "SWAPD", "CONST", -16, "SWAPD", "SHIFT"].

translateComplexArithExpr(L, R, S, Dict) -->
  !, translateArithExpr(L, S, Dict), ["SWAPA", "CONST", S, "SWAPA", "STORE"], % L na stos
  { S1 is S - 1 },
  translateArithExpr(R, S1, Dict), ["SWAPD"], % R do argumentu
  ["CONST", S, "SWAPA", "LOAD"]. % L do Acc



translateBoolExpr([=, L, R], S, Dict) -->
  !, translateBoolExpr(not([<>, L, R]), S, Dict).
translateBoolExpr([<>, L, R], S, Dict) -->
  !, translateArithExpr([-, L, R], S, Dict),
  ["SWAPA", "CONST", Jump0, "SWAPA", "BRANCHZ", "CONST", 1, marker(Jump0)].
translateBoolExpr([<, L, R], S, Dict) -->
  !, translateArithExpr(L, S, Dict),
  ["SWAPA", "CONST", S, "SWAPA", "STORE"],
  { SD is S - 1 },
  translateArithExpr(R, SD, Dict),
  ["SWAPA", "CONST", SD, "SWAPA", "STORE"],
  ["SWAPA", "CONST", RNeg, "SWAPA", "BRANCHN"],
  ["CONST", S, "SWAPA", "LOAD", "SWAPA", "CONST", LNeg, "SWAPA", "BRANCHN"],
  ["CONST", Both, "JUMP"], % Obie nieujemne
  [marker(LNeg), "CONST", 1, "SWAPA", "CONST", End, "JUMP"], % Lewa ujemna
  [marker(RNeg), "CONST", S, "SWAPA", "LOAD", "SWAPA", "CONST", Both, "SWAPA", "BRANCHN"],
  ["CONST", 0, "SWAPA", "CONST", End, "JUMP"], % Prawa ujemna
  [marker(Both), "CONST", SD, "SWAPA", "LOAD", "SWAPD", "CONST", S, "SWAPA", "LOAD", "SUB"],
  ["SWAPA", "CONST", Jump0, "SWAPA", "BRANCHN", "CONST", 0, "SWAPA", "CONST", Jump1, "JUMP", marker(Jump0), "CONST", 1, "SWAPA", marker(Jump1)],
  [marker(End), "SWAPA"].
translateBoolExpr([>, L, R], S, Dict) -->
  !, translateBoolExpr([<, R, L], S, Dict).
translateBoolExpr([>=, L, R], S, Dict) -->
  !, translateBoolExpr(not([<, L, R]), S, Dict).
translateBoolExpr([<=, L, R], S, Dict) -->
  !, translateBoolExpr([>=, R, L], S, Dict).

translateBoolExpr(([and, L, R]), S, Dict) -->
  !, complexBoolExpr(L, R, S, Dict), ["MUL"].
translateBoolExpr(([or, L, R]), S, Dict) -->
  !, complexBoolExpr(L, R, S, Dict), ["ADD", "SWAPD", "CONST", 1, "ADD", "SWAPD", "CONST", 2, "SWAPD", "DIV"].
translateBoolExpr(not(L), S, Dict) -->
  !, translateBoolExpr(L, S, Dict),
  ["SWAPD", "CONST", 1, "SUB"].

complexBoolExpr(L, R, S, Dict) -->
  !, translateBoolExpr(L, S, Dict),
  ["SWAPA", "CONST", S, "SWAPA", "STORE"], % L na stos
  { S1 is S - 1 },
  translateBoolExpr(R, S1, Dict),
  ["SWAPD"],
  ["CONST", S, "SWAPA", "LOAD"].

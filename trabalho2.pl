
%----------------------------------------------------------------------
% Trabalho 02 - Projeto do capítulo 17 do Bratko
% IEC034 - Inteligência Artificial - 2021/1: ano 2022
% Turma: EC01
% Equipe:
%        Fabrício da Costa - 21950515
%        Laura Aguiar      - 21952064
%        Lorena Basto      - 21952638
%----------------------------------------------------------------------

%----------------------------------------------------------------------
% Descrição do mundo dos blocos
%----------------------------------------------------------------------

block(a).
block(b).
block(c).
block(d).

place(1).
place(2).
place(3).
place(4).
place(5).
place(6).

tam(a,1).
tam(b,1).
tam(c,2).
tam(d,3).


%----------------------------------------------------------------------
% Estado Inicial
%----------------------------------------------------------------------

%
%             d d d       
%       c c   a   b
%       = = = = = =
% place 1 2 3 4 5 6


%----------------------------------------------------------------------
% Meta/Goal
%----------------------------------------------------------------------

%               a b
%               c c       
%             d d d
%       = = = = = =
% place 1 2 3 4 5 6


%----------------------------------------------------------------------
% plan(InitialState, Goals, Plan)
%----------------------------------------------------------------------
plan(State, Goals, []) :-
    satisfied(State, Goals).                        % Goals verdadeiros no estado

plan(State, Goals, Plan) :-
    conc(PrePlan, [Action], Plan),                  % Divide o plano
    select(State, Goals, Goal),                     % Seleciona goal
    achieves(Action, Goal),  
    can(Action, Condition),                         % Verifica se a ação não contém variáveis
    preserves(Action, Goals),                       % Protege o goal
    regress(Goals, Action, RegressedGoals),         % Faz Goal Regression por meio do Action
    plan(State, RegressedGoals, PrePlan).

satisfied(State, Goals) :-
    delete_all(Goals, State, []).                   % Todos os Goals no estado

select(State, Goals, Goal) :-                       % Seleciona goal do Goals
    member(Goal, Goals).                            % Seleção simples

achieves(Action, Goal) :-
    adds(Action, Goals),
    member(Goal, Goals).

preserves(Action, Goals) :-                         % Action não destroi os goals            
    deletes(Action, Relations),
    \+ ((member(Goal, Relations), member(Goal, Goals))).

regress(Goals, Action, RegressedGoals) :-           % Faz Goal Regression por meio do Action
    adds(Action, NewRelations),
    delete_all(Goals, NewRelations, RestGoals),
    can(Action, Condition),
    addnew(Condition, RestGoals, RegressedGoals).   % Adiciona uma pré-condição e verifica a impossibilidade


%----------------------------------------------------------------------
% addnew( NewGoals, OldGoals, AllGoals):
%   AllGoal é a união dos NewGoals com os OldGoals
%   NewGoals e Oldgoals tem que ser compatíveis
%----------------------------------------------------------------------
addnew([], L, L).
addnew([Goal|_], Goals, _) :-
    impossible(Goal, Goals),                        % Goal imcompatível com os Goals
    !,
    fail.                                           % Não pode adicionar        
addnew([X|L1], L2, L3) :-
    member(X, L2), !,
    addnew(L1,L2,L3).                               % Ignora duplicados

addnew([X|L1],L2,[X|L3]) :-
    addnew(L1,L2,L3).


%----------------------------------------------------------------------
% delete_all(L1, L2, Diff): Diff é a diferença de conjunto das listas L1 e L2
%----------------------------------------------------------------------
delete_all([], _, []).
delete_all([X|L1], L2, Diff) :-
    member(X, L2), !,
    delete_all(L1,L2,Diff).
delete_all([X|L1], L2, [X|Diff]) :-
    delete_all(L1,L2,Diff).

conc( [], L, L).
conc( [X| L1], L2, [X| L3]) :-
    conc( L1, L2, L3).

can(move(Block, From, To), [clear(Block), clear(To), on(Block, From)]) :-
    block(Block), 
    object(To), 
    To \== Block, 
    object(From),
    From \== To, 
    Block \== From.

impossible(on(X,X), _). 
impossible(on(X,Y), Goals) :-
    member(clear(Y), Goals) 
    ;
    member(on(X,Y1),Goals), Y1 \== Y 
    ;
    member(on(X1,Y), Goals), X1 \== X.
impossible(clear(X), Goals) :-
    member(on(_, X), Goals). 

adds(move(X,From,To), [on(X,To), clear(From)]).

deletes(move(X,From,To), [on(X,From), clear(To)]).
object(X) :-
    place(X)
    ;
    block(X).


% Estado inicial
state([clear(3), clear(c),clear(d), on(c,1), on(a,4), on(b,6), on(d,a), on(d,b)]).   

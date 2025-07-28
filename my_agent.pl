% my_agent.pl

:- dynamic(agent_state/4).

% agent_state(X, Y, Orientation, HasGold)
% Orientation: 0 = direita, 90 = cima, 180 = esquerda, 270 = baixo

init_agent :-
    retractall(agent_state(_,_,_,_)),
    assert(agent_state(1, 1, 0, false)).

restart_agent :-
    retractall(agent_state(_,_,_,_)),
    assert(agent_state(1, 1, 0, false)).

% Main interface
my_agent(Percept, Action) :-
    decide_action(Percept, Action),
    format("Executing ~w~n", [Action]),
    execute(Action, _),
    update_agent_state(Action, Percept),
    print_agent_status.

% Decide action based on percept and internal state
decide_action([Stench, Breeze, Glitter, Bump, _Scream], Action) :-
    agent_state(X, Y, Orient, HasGold),
    (
        Glitter == yes ->
            Action = grab
        ;
        HasGold == true,
        X == 1, Y == 1 ->
            Action = climb
        ;
        Bump == yes ->
            Action = turnright
        ;
        (Breeze == yes ; Stench == yes) ->
            Action = turnleft
        ;
            Action = goforward
    ).

% Update internal state after action
update_agent_state(Action, [_, _, _, Bump, _]) :-
    agent_state(X, Y, Orient, HasGold),
    (
        Action == goforward ->
            (Bump == yes ->
                true  % don't move if bump
            ;
                move_forward(X, Y, Orient, NewX, NewY),
                retract(agent_state(_,_,_,_)),
                assert(agent_state(NewX, NewY, Orient, HasGold))
            )
        ;
        Action == turnleft ->
            NewOrient is (Orient + 90) mod 360,
            retract(agent_state(_,_,_,_)),
            assert(agent_state(X, Y, NewOrient, HasGold))
        ;
        Action == turnright ->
            NewOrient is (Orient + 270) mod 360,
            retract(agent_state(_,_,_,_)),
            assert(agent_state(X, Y, NewOrient, HasGold))
        ;
        Action == grab ->
            retract(agent_state(_,_,_,_)),
            assert(agent_state(X, Y, Orient, true))
        ;
        true
    ).

% Compute new position when moving forward
move_forward(X, Y, Orient, NewX, NewY) :-
    ( Orient =:= 0   -> NewX is X + 1, NewY is Y
    ; Orient =:= 90  -> NewX is X,     NewY is Y + 1
    ; Orient =:= 180 -> NewX is X - 1, NewY is Y
    ; Orient =:= 270 -> NewX is X,     NewY is Y - 1
    ).

% Print agent status
print_agent_status :-
    agent_orientation(O), format("agent_orientation(~w)~n", [O]),
    agent_health(H), format("agent_health(~w)~n", [H]),
    agent_arrows(A), format("agent_arrows(~w)~n", [A]),
    agent_gold(G), format("agent_gold(~w)~n~n", [G]).

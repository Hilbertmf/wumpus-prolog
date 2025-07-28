:- [wumpus].
:- [navigate].
:- [my_agent].

test_agent :-
    tell('test.txt'),
    run_all_trials('random_worlds.pl', Scores, Times),
    summarize_results(Scores, Times),
    told.

% Lê os mundos do arquivo e executa testes
run_all_trials(File, Scores, Times) :-
    exists_file(File), !,
    open(File, read, Stream),
    run_trials(Stream, 1, [], [], Scores, Times),
    close(Stream).

run_trials(Stream, TrialNum, AccScores, AccTimes, Scores, Times) :-
    read(Stream, Fact),
    ( Fact == end_of_file ->
        reverse(AccScores, Scores),
        reverse(AccTimes, Times)
    ;
        format("Trial ~d~n~n", [TrialNum]),
        format('Executing initialize~n~n', []),
        flush_output,
        once(evaluate_trial(Fact, Actions, Score, Time)),
        format("I am outta here.~n", []),
        format("  Actions = ~w~n", [Actions]),
        format("****************~n~n", []),
        TrialNum1 is TrialNum + 1,
        run_trials(Stream, TrialNum1, [Score|AccScores], [Time|AccTimes], Scores, Times)
    ).

evaluate_trial(Fact, Actions, Score, Time) :-
    statistics(runtime, [Start|_]),
    navigate(Fact, Actions, Score, UsedTime),
    statistics(runtime, [Finish|_]),
    Time is Finish - Start + UsedTime.

summarize_results(Scores, Times) :-
    length(Scores, N),
    mean(Scores, MS), sd(Scores, SS),
    mean(Times, MT), sd(Times, ST),
    format("Tested: ~d~n", [N]),
    format("Score: ~2f[~2f]~n", [MS, SS]),
    format("Time: ~2f[~2f]~n", [MT / 1000, ST / 1000]),  % convert ms to sec
    format("****************~n", []).

mean(List, Mean) :-
    sum_list(List, Sum),
    length(List, Len),
    Len > 0, Mean is Sum / Len.

sd(List, SD) :-
    mean(List, Mean),
    length(List, Len),
    Len > 1,
    sumsq(List, SumSq),
    SD is sqrt((SumSq - Len * Mean * Mean) / (Len - 1)).

sumsq([], 0).
sumsq([X|Xs], SumSq) :-
    sumsq(Xs, Rest),
    SumSq is X * X + Rest.

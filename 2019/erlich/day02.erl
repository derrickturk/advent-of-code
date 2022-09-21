-module(day02).
-import(vm, [from_file/1, peek/2, poke/3, run/1]).
-export([main/1, problem1/1, problem2/1]).

main(Input) ->
    Vm = from_file(Input),
    problem1(Vm),
    problem2(Vm).

problem1(Vm) ->
    NewVm = poke(2, 2, poke(1, 12, Vm)),
    {FinalVm, []} = run(NewVm),
    io:format("problem 1: ~w~n", [peek(0, FinalVm)]).

problem2(Vm) ->
    io:format("problem 2: ~w~n", [go(Vm, 0, 0)]).

go(_, 100, _) ->
    error(no_solution);
go(Vm, Noun, 100) ->
    go(Vm, Noun + 1, 0);
go(Vm, Noun, Verb) ->
    NewVm = poke(2, Verb, poke(1, Noun, Vm)),
    {FinalVm, []} = run(NewVm),
    Result = peek(0, FinalVm),
    if
        Result == 19690720 ->
            100 * Noun + Verb;
        true ->
            go(Vm, Noun, Verb + 1)
    end.

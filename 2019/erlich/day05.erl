-module(day05).
-import(vm, [from_file/1, run/2]).
-export([main/1, problem1/1, problem2/1]).

main(Input) ->
    Vm = from_file(Input),
    problem1(Vm),
    problem2(Vm).

problem1(Vm) ->
    {_, Outputs} = run(Vm, [1]),
    io:format("problem 1: ~w~n", [Outputs]).

problem2(Vm) ->
    {_, Outputs} = run(Vm, [5]),
    io:format("problem 2: ~w~n", [Outputs]).

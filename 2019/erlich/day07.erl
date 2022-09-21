-module(day07).
-import(vm, [from_file/1, listen/1]).
-export([main/1, problem1/1, problem2/1, thrust1/2]).

main(Input) ->
    Vm = from_file(Input),
    problem1(Vm),
    problem2(Vm).

problem1(Vm) ->
    lists:max(
      lists:map(
        fun(Ps) -> thrust1(Vm, Ps) end,
        permutations([0, 1, 2, 3, 4]))).

problem2(Vm) ->
    io:format("problem 2: ~w~n", [permutations([5,6,7,8,9])]).

thrust1(Vm, Phases) ->
    Vms = [First | _] = spawn_cpus(Vm, Phases),
    Last = plumb_cpus_linear(Vms),
    Self = self(),
    Listener = spawn(fun() -> listen_last(Self) end),
    Last ! {subscribe, Listener},
    First ! {send, 0},
    lists:foreach(fun(V) -> V ! run end, Vms),
    receive
        Word ->
            lists:foreach(fun(V) -> V ! stop end, Vms),
            Word
    end.

listen_last(Tell) ->
    listen_last(Tell, nil).

listen_last(Tell, Prev) ->
    receive
        {send, Word} ->
            listen_last(Tell, Word);
        halt ->
            Tell ! Prev
    end.

spawn_cpus(Vm, Phases) ->
    lists:map(fun(Phase) ->
        Pid = spawn(vm, listen, [Vm]),
        Pid ! {send, Phase},
        Pid
    end, Phases).

plumb_cpus_linear([Vm | Vms]) ->
    plumb_cpus_linear(Vm, Vms).

plumb_cpus_linear(Last, []) ->
    Last;
plumb_cpus_linear(Src, [First | Rest]) ->
    Src ! {subscribe, First},
    plumb_cpus_linear(First, Rest).

permutations([]) ->
    [[]];
permutations(Xs) ->
    [[H|T] || H <- Xs, T <- permutations(Xs -- [H])].

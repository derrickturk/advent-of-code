-module(vm).
-export([
    new/0,
    new/1,
    from_file/1,
    peek/2,
    poke/3,
    decode/1,
    step/1,
    listen/1,
    run/1
]).
-include("erlich.hrl").

new() ->
    #vm{}.

new(Mem) when is_list(Mem) ->
    #vm{mem=array:from_list(Mem)};
new(Mem) ->
    #vm{mem=Mem}.

from_file(Path) ->
    {ok, Txt} = file:read_file(Path),
    Data = lists:map(fun(Word) ->
        {Val, _} = string:to_integer(Word),
        Val
      end,
      binary:split(Txt, <<",">>, [global])
    ),
    new(Data).

peek(Ptr, Vm) ->
    array:get(Ptr, Vm#vm.mem).

poke(Ptr, Word, Vm) ->
    Vm#vm{mem=array:set(Ptr, Word, Vm#vm.mem)}.

decode(Vm = #vm{ip=Ip}) ->
    Word = peek(Ip, Vm),
    Op = Word rem 100,
    Mode1 = decode_mode(Word div 100 rem 10),
    Mode2 = decode_mode(Word div 1000 rem 10),
    Mode3 = decode_mode(Word div 10000 rem 10),
    case Op of
        1 -> {
            add,
            {Mode1, peek(Ip + 1, Vm)},
            {Mode2, peek(Ip + 2, Vm)},
            {Mode3, peek(Ip + 3, Vm)}
        };
        2 -> {
            mul,
            {Mode1, peek(Ip + 1, Vm)},
            {Mode2, peek(Ip + 2, Vm)},
            {Mode3, peek(Ip + 3, Vm)}
        };
        3 -> {
            inp,
            {Mode1, peek(Ip + 1, Vm)}
        };
        4 -> {
            out,
            {Mode1, peek(Ip + 1, Vm)}
        };
        5 -> {
            jnz,
            {Mode1, peek(Ip + 1, Vm)},
            {Mode2, peek(Ip + 2, Vm)}
        };
        6 -> {
            jz,
            {Mode1, peek(Ip + 1, Vm)},
            {Mode2, peek(Ip + 2, Vm)}
        };
        7 -> {
            lt,
            {Mode1, peek(Ip + 1, Vm)},
            {Mode2, peek(Ip + 2, Vm)},
            {Mode3, peek(Ip + 3, Vm)}
        };
        8 -> {
            eq,
            {Mode1, peek(Ip + 1, Vm)},
            {Mode2, peek(Ip + 2, Vm)},
            {Mode3, peek(Ip + 3, Vm)}
        };
        9 -> {
            arb,
            {Mode1, peek(Ip + 1, Vm)}
        };
        99 -> {hlt}
    end.

step(Vm) ->
    step(Vm, []).

step(Vm = #vm{ip=Ip, rb=Rb}, Subs) ->
    case decode(Vm) of
        {add, Lhs, Rhs, Dst} ->
            NewVm = write_mode(
              Dst,
              read_mode(Lhs, Vm) + read_mode(Rhs, Vm),
              Vm
            ),
            NewVm#vm{ip = Ip + 4};
        {mul, Lhs, Rhs, Dst} ->
            NewVm = write_mode(
              Dst,
              read_mode(Lhs, Vm) * read_mode(Rhs, Vm),
              Vm
            ),
            NewVm#vm{ip = Ip + 4};
        {inp, Dst} ->
            Cont = fun(Word) ->
                NewVm = write_mode(Dst, Word, Vm),
                NewVm#vm{ip = Ip + 2}
            end,
            {Cont, wait};
        {out, Src} ->
            lists:foreach(fun(S) -> S ! read_mode(Src, Vm) end, Subs),
            Vm#vm{ip = Ip = 2};
        {jnz, Cnd, Tgt} ->
            Cnd = read_mode(Cnd, Vm),
            Tgt = read_mode(Tgt, Vm),
            NewIp = if
                Cnd /= 0 -> Tgt;
                true -> Ip + 3
            end,
            Vm#vm{ip = NewIp};
        {jz, Cnd, Tgt} ->
            Cnd = read_mode(Cnd, Vm),
            Tgt = read_mode(Tgt, Vm),
            NewIp = if
                Cnd == 0 -> Tgt;
                true -> Ip + 3
            end,
            Vm#vm{ip = NewIp};
        {lt, Lhs, Rhs, Dst} ->
            Cnd = read_mode(Lhs, Vm) < read_mode(Rhs, Vm),
            NewVm = write_mode(
              Dst,
              if
                  Cnd -> 1;
                  true -> 0
              end,
              Vm
            ),
            NewVm#vm{ip = Ip + 4};
        {eq, Lhs, Rhs, Dst} ->
            Cnd = read_mode(Lhs, Vm) == read_mode(Rhs, Vm),
            NewVm = write_mode(
              Dst,
              if
                  Cnd -> 1;
                  true -> 0
              end,
              Vm
            ),
            NewVm#vm{ip = Ip + 4};
        {arb, off} ->
            Vm#vm{ip = Ip + 2, rb = Rb + read_mode(off, Vm)};
        {hlt} -> {Vm, halt}
    end.

listen(Vm) ->
    listen(Vm, [], break).

listen(Vm, Subs) ->
    listen(Vm, Subs, break).

listen(Vm, Subs, break) ->
    receive
        run ->
            listen(Vm, Subs, run);
        break ->
            listen(Vm, Subs, break);
        {subscribe, Sub} ->
            listen(Vm, [Sub | Subs], break);
        {unsubscribe, Sub} ->
            NewSubs = lists:filter(fun(S) -> S /= Sub end, Subs),
            listen(Vm, NewSubs, break);
        {send, Word} ->
            self() ! {send, Word},
            listen(Vm, break);
        {query, Whom} ->
            Whom ! {Vm, break},
            listen(Vm, break)
    end;
listen(Vm, Subs, {wait, Cont}) ->
    receive
        run ->
            listen(Vm, Subs, {wait, Cont});
        break ->
            listen(Vm, Subs, break);
        {subscribe, Sub} ->
            listen(Vm, [Sub | Subs], {wait, Cont});
        {unsubscribe, Sub} ->
            NewSubs = lists:filter(fun(S) -> S /= Sub end, Subs),
            listen(Vm, NewSubs, {wait, Cont});
        {send, Word} ->
            listen(Cont(Word), run);
        {query, Whom} ->
            Whom ! {Vm, wait},
            listen(Vm, Subs, {wait, Cont})
    end;
listen(Vm, Subs, run) ->
    case step(Vm, Subs) of
        {Cont, wait} ->
            listen(Vm, Subs, {wait, Cont});
        {NewVm, halt} ->
            lists:foreach(fun(S) -> S ! halt end, Subs),
            listen(NewVm, [], halt);
        NewVm ->
            listen(NewVm, Subs, run)
    end;
listen(Vm, _, halt) ->
    receive
        run ->
            listen(Vm, [], halt);
        break ->
            listen(Vm, [], halt);
        {subscribe, _} ->
            listen(Vm, [], halt);
        {unsubscribe, _} ->
            listen(Vm, [], halt);
        {send, _} ->
            listen(Vm, [], halt);
        {query, Whom} ->
            Whom ! {Vm, halt},
            listen(Vm, [], halt)
    end.

run(Vm) ->
    run(Vm, []).

run(Vm, Inputs) ->
    VmPID = spawn(vm, listen, [Vm]),
    lists:foreach(fun(W) -> VmPID ! {send, W} end, Inputs),
    VmPID ! {subscribe, self()},
    VmPID ! run,
    vm_wait(VmPID, []).

vm_wait(VmPID, Outputs) ->
    receive
        halt ->
            VmPID ! {query, self()},
            receive
                {Vm, halt} -> {Vm, lists:reverse(Outputs)}
            end;
        {send, Word} ->
            vm_wait(VmPID, [Word | Outputs])
    end.

decode_mode(0) ->
    mem;
decode_mode(1) ->
    imm;
decode_mode(2) ->
    rel.

read_mode({mem, Ptr}, Vm) ->
    peek(Ptr, Vm);
read_mode({imm, Ptr}, _) ->
    Ptr;
read_mode({rel, Ptr}, Vm = #vm{rb=Rb}) ->
    peek(Rb + Ptr, Vm).

write_mode({mem, Ptr}, Word, Vm) ->
    poke(Ptr, Word, Vm);
write_mode({rel, Ptr}, Word, Vm = #vm{rb=Rb}) ->
    poke(Rb + Ptr, Word, Vm).

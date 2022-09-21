-module(vm).
-export([
    new/0,
    new/1,
    from_file/1,
    peek/2,
    poke/3,
    decode/1,
    step/1,
    run/1
]).
-include("erlic.hrl").

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

step(Vm = #vm{ip=Ip, rb=Rb}) ->
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
        {inp, Src} ->
            io:format("input here"),
            Vm#vm{ip = Ip + 2};
        {out, Src} ->
            io:format("output here"),
            Vm#vm{ip = Ip + 2};
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
        {hlt} -> {Vm, hlt}
    end.

run({Vm}) ->
    run(step(Vm));
run({Vm, hlt}) ->
    Vm;
run(Vm) ->
    run({Vm}).

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

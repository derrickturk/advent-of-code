let Prelude = https://prelude.dhall-lang.org/v20.0.0/package.dhall
let input = ./input.dhall
let target = 2020
let cartesian = λ(a : Type) -> λ(xs : List a)
             -> λ(b : Type) -> λ(ys : List b)
             -> Prelude.List.concatMap a { _1 : a, _2 : b }
                  (λ(x : a) -> Prelude.List.map b { _1 : a, _2 : b }
                                 (λ(y : b) -> { _1 = x, _2 = y })
                                 ys)
                  xs
let ixedTy = { index : Natural, value : Natural }
let cartedTy = { _1 : ixedTy, _2 : ixedTy }
let sum2 = λ(xs : List Natural) ->
  let ixed = List/indexed Natural xs
  let carted = cartesian ixedTy ixed ixedTy ixed
  in Prelude.List.filter cartedTy
       (λ(x : cartedTy) ->
          (Prelude.Bool.not (Prelude.Natural.equal x._1.index x._2.index))
          && (Prelude.Natural.equal (x._1.value + x._2.value) target))
       carted
let winners = List/head cartedTy (sum2 input)
in Prelude.Optional.map cartedTy Natural
     (λ(w : cartedTy) -> w._1.value * w._2.value)
     winners

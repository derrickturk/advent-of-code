type 'a t = ('a, Error.t) result
let (let*) m f = Result.bind m f
let return x = Ok x

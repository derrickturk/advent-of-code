pub value struct queue<a>
  left: list<a>
  right: list<a>

pub fun queue(): queue<a>
  Queue(Nil, Nil)

pub fun pop(q: queue<a>): div maybe<(a, queue<a>)>
  match q
    Queue(Nil, Nil) -> Nothing
    Queue(Nil, r) -> Queue(r.reverse, Nil).pop
    Queue(Cons(x, xs), r) -> Just((x, Queue(xs, r)))

pub fun push(q: queue<a>, x: a): queue<a>
  q(right = Cons(x, q.right))

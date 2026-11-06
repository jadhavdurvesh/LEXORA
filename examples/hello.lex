let twice = fn f => fn x => f (f x) in
  twice (fn n => n + 1) 20

An expression evaluates to:
(head tail ...) => head [prepare tail ...]

A function inside the call stack evaluates to:
[prepare tail ...] => {tail parameterize} ... CALL POP-PARAMS

Pair (a b), source of a
-> Parameterize(src(a), ..)
  -> Call(src(a), ..)
    -> Library:Deparameterize(src(a), ..)
  -> Macro::Library(src(a), ..)
    -> Library:Deparameterize(src(a), ..)

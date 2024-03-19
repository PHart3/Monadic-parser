
(* A monadic parser for closed arithmetic powressions in a
 * signature extended with sub, div, pow, and additive 
 * inverse operations *)

open ParserMon.Parsing

let sat (pr : char -> bool) : char parse = 
  let* c = Pars (fun s -> match s with 
    | [] -> []
    | c::cs -> [(c,cs)])
  in
    if pr c then return c else zero

let rec str (s : stringL) : stringL parse = 
  match s with
    | [] -> return []
    | c::cs -> 
      let* _ = sat (fun x -> c = x) in
      let* _ = str cs in
        return s

let rec many (p : 'a parse) : ('a list) parse = 
  (many1 p) +++ (return [])
and many1 p =
  let* x = p in
  let* xs = many p in
    return (x::xs)

let manyFail (pa : 'a parse) : ('a list) parse =
  Pars ( fun s ->
    match parseApp (many pa) s with
      | [] -> []
      | c::cs -> 
        if List.length (fst c) > 0 then c::cs else [] ) 

let chainl (p1 : 'a parse) (p2 : ('a -> 'a -> 'a) parse) : 'a parse = 
  let rec rest x = (  
    let* f = p2 in
    let* y = p1 in
      (rest (f x y)) ) 
    +++ (return x)
  in
    let* v = p1 in
      rest v

let chainr (p1 : 'a parse) (p2 : ('a -> 'a -> 'a) parse) : 'a parse = 
  let rec rest z = ( 
    let* f = p2 in
    let* x = p1 in
    let* y = rest x in
      (return (f z y)) ) 
    +++ (return z)
  in
    let* v = p1 in
      rest v

exception Error of string

let digitConv (c : char) : float =
  if c = '0' then 0.0
  else if c = '1' then 1.0 
  else if c = '2' then 2.0  
  else if c = '3' then 3.0
  else if c = '4' then 4.0 
  else if c = '5' then 5.0
  else if c = '6' then 6.0 
  else if c = '7' then 7.0
  else if c = '8' then 8.0
  else if c = '9' then 9.0
  else raise (Error "Expected digit here") 

let isDigit = function '0' .. '9' ->  true | _ -> false

let numbP : float parse =
  let sumList xs = List.fold_right 
    (fun y (x, n) -> (x +. (digitConv y) *. (Float.pow 10.0 (float n)), n + 1)) xs (0.0, 0) in
      let* cs = manyFail (sat (fun c -> isDigit c)) in
        return (fst (sumList cs))  

let space : stringL parse = 
  many (sat (fun c -> c = ' ' || c = '\n' || c = '\t'))

let token (p : 'a parse) : 'a parse = 
  let* x = p in
  let* _ = space in
    return x

let symb (s : stringL) : stringL parse = 
  token (str s) 

let apply (pa : 'a parse) (s : stringL) : ('a * stringL) list =
  match (let* _ = space in pa) with
    | Pars p -> p s

let addop : (float -> float -> float) parse = 
  ( let* _ = symb ['+'] in
      return (+.) ) +++
  ( let* _ = symb  ['-'] in
      return (-.) )

let mulop : (float -> float -> float) parse = 
  ( let* _ = symb ['*'] in
      return Float.mul ) +++
  ( let* _ = symb  ['/'] in
      return (/.) ) 

let powop : (float -> float -> float) parse = 
  let* _ = str ['^'] in
    return Float.pow

let addinv (pa : float parse) : float parse =
  let* _ = sat (fun c -> c = '~') in
  let* n = pa in
    return (Float.neg n)

let rec expr : (float parse) Lazy.t =
  let minus = lazy ((addinv (Lazy.force factor)) +++ (Lazy.force factor)) in
    let strg = lazy (chainr (Lazy.force minus) powop) in 
      let term = lazy (chainl (Lazy.force strg) mulop) in
        lazy (chainl (Lazy.force term) addop)
and factor = lazy ( 
  (token numbP) +++ (
    let* _ = symb ['('] in
    let* n = Lazy.force expr in
    let* _ = symb [')'] in
      return n ) )
 

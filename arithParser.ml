
(* A monadic parser for closed arithmetic powressions in a
 * signature extended with sub, div, and pow operations *)

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

let rec pow (n : int) (m : int) : int =
  if m = 0 then 1
  else if m = 1 then n
  else if m > 1 then
    let p = (pow n (m / 2)) in
      if m mod 2  = 0 then  p * p
      else p * p * n
  else raise (Error "Negative powers not allowed")

let digitConv (c : char) : int =
  if c = '0' then 0
  else if c = '1' then 1 
  else if c = '2' then 2  
  else if c = '3' then 3
  else if c = '4' then 4 
  else if c = '5' then 5
  else if c = '6' then 6 
  else if c = '7' then 7
  else if c = '8' then 8
  else if c = '9' then 9
  else raise (Error "Expected digit here") 

let numbP : int parse =
  let isDigit = function '0' .. '9' ->  true | _ -> false
  and sumList xs = List.fold_right (fun y (x, n) -> (x + (digitConv y) * (pow 10 n), n + 1)) 
    xs (0, 0) in
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

let addop : (int -> int -> int) parse = 
  ( let* _ = symb ['+'] in
      return Int.add ) +++
  ( let* _ = symb  ['-'] in
      return Int.sub )

let mulop : (int -> int -> int) parse = 
  ( let* _ = symb ['*'] in
      return Int.mul ) +++
  ( let* _ = symb  ['/'] in
      return Int.div ) 

let powop : (int -> int -> int) parse = 
  let* _ = str ['^'] in
    return pow

let rec expr : (int parse) Lazy.t =
  let strg = lazy (chainr (Lazy.force factor) powop) in 
    let term = lazy (chainl (Lazy.force strg) mulop) in
      lazy (chainl (Lazy.force term) addop)
and factor = lazy ( 
  (token numbP) +++ (
    let* _ = symb ['('] in
    let* n = Lazy.force expr in
    let* _ = symb [')'] in
      return n ) )
 

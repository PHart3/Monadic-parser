
#load "parserMon.cmo"
#load "arithParser.cmo"
open ArithParser

(* functions that convert between strings and char lists *)
let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let rec word l = match l with
  | [] -> (0, fun x -> '!')
  | c::cs -> let (n, f) = word cs in
    (n + 1, fun x -> if x = 0 then c else f (x - 1)) 

let implode l =
  let (n, f) = word l in
    String.init n f  

let doParse (s : string) : (float * string) list = 
  let xs = apply (Lazy.force expr) (explode s) in
    List.map (fun (n, cs) -> (n, implode cs)) xs


(* Definition of Parsing monad *)

module Parsing = struct
  type stringL = char list
  type 'a parse = | Pars of (stringL -> (('a * stringL) list))
  let return x = Pars (fun s -> [(x, s)]) 
  let parseApp (Pars p) s = p s  
  let ( let* ) (Pars p) f =     
    Pars (fun s -> List.concat_map (fun (x, s') -> parseApp (f x) s') (p s))
  let (+++) (Pars p1) (Pars p2) = 
    Pars (fun s -> 
      match (p1 s), (p2 s) with
      | [], [] -> []
      | (x::xs), _ -> [x]
      | [], (x::xs) -> [x] ) 
  let zero = Pars (fun s -> [])
end


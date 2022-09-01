(* String maps *)
(* IIT CS443, Fall 2022 *)
(* Stefan Muller *)

module M = Map.Make
               (struct
                 type t = string
                 let compare = String.compare
               end)

include M

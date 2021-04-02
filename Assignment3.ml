(*
Honor code comes here:
I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.

First Name: Jungin
Last Name: Chang 
BU ID: U07196971
*)

(* the type of a plymorphic tree *)
type 'a tree =
  | Leaf of 'a 
  | Node of 'a tree * 'a tree


(*
TODO: write a map function for trees:
For example,
map_tree (fun x -> x+1) (Node (Leaf 1, Leaf 2)) =  (Node (Leaf 2, Leaf 3))
map_tree (fun _ -> 0)  (Node (Node (Leaf true, Node (Leaf true, Leaf false)), Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
                       (Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Leaf 0    )))
*)
let rec map_tree (f: 'a -> 'b) (tree: 'a tree): 'b tree =  
  match tree with 
  |Leaf x -> Leaf (f x)
  |Node (lt,rt) -> Node(map_tree f lt, map_tree f rt)

(*
TODO: write a fold function for trees:
*)

let rec fold_tree (node: 'b -> 'b -> 'b)  (leaf: 'a -> 'b)  (tree: 'a tree): 'b  = 
  match tree with 
  |Leaf x -> leaf x 
  |Node (lt,rt) -> node (fold_tree node leaf lt) (fold_tree node leaf rt)

(*
TODO: sum the contents of an int tree
For example,
sum_ints (Node (Leaf 1, Leaf 2)) = 3
*)
let rec sum_ints (tree: int tree): int  = 
  match tree with
    Leaf x -> x
  | Node (lt,rt) -> sum_ints lt + sum_ints rt

(*
TODO: find the size of the tree
For example,
tree_size (Leaf 1) = 1
tree_size (Node (Leaf 1, Leaf 2)) = 3
*)
                      
let rec tree_size (tree: 'a tree): int  = 
  match tree with
  | Leaf x -> 1
  | Node (lt, rt) -> 1 + tree_size lt + tree_size rt



(*
TODO: find the height of the tree
For example,
tree_height (Leaf 2) = 1
tree_height (Node ((Node (Leaf 1, (Node ((Node (Leaf 1, Leaf 2)), Leaf 2)))), Leaf 2)) = 5
*)
let rec tree_height (tree: 'a tree): int  = 
  match tree with
  | Leaf x -> 1
  | Node (lt, rt) -> 1 + (max (tree_height lt) (tree_height rt))


(*
TODO: write a function that takes a predicate on trees and retuns true if any subtree satisfies that predicate
For example,
tree_contains (Node (Leaf 1, Leaf 2)) (fun x -> match x with Leaf 2 -> true | _ -> false) = true
*)
                      

let rec tree_contains (tree: 'a tree) (look_for: 'a tree -> bool): bool  = 
  match look_for(tree) with 
  |true -> true
  |false -> match tree with
    |Leaf x -> look_for(Leaf x) 
    |Node (lt,rt) -> tree_contains lt look_for || tree_contains rt look_for 
        

(*
TODO: write a function that shows bool trees :
For example,
show_bool_tree (Leaf true) ="true"
show_bool_tree (Node (Leaf true, Leaf false)) = "(true^false)" 
show_bool_tree  (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
    "((true^(true^false))^((true^(true^false))^false))" 
*)
let rec show_bool_tree (tree: bool tree) : string  = 
  match tree with
  |Leaf x -> if x = true then "true" else "false" 
  |Node (lt,rt) ->"(" ^ show_bool_tree lt ^ "^" ^ show_bool_tree rt ^ ")" 


(* standard functions to convert between strin and char list *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
    | [] -> res
    | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;

(*
TODO: write a fubction that reads bool trees :
for all (finite) t : bool trees.
read_bool_tree t = Some (show_bool_tree t)
For example,
read_bool_tree "true" = Some (Leaf true)
read_bool_tree "false" = Some (Leaf false)
read_bool_tree "tralse" = None
read_bool_tree "(true^false)" = Some (Node (Leaf true, Leaf false))
read_bool_tree "((true^(true^false))^((true^(true^false))^false))" =
Some
 (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false)))
*)

(* Hint 
   write a helper function 
   read_bool_prefix : (char list) -> ((bool * (char list)) option) 
   such that
   read_bool_prefix (explode "true???")       = Some (true, ['?'; '?'; '?'])
   read_bool_prefix (explode "false123")      = Some (false, ['1'; '2'; '3'])
   read_bool_prefix (explode "antythingales") = None
   read_bool_prefix []                        = None
   write a helper function 
   read_bool_tree_prefix (char list) -> ((bool tree * (char list)) option) 
   such that
   read_bool_tree_prefix [] = None
   read_bool_tree_prefix (explode "true???") = Some (Leaf true, ['?'; '?'; '?'])
   read_bool_tree_prefix (explode "(true^false)124") = Some (Node (Leaf true, Leaf false), ['1'; '2'; '4'])
   read_bool_tree_prefix (explode "(true^(true^false))aaa") = Some (Node (Leaf true, Node (Leaf true, Leaf false)), ['a'; 'a'; 'a'])
   read_bool_tree_prefix (explode "(true^(true^fa se))aaa") = None
*)
let firstc clist =
  match clist with
  | [] -> None
  | h::t -> Some (h, t)

  let fivec clist =
    match (firstc clist) with
    | None -> None
    | Some (c1, t1) -> match (firstc t1) with
      | None -> None
      | Some (c2,t2) -> match (firstc t2) with
        | None -> None
        | Some (c3, t3) -> match (firstc t3) with
          | None -> None
          | Some (c4, t4) -> match (firstc t4) with
            | None -> None
            | Some (c5, t5) -> if implode[c1;c2;c3;c4;c5] = Bytes.of_string "false" then Some (Bytes.of_string "false", t5)
                else Some (Bytes.of_string "true", t4) 

let rec read_bool_prefix clist : ((bool * (char list)) option) = 
  match fivec(clist) with
  | None -> None
  | Some (bytes, t) -> if bytes = Bytes.of_string "false" then Some (false, t) 
      else if bytes = Bytes.of_string "true" then Some (true, t)
      else None



let rec read_bool_tree_prefix clist : (bool tree * (char list)) option =
  match clist with
  | [] -> None
  | h::t ->let res = 
              if h != '(' then match (read_bool_prefix clist) with
                | None -> Some (Leaf false, clist)
                | Some (x, next) -> Some (Leaf x, next)

              else match (read_bool_tree_prefix t) with
                | None -> None
                | Some (bytes, t1) ->
                    match t1 with
                    | [] -> None
                    | h::t -> if h != ')' then None else Some (bytes, t)
              
      in match res with
      | None -> None
      | Some (tree, rest) ->
          match rest with
          | '^'::cont -> (match (read_bool_tree_prefix cont) with
              | None -> None
              | Some (tree_in, rem) -> Some (Node (tree, tree_in), rem)) 
          | _ -> res


let rec read_bool_tree (tree: string) : ((bool tree) option) = 
  match tree with 
  |"true" -> Some(Leaf true)
  | _ -> match read_bool_tree_prefix (explode tree) with
    | Some (tree, []) -> Some tree
    | _ -> None



(*
write a fubction that checks that parenthisis are balnaced:
Parenthisis are balenced if there are no parenthises
Parenthisis are balenced if ( and )  enclose a balenced parenthises
Parenthisis are balenced if balenced parenthises are ajacent to a balenced parenthisis
For example,
matching_parens "" = true
matching_parens "((((((((((()))))))))))" = true
matching_parens "()()()()()()" = true
matching_parens "(()())" = true
matching_parens "())(()" = false
*)


(* Hint 
   write mutually recirsive functions 
   matching_paren_prefix : (char list) -> ((char list) option)
   matching_parens_prefix : (char list) -> ((char list) option)
   the and keyword allows mutual recursion
   let rec matching_paren_prefix (ls: char list) : ((char list) option) = failwith "unimplemented"
   and matching_parens_prefix  (ls: char list) : ((char list) option) = failwith "unimplemented"
   such that
   matching_paren_prefix [] = None
   matching_paren_prefix (explode "(???") = None
   matching_paren_prefix (explode "()???") = Some ['?'; '?'; '?']
   matching_paren_prefix (explode "(((())))123") = Some ['1'; '2'; '3']
   matching_paren_prefix (explode "()()()") = Some []
   matching_paren_prefix (explode "(()()())abc") = Some ['a'; 'b'; 'c']
   matching_parens_prefix [] = Some []
   matching_parens_prefix (explode "()()()") = Some ['('; ')'; '('; ')']
   matching_parens_prefix (explode ")aa") = Some [')'; 'a'; 'a']
*)


let rec matching_parens_prefix list acc =
  match list with 
  | [] -> acc = []
  | h::t -> if h = '(' then matching_parens_prefix t ('('::acc)
      else if h = ')' then match acc with 
        | h1::t1 -> if h1 = '(' then  matching_parens_prefix t t1
            else false
        | _ -> false 
      else false 

let rec matching_parens (tree: string) : bool =
  matching_parens_prefix (explode(tree)) []

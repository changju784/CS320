(*
Honor code comes here:

First Name:Jungin
Last Name:Chang
BU ID:U07196971

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)



(* 
a print_list function useful for debugging.
*)



let rec print_list (ls: int list): unit =
  let rec aux ls = match ls with
    | [] -> print_string ""
    | e::[] -> print_int e
    | e::l -> 
      let _ = print_int e 
      in let _ = print_string "; " 
      in aux l

  in let _ = print_string "[" 
  in let _ = aux ls
  in         print_string "]" 


(* Problems *)

(*
TODO: Write a function called between that lists the integers between two integers (inclusive)
If the first number is greater then the second return the empty list
the solution should be tail recursive

For example,
between 4 7 = [4; 5; 6; 7]
between 3 3 = [3]
between 10 2 = []
between 4 1000000 does not stack overflow
*)

let rec between (n:int) (e:int): int list = 
  let rec range a b acc =
    if a==b 
    then a::acc 
    else if a<b 
    then range a (b-1) (b::acc)
    else acc
  in range n e []

  



(*
TODO: Write a zip function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, combine as long as possible
your method should be tail recursive.

For example,
zip_int [1;2;3;5] [6;7;8;9] = [(1,6);(2,7);(3,8);(5,9)]
zip_int [1] [2;4;6;8] = [(1,2)]
zip_int (between 0 1000000) (between 0 1000000) does not stack overflow
*)

let rec pair x y acc = 
  match x, y with 
  | [], [] -> List.rev acc
  | x1::[], y1::y2 -> List.rev ((x1, y1)::acc)
  | x1::x2, y1::[] -> List.rev ((x1, y1)::acc)
  | x1::x2, y1::y2 -> pair x2 y2 ((x1, y1)::acc)
  | [], y1::y2 -> List.rev acc
  | x1::x2, [] -> List.rev acc 
    
let zip_int (a: int list) (b: int list): (int * int) list = 
  pair a b []


(*
TODO: Write a dotProduct function for lists of integers,
If the two list are of unequal lengths then return 0

For example,
dotProduct [1;2;3;4] [6;7;8;9] = 80            (since 1*6+2*7+3*8+4*9 = 80)
dotProduct [1;2;3;4] [6] = 0
*)



let product a b =          
  let rec prod a b acc = 
    match a,b with 
    | [], [] -> acc
    | a1::[], b1::[] -> (a1*b1)::acc 
    | a1::b2, [] -> []
    | [], b1::b2 -> []
    | a1::[], b1::b2 -> []
    | a1::a2, b1::[] -> [] 
    | a1::a2, b1::b2 -> prod a2 b2 ((a1*b1)::acc) 
  in prod a b []
    
let rec sum ls=
  match ls with
  | [] -> 0
  | h::t-> h+ (sum t)

let rec dotProduct (x: int list) (y: int list): int = 
  sum(product x y)


(* 
TODO:
Write a function that takes a list of tuples and returns a string representation of that list

your representation should be valid as OCaml source:
* every element of a list must be separated by ";"
* the list must be wrapped in "[" and "]"
* tuples should (1,2)
* You may use whitespace however you like

For example,
list_of_tuple_as_string [(1,2);(3,4);(5,6)] = "[ (1,2); (3,4); (5,6) ]"
*)


let rec t_to_s ls =
  match ls with 
  | [] -> ""
  | (x, y) :: [] -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ") ]" 
  | (x, y) :: tail -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ");" ^ t_to_s tail 
                        
  
let rec list_of_tuple_as_string (list: (int*int) list): string = 
  let str ="[ " ^ (t_to_s list)
  in str



(* 
TODO:
Write an insertion sort function for lists of integers

for example,
sort [6;7;1] = [1;6;7]
*)

(* 
Hint: We encourage you to write the following helper function 

let rec insert (i: int) (list: int list): int list = failwith "unimplemented"

that takes a a number, an already sorted ls and returns a new sorted list with that number inserted
for example,
insert 5 [1;3;5;7] = [1;3;5;5;7]

You can  then call this helper function inside sort. 
*)
let rec insert new_int ls =
  match ls with 
  | [] -> [new_int]
  | h::t -> if new_int < h then new_int::h::t else h:: insert new_int t 

let rec sort (ls: int list): int list =
  match ls with
  | [] -> []
  | h::t -> insert h (sort t)

              
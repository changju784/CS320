(*
Honor code comes here: I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.

First Name: Jungin 
Last Name:Chang
BU ID:U07196971


*)

(*
Write a zip function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, return None
your method should be tail recursive.

For example,
safe_zip_int [1;2;3;5] [6;7;8;9] = Some [(1,6);(2,7);(3,8);(5,9)]
safe_zip_int [1] [2;4;6;8] = None
safe_zip_int (between 0 1000000) (between 0 1000000) does not stack overflow
*)

let rec pair ls1 ls2 acc =   
  match ls1,ls2 with 
  | ([],[]) -> Some acc
  | (_,[]) -> None 
  | ([],_) -> None
  | ((h1::t1),(h2::t2)) -> pair t1 t2 ((h1,h2)::acc) 


let safe_zip_int (ls1: int list) (ls2: int list) : ((int * int) list) option =
  pair ls1 ls2 []
    


(*
Write a zip function that produces the ith Pell number:
https://en.wikipedia.org/wiki/Pell_number
https://oeis.org/A000129
your function should be tail recursive, but only needs to have the correct output up to integer overflow

pell 0 = 0
pell 1 = 1
pell 7 = 169
pell 1000000  does not stack overflow
*)
let rec pell_num i acc1 acc2 =
  if i = 0 then acc1
  else pell_num (i - 1) acc2 (acc1 + (2* acc2))
        

let pell (i: int) : int = 
  if i = 0 then 0
  else if i = 1 then 1 
  else pell_num i 0 1

  

(* The nth Tetranacci number T(n) is mathematically defined as follows.
 *
 *      T(0) = 0
 *      T(1) = 1
 *      T(2) = 1
 *      T(3) = 2
 *      T(n) = T(n-1) + T(n-2) + T(n-3) + T(n-4)
 *
 * For more information, you may consult online sources.
 *
 *    https://en.wikipedia.org/wiki/Generalizations_of_Fibonacci_numbers
 *    https://mathworld.wolfram.com/TetranacciNumber.html
 *
 * Write a tail recursive function tetra that computes the nth Tetranacci
 * number efficiently.In particular, large inputs such as (tetra 1000000)
 * should neither cause stackoverflow nor timeout.
*)

let tetra (n : int) : int = 
  let rec aux i acc1 acc2 acc3 acc4 =
    if i == 0 then acc1
    else if i == 1 then acc2
    else if i == 2 then acc3 
    else if i == 3 then acc4
    else aux (i - 1) acc2 acc3 acc4 (acc4+acc3+acc2+acc1)
  in aux n 0 1 1 2

(*
infinite precision natural numbers can be represented as lists of ints between 0 and 9

Write a function that takes an integer and represents it with a list of integers between 0 and 9 where the head is the least signifigant digit.
If the input is negative return None

toDec 1234 = Some [4; 3; 2; 1]
toDec 0 = Some []
toDec -1234 = None
*)

(* Hint use 
   mod 10
   / 10
*)
let rec modu i acc= 
  match i with
  | 0 -> acc
  | _ -> modu ((i- (i mod 10))/10) ((((i- (i mod 10))/10) mod 10)::acc) 
          
let cut nl =
  match nl with
  | []->[]         
  | head::tail-> List.rev tail

let rec toDec (i : int) : int list option = 
if i > 0 then Some (cut(modu (i*10) []))
else None

        
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

let plo f = 
  match f with
  | None -> print_string "None"
  | Some v -> print_list(v)


let _ = 
  plo(toDec (1234))

(*
Write a function that sums 2 natrual numbers as represented by a list of integers between 0 and 9 where the head ias the least signifigant digit.
Your function should be tail recursive

sum [4; 3; 2; 1] [1;0;1] = [5; 3; 3; 1]
sum [1] [9;9;9] = [0; 0; 0; 1]
sum [] [] = []
sum (nines 1000000) [1] does not stack overflow, when (nines 1000000) provides a list of 1000000 9s
*)

let revl l = 
  let rec revl2 l num =
    match l with 
    | [] -> num
    | (h1::t1) -> revl2 t1 ((num * 10 ) + h1)
  in revl2 (List.rev l) 0

let rec modu i acc= 
  match i with
  | 0 -> acc
  | _ -> modu ((i- (i mod 10))/10) ((((i- (i mod 10))/10) mod 10)::acc) 
          
let cut nl =
  match nl with
  | []->[]         
  | head::tail-> List.rev tail

let rec toDec1 (i : int) : int list = 
  if i = 0 then (cut(modu (i*10) [0]))
  else [] 


let rec sum (a : int list) (b : int list) : int list = 
  match a,b with
  | [],[] -> [] 
  | _,[] -> toDec1(revl a) 
  | [],_ -> toDec1(revl b) 
  | _,_ -> toDec1(revl (a) + revl (b)) 
             
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
    
let _ = 
  print_list (sum [] [] )


(*
Write an infinite precision version of the pel function from before

pell2 0 = []
pell2 1 = [1]
pell2 7 = [9; 6; 1]
pell2 50 = [2; 2; 5; 3; 5; 1; 4; 2; 9; 2; 4; 6; 2; 5; 7; 6; 6; 8; 4]

*)

let rec pell2 (i: int) : int list = 
  toDec1(pell i) 
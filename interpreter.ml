(*
Honor code comes here:I pledge that this program represents my own
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





open Printf


let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)
    
(*types-----------------------*)

type 'a parser = Parser of (string -> ('a * string ) list)

type const = | INT of int | BOOL of bool | STRING of string | UNIT of unit 

type com = | Push of const | Pop | Log | Swap | Add | Sub | Mul | Div | Rem | Neg

(*parsers------------------*)

let parse p s = 
  match p with 
    Parser f -> f s 
                  
let returnP a = 
  Parser 
    ( fun s -> [a,s] )

let failP = 
  Parser
    ( fun s->[] )
    
let (>>=) p f = 
  Parser ( fun s ->
      match (parse p s ) with 
        []->[]   
      |
        (h,rest)::_->  let parser2 = f h in 
          match (parse parser2 rest) with 
            []->[]   
          |
            (h2,rest2)::_->[(h2,rest2)] )

let (<|>) a b = 
  Parser (
    fun s->  match (parse a s) with 
        []-> parse b s 
      |
        r->r
  )

let charP  = 
  Parser (
    fun s ->
      match (explode s) with 
        []->[]
      |
        h::rest->[(h,implode rest)]
  )

let strP =
  Parser (
    fun s ->
      match (explode s) with
        [] -> []
      | x::[] -> [x, ""]
      | x::x1::y -> 
          if  x != ';' && x1 != '\n' && x1 != ' '  then [x,(implode (x1::y))] 
          else if x == ';' && x1 != '\n' && x1 != ' ' then [x, (implode (x1::y))] 
          else if x1 == ' '  then  [x, (implode (x1::y))] 
          else []
  )

let satcP (c:char)= 
  charP>>=fun x->
  if x=c then returnP c 
  else failP

let satsP s = 
  if s="" then failP else
    let rec asats (s:string)= 
      match (explode s) with 
        h::rest->satcP h >>=fun _->asats (implode rest)
      |
        []-> returnP([])
    in 
    asats (s:string)

let rec many0 p =
  (p >>= fun a -> 
   many0 p >>= fun b-> 
   returnP (a::b))
  <|>
  returnP []


let rec many1 p =
  p >>= fun a -> 
  many0 p >>= fun b-> 
  returnP (a::b) 

let whitespaceP = 
  satcP ' ' <|> satcP '\t' <|> satcP '\n' <|> satcP ';'

let enterP =
  satsP ";\t" <|> satsP ";\n" <|> satsP "; "

let digitP = 
  charP >>= fun x -> 
  if '0' <= x && x <= '9' 
  then returnP x
  else failP

let natP = 
  (many1 digitP >>= fun a-> 
   returnP (int_of_string (implode a)))

let integerP = 
  natP
  <|>
  (satcP '-' >>= fun _->
   natP >>= fun v -> 
   returnP ((-1)*v)) 

let stringP =
  many1 strP >>= fun b ->
  enterP >>= fun _ ->
  returnP (implode b)
    
let boolP =
  (many0 whitespaceP >>= fun _->
   satsP "<true>" >>= fun s ->
   many1 whitespaceP >>= fun _->
   returnP true)
  <|>
  (many0 whitespaceP >>= fun _->
   satsP "<false>" >>= fun s1 ->
   many1 whitespaceP >>= fun _->
   returnP false)

let unitP =
  many0 whitespaceP >>= fun _->
  satsP "<unit>" >>= fun s ->
  many1 whitespaceP >>= fun _->
  returnP ()

    

    (* functions ----------------------------------------*)


let pushP = 
  many0 whitespaceP >>= fun _->
  satsP "Push" >>= fun _-> 
  many1 whitespaceP >>= fun _-> 
  (integerP >>= fun i -> 
   returnP (Push (INT i))) 
  <|> 
  (boolP >>= fun b -> 
   returnP (Push (BOOL b)))
  <|>
  (unitP >>= fun u ->
   returnP (Push (UNIT u)))
  <|>
  (stringP >>= fun s ->
   returnP (Push (STRING s)))
  
  
let popP =
  many0 whitespaceP >>= fun _->
  satsP "Pop" >>= fun _-> 
  many1 whitespaceP >>= fun _->
  returnP (Pop)

let logP =
  many0 whitespaceP >>= fun _->
  satsP "Log" >>= fun _-> 
  many1 whitespaceP >>= fun _->
  returnP (Log)

let swapP =
  many0 whitespaceP >>= fun _->
  satsP "Swap" >>= fun _-> 
  many1 whitespaceP >>= fun _->
  returnP (Swap)

let addP = 
  many0 whitespaceP >>= fun _->
  satsP "Add" >>= fun _-> 
  many1 whitespaceP >>= fun _->
  returnP (Add)

let subP =
  many0 whitespaceP >>= fun _->
  satsP "Sub" >>= fun _-> 
  many1 whitespaceP >>= fun _->
  returnP (Sub)

let mulP =
  many0 whitespaceP >>= fun _->
  satsP "Mul" >>= fun _-> 
  many1 whitespaceP >>= fun _-> 
  returnP (Mul)
    
let negP =
  many0 whitespaceP >>= fun _->
  satsP "Neg" >>= fun _-> 
  many1 whitespaceP >>= fun _->
  returnP (Neg)

let divP =
  many0 whitespaceP >>= fun _->
  satsP "Div" >>= fun _-> 
  many1 whitespaceP >>= fun _->
  returnP (Div)

let remP =
  many0 whitespaceP >>= fun _->
  satsP "Rem" >>= fun _-> 
  many1 whitespaceP >>= fun _->
  returnP (Rem)



let comP =
  pushP <|> addP <|> popP <|> logP <|> swapP <|> subP <|> mulP <|> divP <|> remP <|> negP

let topP = 
  many1 comP

    (*execution functions-----------------------*)


let push var acc =
  var::acc

let pop acc =
  match acc with
  | [] -> []
  | x::y -> y

let swap acc =
  match acc with
  | [] -> []
  | x::[] -> []
  | x::(x1::y) -> x1::(x::y)

let add acc =
  match acc with
  | [] -> []
  | _::[] -> []
  | (INT x)::(INT x1)::y -> (INT (x+x1))::y
  | _::_ -> []

let sub acc =
  match acc with
  | [] -> []
  | _::[] -> []
  | (INT x)::(INT x1)::y -> (INT (x-x1))::y
  | _::_ -> []


let mul acc =
  match acc with
  | [] -> []
  | _::[] -> []
  | (INT x)::(INT x1)::y -> (INT (x*x1))::y
  | _::_ -> []

let div acc =
  match acc with
  | [] -> []
  | _::[] -> []
  | (INT x)::(INT x1)::y -> if (x1 != 0) then (INT (x / x1))::y else [] 
  | _::_ -> []


let rem acc =
  match acc with
  | [] -> []
  | _::[] -> []
  | (INT x)::(INT x1)::y -> if (x1 != 0) then (INT (x mod x1))::y else []
  | _::_ -> []

let neg acc =
  match acc with
  | [] -> []
  | (INT x)::y -> (INT (x*(-1)) )::y
  | _::_ -> []


let log acc out =
  match acc with
  | [] -> out
  | x::y -> x::out




(* error functions --------------------*)

let pop_log_err acc err =
  match acc with
  | [] -> 2
  | x::y -> 0

let div_rem_err acc err =
  match acc with
  | [] -> 2
  | x::[] -> 2
  | (INT x)::(INT x1)::y -> if x1 != 0 then 0 else 3
  | _::_ -> 1 

let add_sub_mul_err acc err =
  match acc with
  | [] -> 2
  | h::[] -> 2
  | (INT x)::(INT x1)::y ->  0
  | _::_ -> 1    
    
let swap_err acc err =
  match acc with
  | [] -> 2
  | x::[] -> 2
  | x::y -> 0 

let neg_err acc err =
  match acc with
  | [] -> 2
  | (INT x)::y ->  0
  | _::_ -> 1




(*execution------------------*)

let rec runParse ps acc res err =
  match ps with
  | [] -> res, err
  | x::y -> if err == 0 then
        (match x with | Push var -> runParse y (push var acc) res (0)
                      | Pop -> runParse y (pop acc) res (pop_log_err acc err)
                      | Log -> runParse y (pop acc) (log acc res) (pop_log_err acc err)
                      | Swap -> runParse y (swap acc) res (swap_err acc err)
                      | Add -> runParse y (add acc) res (add_sub_mul_err acc err)
                      | Sub -> runParse y (sub acc) res (add_sub_mul_err acc err)
                      | Mul -> runParse y (mul acc) res (add_sub_mul_err acc err)
                      | Div -> runParse y (div acc) res (div_rem_err acc err)
                      | Rem -> runParse y (rem acc) res (div_rem_err acc err)
                      | Neg -> runParse y (neg acc) res (neg_err acc err))
      else res, err

let rec const_to_string cl acc =
  match cl with
  | [] -> acc
  | (INT x)::y -> const_to_string y (string_of_int x::acc)
  | (BOOL x)::y -> if x == false then const_to_string y ("<false>"::acc)  else const_to_string y ("<true>"::acc)
  | (UNIT x)::y -> const_to_string y ("<unit>"::acc)
  | (STRING x)::y -> const_to_string y (x::acc)

let interpreter (s : string) : string list * int = 
  match (parse topP s) with
  | [] -> [],0
  | [ps, _] -> (match (runParse ps [] [] 0)with 
      | [],0 -> [],0
      | result,error -> (const_to_string result []),error 
    ) 
  | (_, _)::_::_ -> [],0
                    
let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res


let runfile (file : string) : string list * int =
  let s = readlines file in
  interpreter s;;
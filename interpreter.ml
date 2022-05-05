(* parsing util functions *)

let is_lower_case c = 'a' <= c && c <= 'z'

let is_upper_case c = 'A' <= c && c <= 'Z'

let is_alpha c = is_lower_case c || is_upper_case c

let is_digit c = '0' <= c && c <= '9'

let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c

let is_blank c = String.contains " \012\n\r\t" c

let is_bool x = String.contains "01" x

let is_initial x = is_alphanum x || x = '_' || x = '\''

let explode s = List.of_seq (String.to_seq s)

let implode ls = String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ loop ()
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option = p (explode s)

let pure (x : 'a) : 'a parser = fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let ( >>= ) = bind

let ( let* ) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then
      Some (x, ls)
    else
      None
  | _ -> None

let char (c : char) : char parser = satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let ( >> ) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> (
      match p2 ls with
      | Some (_, ls) -> Some (x, ls)
      | None -> None)
  | None -> None

let ( << ) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> Some (x, ls)
  | None -> p2 ls

let ( <|> ) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let ( >|= ) = map

let ( >| ) p c = map p (fun _ -> c)

let rec many (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some ((), ls)
    else
      None
  | _ -> None

let ws : unit parser = many whitespace >| ()

let ws1 : unit parser = many1 whitespace >| ()

let digit : char parser = satisfy is_digit

let initial : char parser = satisfy is_initial 

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match (cs, ls) with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c then
        loop cs xs
      else
        None
    | _ -> None
  in
  loop cs ls

let keyword (s : string) : unit parser = literal s >> ws >| ()

(* end of parser combinators *)

(* Types *)
type constant =
  | Int of int
  | Bool of bool
  | Name of string
  | Nothing

  | Closure of 
      (constant * constant) list * (constant * constant) list * constant * constant * prog

and command = 
  | Trace of constant
  | Push of constant
  | Pop of constant
  | Add of constant
  | Sub of constant
  | Mul of constant
  | Div of constant

  | And 
  | Or 
  | Not 
  | Equal
  | Lte 
  | Local 
  | Global 
  | Lookup 
  | Beginend of prog 
  | Condition of prog * prog 

  | Funend of constant * constant * prog
  | Call 
  | Tryend of prog
  | Switchend of sub_command list

and sub_command = 
  | Case of constant * prog

and stack = constant list
and prog = command list
and env = (constant * constant) list

(* Helpers *)
let int_natural : constant parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) -> Some (Int(int_of_string (implode xs)), ls)
  | _ -> None

let is_name : constant parser = 
  fun ls -> 
  match many1 initial ls with
  | Some (xs, ls) -> Some (Name(implode xs), ls)
  | _ -> None

(* Parsers for constant type *)
let parse_int : constant parser = 
  let* x = int_natural in 
  pure (x)

let parse_bool_f : constant parser = 
  let* _ = keyword "False" in
  pure (Bool false)

let parse_bool_t : constant parser = 
  let* _ = keyword "True" in
  pure (Bool true)

let parse_name : constant parser = 
  let* x = is_name in 
  pure (x)

let parse_nothing : constant parser =
  let* _ = keyword "()" in
  pure (Nothing)

let rec parse_constant () : constant parser =        
  parse_int
  <|>
  parse_bool_f
  <|>
  parse_bool_t
  <|>
  parse_name
  <|>
  parse_nothing

(* Parsers for command type *)
and parse_trace () : command parser = 
  let* _ = ws in
  let* _ = keyword "Trace" in
  let* x = parse_constant () in 
  pure (Trace x) << ws

and parse_push () : command parser = 
  let* _ = ws in
  let* _ = keyword "Push" in
  let* x = parse_constant () in
  pure (Push x) << ws

and parse_pop () : command parser = 
  let* _ = ws in
  let* _ = keyword "Pop" in
  let* x = parse_constant () in
  pure (Pop x) << ws

and parse_add () : command parser = 
  let* _ = ws in
  let* _ = keyword "Add" in
  let* x = parse_constant () in 
  pure (Add x) << ws

and parse_sub () : command parser = 
  let* _ = ws in
  let* _ = keyword "Sub" in 
  let* x = parse_constant () in 
  pure (Sub x) << ws

and parse_mul () : command parser = 
  let* _ = ws in
  let* _ = keyword "Mul" in 
  let* x = parse_constant () in 
  pure (Mul x) << ws

and parse_div () : command parser = 
  let* _ = ws in
  let* _ = keyword "Div" in 
  let* x = parse_constant () in 
  pure (Div x) << ws

and parse_and () : command parser = 
  let* _ = ws in
  let* _ = keyword "And" in 
  pure (And) 

and parse_or () : command parser = 
  let* _ = ws in
  let* _ = keyword "Or" in 
  pure (Or)

and parse_not () : command parser =
  let* _ = ws in
  let* _ = keyword "Not" in 
  pure (Not)

and parse_equal () : command parser = 
  let* _ = ws in
  let* _ = keyword "Equal" in 
  pure (Equal) 

and parse_lte () : command parser = 
  let* _ = ws in
  let* _ = keyword "Lte" in 
  pure (Lte)

and parse_local () : command parser = 
  let* _ = ws in
  let* _ = keyword "Local" in 
  pure (Local) 

and parse_global () : command parser = 
  let* _ = ws in
  let* _ = keyword "Global" in 
  pure (Global)

and parse_lookup () : command parser = 
  let* _ = ws in
  let* _ = keyword "Lookup" in 
  pure (Lookup)

and parse_beginend () : command parser = 
  let* _ = ws in 
  let* _ = keyword "Begin" in 
  let* _ = ws in 
  let* x = parse_cmdlst () in 
  let* _ = ws in 
  let* _ = keyword "End" in 
  pure (Beginend x)

and parse_condition () : command parser = 
  let* _ = ws in
  let* _ = keyword "If" in 
  let* _ = ws in 
  let* x = parse_cmdlst () in 
  let* _ = ws in 
  let* _ = keyword "Else" in 
  let* _ = ws in
  let* y = parse_cmdlst () in 
  let* _ = ws in 
  let* _ = keyword "End" in 
  pure (Condition(x, y))

and parse_funend () : command parser =
  let* _ = ws in 
  let* _ = keyword "Fun" in
  let* _ = ws in 
  let* fun_name = parse_name in 
  let* _ = ws in 
  let* fun_param = parse_name in 
  let* _ = ws in 
  let* fun_body = parse_cmdlst () in 
  let* _ = ws in 
  let* _ = keyword "End" in 
  pure (Funend(fun_name, fun_param, fun_body))

and parse_call () : command parser = 
  let* _ = ws in
  let* _ = keyword "Call" in 
  pure (Call)

and parse_tryend () : command parser = 
  let* _ = ws in 
  let* x = keyword "Try" in
  let* _ = ws in 
  let* y = parse_cmdlst () in
  let* _ = ws in
  let* _ = keyword "End" in
  pure (Tryend(y))

and parse_switchend () : command parser = 
  let* _ = ws in 
  let* _ = keyword "Switch" in 
  let* _ = ws in 
  let* x = many (parse_case ()) in 
  let* _ = ws in 
  let* _ = keyword "End" in
  pure (Switchend(x))

and parse_command () : command parser = 
  parse_trace ()
  <|>
  parse_push ()
  <|>
  parse_pop ()
  <|> 
  parse_add ()
  <|>
  parse_sub ()
  <|>
  parse_mul ()
  <|>
  parse_div ()
  <|> 
  parse_and ()
  <|> 
  parse_or ()
  <|> 
  parse_not ()
  <|> 
  parse_equal ()
  <|> 
  parse_lte ()
  <|>
  parse_local ()
  <|>
  parse_global ()
  <|>
  parse_lookup ()
  <|> 
  parse_beginend ()
  <|>
  parse_condition ()
  <|>
  parse_funend ()
  <|>
  parse_call ()
  <|>
  parse_tryend ()
  <|>
  parse_switchend ()

and parse_cmdlst () : prog parser = 
  many (parse_command ())

and parse_case () : sub_command parser = 
  let* _ = ws in 
  let* _ = keyword "Case" in
  let* _ = ws in
  let* x = parse_int in 
  let* _ = ws in
  let* y = parse_cmdlst () in
  pure (Case(x, y))


(* Helpers for eval *)
let rec trace_helper (stk : stack) (log_bf : string list) (log : string list) (x : int) 
  : stack * string list * string list =
  if x = 0 then (stk, log_bf, log) 
  else
    match stk with
    | [] -> ([], log_bf, log)
    | h::t -> match h with
      | Int num -> trace_helper t (string_of_int(num)::log_bf) (string_of_int(num)::log) (x-1)
      | Bool b -> 
        (
          match b with 
          | true -> trace_helper t ("True"::log_bf) ("True"::log) (x-1)
          | false -> trace_helper t ("False"::log_bf) ("False"::log) (x-1)
        )
      | Name n -> trace_helper t (n::log_bf) (n::log) (x-1) 
      | Nothing -> trace_helper t ("()"::log_bf) ("()"::log) (x-1)
      (* Closure? *)
      | _ -> 
        (* ([], log_bf, ["Error"]) *)
        (stk, log_bf, log)
(* trace_helper t log_bf ["Error"] (x-1) *)

(* Checks if elements in the list are all Ints *)
let rec int_check (lst : constant list) : bool =
  match lst with
  | [] -> true
  | h::t -> match h with
    | Int _ -> int_check t
    | _ -> false

let rec pop (lst : 'a list) (x : int) : 'a list = 
  if x = 0 then lst
  else
    match lst with
    | [] -> []
    | h::t -> pop t (x-1)

let rec op (stk1 : stack) (stk2 : stack) (x : int) : (stack * stack) =
  if x = 0 then (stk1, stk2) else
    match stk1 with
    | h::t -> op t (stk2 @ [h]) (x-1)
    | _ -> ([], [])

let rec lst_addnums (lst : constant list) : int =
  List.fold_left 
    (
      fun acc x -> match x with 
        | Int(x) -> acc + x
        | _ -> acc
    ) 0 lst

let lst_subnums (lst : constant list) : int =
  match lst with
  | [] -> 0
  | h::t -> match h with
    | Int x -> x - (lst_addnums t)
    | _ -> 0 

let rec lst_mulnums (lst : constant list) : int =
  match lst with
  | [] -> 1
  | h::t -> match h with
    | Int x -> x * (lst_mulnums t)
    | _ -> 0 

let lst_divnums (lst : constant list) : int =
  match lst with
  | [] -> 1
  | h::t -> match h with
    | Int x -> if lst_mulnums t = 0 then -1 
      else x / (lst_mulnums t)
    | _ -> -1  

let const_to_bool (x : constant) : bool = 
  match x with
  | Bool x -> x
  | _ -> failwith "const_to_bool failed"

let str_to_const (str : string) : constant = 
  match str with
  | x -> Name x

let safe_funend (name1 : constant) (name2 : constant) (cmdlst : prog) : (string * string) option =
  match (name1, name2) with
  | (Name x, Name y) -> Some (x, y)
  | _ -> None

let safe_assoc (x : string) (lst : env) : constant option = 
  try Some (List.assoc (str_to_const x) lst) with
  | Not_found -> None

(* Evaluation function *)
let rec eval 
    (stk : stack) (cmdlst : prog) (log_bf : string list) (log : string list) 
    (local : env) (global : env)
  : stack * prog * string list * string list * env * env = 
  (
    match cmdlst with 
    (* | [] -> ([], cmdlst, log, local, global) *)
    | [] -> (stk, [], log_bf, log, local, global)
    | h::t -> match h with 

      (* TRACE *)
      | Trace(x) -> 
        (
          match x with 
          | Int intgr -> 
            (
              if List.length(stk) >= intgr then 
                (
                  match trace_helper stk log_bf log intgr with
                  | (new_stk, new_log_bf, new_log) -> 
                    if new_log = ["Error"] then 
                      (stk, t, new_log_bf, ["Error"], local, global)
                    else 
                      eval new_stk t new_log_bf new_log local global 
                )
              else
                (stk, t, log_bf, ["Error"], local, global)
            )
          | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* PUSH *)
      | Push(x) -> eval (x::stk) t log_bf log local global 

      (* POP *)
      | Pop(x) -> 
        (
          match x with
          | Int intgr -> 
            if List.length(stk) >= intgr then 
              eval (pop stk intgr) t log_bf log local global 
            else 
              (stk, t, log_bf, ["Error"], local, global)
          | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* ADD *)
      | Add(x) -> 
        (
          match x with 
          | Int 0 -> eval (Int(0)::stk) t log_bf log local global 
          | Int intgr -> 
            (
              if List.length(stk) >= intgr then 
                (
                  match op stk [] intgr with 
                  | (a, b) -> 
                    (
                      if int_check b then
                        let res = lst_addnums b in 
                        eval (Int(res)::a) t log_bf log local global 
                      else 
                        (stk, t, log_bf, ["Error"], local, global)
                    )
                )
              else 
                (stk, t, log_bf, ["Error"], local, global)
            )
          | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* SUB *)
      | Sub(x) -> 
        (
          match x with 
          | Int 0 -> eval (Int(0)::stk) t log_bf log local global 
          | Int intgr -> 
            (
              if List.length(stk) >= intgr then 
                (
                  match op stk [] intgr with 
                  | (a, b) -> 
                    (
                      if int_check b then
                        let res = lst_subnums b in 
                        eval (Int(res)::a) t log_bf log local global 
                      else 
                        (stk, t, log_bf, ["Error"], local, global)
                    )
                ) 
              else 
                (stk, t, log_bf, ["Error"], local, global)
            )
          | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* MUL *)
      | Mul(x) -> 
        (
          match x with 
          | Int 0 -> eval (Int(1)::stk) t log_bf log local global 
          | Int intgr -> 
            (
              if List.length(stk) >= intgr then 
                (
                  match op stk [] intgr with 
                  | (a, b) -> 
                    (
                      if int_check b then
                        let res = lst_mulnums b in 
                        eval (Int(res)::a) t log_bf log local global 
                      else 
                        (stk, t, log_bf, ["Error"], local, global)
                    )
                )
              else 
                (stk, t, log_bf, ["Error"], local, global)
            )
          | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* DIV *)
      | Div(x) ->
        (
          match x with 
          | Int 0 -> eval (Int(1)::stk) t log_bf log local global 
          | Int intgr -> 
            (
              if List.length(stk) >= intgr then 
                (
                  match op stk [] intgr with 
                  | (a, b) -> 
                    (
                      if int_check b then
                        let res = lst_divnums b in 
                        if res = -1 then 
                          (stk, t, log_bf, ["Error"], local, global) 
                        else
                          eval (Int(res)::a) t log_bf log local global 
                      else 
                        (stk, t, log_bf, ["Error"], local, global)
                    )
                )
              else 
                (stk, t, log_bf, ["Error"], local, global)
            )
          | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* AND *)
      | And -> 
        (
          match stk with
          | [] -> (stk, t, log_bf, ["Error"], local, global)
          | head::tail -> 
            if List.length(stk) < 2 then 
              (stk, t, log_bf, ["Error"], local, global)
            else 
              match tail with 
              | [] -> (stk, t, log_bf, ["Error"], local, global)
              | head'::tail' -> 
                match (head, head') with
                | (Bool h, Bool h') -> 
                  let res = ((const_to_bool head) && (const_to_bool head'))
                  in eval (Bool(res)::tail') t log_bf log local global 
                | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* OR *)
      | Or -> 
        (
          match stk with
          | [] -> (stk, t, log_bf, ["Error"], local, global)
          | head::tail -> if List.length(stk) < 2 then 
              (stk, t, log_bf, ["Error"], local, global)
            else match tail with 
              | [] -> (stk, t, log_bf, ["Error"], local, global)
              | head'::tail' -> 
                match (head, head') with
                | (Bool h, Bool h') -> 
                  let res = ((const_to_bool head) || (const_to_bool head'))
                  in eval (Bool(res)::tail') t log_bf log local global 
                | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* NOT *)
      | Not -> 
        (
          match stk with
          | [] -> (stk, t, log_bf, ["Error"], local, global)
          | head::tail -> 
            match head with
            | Bool x -> 
              if x = true then 
                eval (Bool(false)::tail) t log_bf log local global 
              else 
                eval (Bool(true)::tail) t log_bf log local global 
            | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* EQUAL *)
      | Equal -> 
        (
          match stk with
          | [] -> (stk, t, log_bf, ["Error"], local, global)
          | head::tail -> if List.length(stk) < 2 then 
              (stk, t, log_bf, ["Error"], local, global)
            else 
              match tail with
              | [] -> (stk, t, log_bf, ["Error"], local, global)
              | head'::tail' -> 
                match (head, head') with
                | (Int x, Int y) -> if x = y then 
                    eval (Bool(true)::tail') t log_bf log local global 
                  else 
                    eval (Bool(false)::tail') t log_bf log local global 
                | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* LTE *)
      | Lte -> 
        (
          match stk with
          | [] -> (stk, t, log_bf, ["Error"], local, global)
          | head::tail -> 
            if List.length(stk) < 2 then 
              (stk, t, log_bf, ["Error"], local, global)
            else 
              match tail with
              | [] -> (stk, t, log_bf, ["Error"], local, global)
              | head'::tail' -> 
                match (head, head') with
                | (Int x, Int y) -> if x <= y then 
                    eval (Bool(true)::tail') t log_bf log local global 
                  else 
                    eval (Bool(false)::tail') t log_bf log local global 
                | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* LOCAL *)
      | Local -> 
        (
          match stk with
          | [] -> (stk, t, log_bf, ["Error"], local, global)
          | head::tail ->
            if List.length(stk) < 2 then
              (stk, t, log_bf, ["Error"], local, global)
            else 
              match tail with
              | [] -> (stk, t, log_bf, ["Error"], local, global)
              | head'::tail' -> 
                match (head, head') with 
                (* Top value on the stack has to be a Name *)
                | (Name x, value) -> eval (Nothing::tail') t log_bf log ((Name x, value)::local) global 
                | (_, _) -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* GLOBAL *)
      | Global -> 
        (
          match stk with
          | [] -> (stk, t, log_bf, ["Error"], local, global)
          | head::tail -> 
            if List.length(stk) < 2 then
              (stk, t, log_bf, ["Error"], local, global)
            else 
              match tail with
              | [] -> (stk, t, log_bf, ["Error"], local, global)
              | head'::tail' -> 
                match (head, head') with 
                (* Top value on the stack has to be a Name *)
                | (Name x, value) -> eval (Nothing::tail') t log_bf log local ((Name x, value)::global) 
                | (_, _) -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* LOOKUP *)
      | Lookup -> 
        (
          match stk with
          | [] -> (stk, t, log_bf, ["Error"], local, global)
          | head::tail -> 
            match head with
            | Name x -> 
              (
                match (safe_assoc x local, safe_assoc x global) with
                | (Some x, _) -> eval (x::tail) t log_bf log local global 
                | (None, Some y) -> eval (y::tail) t log_bf log local global 

                (* Name not bound in the current scope *)
                | (_, _) -> (stk, t, log_bf, ["Error"], local, global)
              )
            (* Top value on the stack is not a Name *)
            | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* BEGINEND *)
      | Beginend(cmdlst_be) -> 
        (
          let beginend_output = 
            eval [] cmdlst_be log_bf log local global 
          in 
          (
            match beginend_output with 
            | (stk_be, prog_be, log_bf_be, log_be, _, global_be) ->
              if log_be = ["Error"] then 
                (stk, t, log_bf, ["Error"], local, global)
              else 
                (
                  match stk_be with
                  | head::_ -> eval (head::stk) t log_bf_be log_be local global_be
                  | _ -> (stk, t, log_bf, ["Error"], local, global)
                )
          )
        )

      (* IF ELSE / CONDITION *)
      | Condition(cmdlst1, cmdlst2) -> 
        (
          match stk with 
          | [] -> (stk, t, log_bf, ["Error"], local, global)
          | head::tail -> match head with
            | Bool x -> 
              (
                if x = true then 
                  eval tail (cmdlst1 @ t) log_bf log local global 
                else 
                  eval tail (cmdlst2 @ t) log_bf log local global 
              )
            | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* FUN *)
      | Funend(name1, name2, cmdlst') ->  
        (
          let funend_closure = Closure(local, global, name1, name2, cmdlst') 
          in
          eval stk t log_bf log ((name1, funend_closure)::local) global  
        )

      (* CALL *)
      | Call -> 
        (
          match stk with
          | Closure(r_loc, r_glob, r_name, r_argu, r_body)::const_clos::rest_stk -> 
            (
              let res = eval [] r_body log_bf log ((r_name, Closure(r_loc, r_glob, r_name, r_argu, r_body))::(r_argu, const_clos)::r_loc) global 
              in 
              match res with
              | ([], res_prog, res_log_bf, res_log, _, res_glob) -> 
                (stk, t, res_log_bf, ["Error"], local, res_glob)
              | (res_stk_h::res_stk_t, res_prog, res_log_bf, res_log, _, res_glob) ->
                if res_log = ["Error"] then 
                  (stk, t, res_log_bf, ["Error"], local, res_glob) 
                else
                  eval (res_stk_h::rest_stk) t res_log_bf res_log local res_glob
            )
          | _ -> (stk, t, log_bf, ["Error"], local, global)
        )

      (* TRY *)
      | Tryend(try_cmdlst) -> 
        (
          (* Empty stack and the current environment *)
          let try_eval = eval [] try_cmdlst log_bf log local global 
          in
          match try_eval with
          | (try_stack_h::try_stack_t, try_prog, try_log_bf, try_log, _, try_glob) -> 
            if try_log = ["Error"] (* Error found *)
            then 
              eval stk t try_log_bf try_log_bf local try_glob 
            else 
              (* No errors raised during evaluation *)
              eval (try_stack_h::stk) t try_log_bf try_log local try_glob 
          | ([], try_prog, try_log_bf, try_log, _, try_glob) -> 
            (stk, t, try_log_bf, ["Error"], local, try_glob) 
        )

      (* SWITCH *)
      | Switchend(se_cmdlist) -> 
        (
          match stk with
          | [] -> (stk, t, log_bf, ["Error"], local, global)
          | head::tail -> 
            (
              match head with
              | Int x -> 
                (
                  match List.find_opt 
                          (fun f ->
                             match f with
                             | Case(Int(some_int), some_prog) -> if some_int = x then true else false 
                             | _ -> false 
                          )
                          se_cmdlist with
                  | Some (Case(Int(check_int), check_prog)) -> eval tail (check_prog @ t) log_bf log local global 
                  | _ -> (stk, t, log_bf, ["Error"], local, global)
                )
              (* Top element on the stack is not an Int *)
              | _ -> (stk, t, log_bf, ["Error"], local, global)
            )
        )
  )

let parse_code = parse(ws >> parse_cmdlst ())

let interp (src : string) : string list = 
  let eval_entry (src : string)
    : stack * prog * string list * string list * env * env = 
    (
      (* match ((parse(ws >> parse_cmdlst ())) src) with *)
      match parse_code src with
      | None -> ([], [], ["Error"], ["Error"], [], [])
      (* | Some(x, y) -> eval [] x [] [] [] [] *)
      | Some (x, y) -> eval [] x [] [] [] []
    )
  in match eval_entry src with
  | (stk, cmdlst, log_bf, log, local, global) -> log

(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src
open System

type parser<'a> = list<char> -> option<'a * list<char>>

let pure' (a : 'a) : 'a parser =
  fun str ->
    Some (a, str)

let (<|>) (f : 'a parser) (g : 'a parser) : 'a parser =
  fun str ->
    match f str with
      | Some (x, str') -> Some (x, str')
      | None -> g str

let get : char parser =
  fun str ->
    match str with
      | [] -> None
      | x :: xs -> Some (x, xs)

let satisfy (p : char -> bool): char parser =
  fun str ->
    get str
    |> Option.bind (fun (c, str') -> if p c then Some (c, str') else None)

let rec many (p : 'a parser) : 'a list parser =
  (fun str ->
   p str
   |> Option.bind (fun (c, str') ->
                   many p str'
                   |> Option.map (fun (cs, str'') -> (c::cs, str'')))
   ) <|> pure' []

let between (open' : 'a parser) (close : 'b parser) (p : 'c parser) : 'c parser =
  fun str ->
    let result =
      open' str
      |> Option.map snd
      |> Option.bind p

    result
    |> Option.map snd
    |> Option.bind close
    |> Option.map2 (fun (r, _) (_, str') -> (r, str')) result
    
let rec chainl1 (p : 'a parser) (op : ('a -> 'a -> 'a) parser) : 'a parser =
  fun str ->

    let rec rest x' =
      let op' =
        x'
        |> Option.map snd
        |> Option.bind op

      let y' =
        op'
        |> Option.map snd
        |> Option.bind p

      let r = Option.map3 (fun (f, _) (a, _) (b, s) -> (f a b, s)) op' x' y'

      if Option.isSome r
      then rest r
      else x'

    let x = p str
    rest x

let rwhitespace (p : 'a parser) : 'a parser =
  fun str ->
    many (satisfy (fun c -> c =' ')) str
    |> Option.defaultValue ([], str)
    |> snd
    |> p

let lwhitespace (p : 'a parser) : 'a parser =
  fun str ->
    let result = p str
    
    result
    |> Option.map snd
    |> Option.bind (many (satisfy (fun c -> c = ' ')))
    |> Option.map2 (fun (v, _) (_, str') -> (v, str')) result

let to_string : char list -> string =
  Array.ofList >> System.String.Concat

let p_int : float parser =
  fun str ->
    let try_parse s = 
      try s |> float |> Some
      with :? FormatException -> None
    
    let auxiliary pair =
      match pair with
        | ([], _) -> None
        | (digits, str') ->
          digits
          |> to_string
          |> try_parse
          |> Option.map (fun f -> (f, str'))
    
    many (satisfy (fun c -> System.Char.IsDigit c || c = '.')) str
    |> Option.bind auxiliary

let p_op (sym : char) (op : 'a -> 'a -> 'a) : ('a -> 'a -> 'a) parser =
  fun str ->
    satisfy (fun c -> c = sym) str
    |> Option.map (fun (_, str') -> (op, str'))

let p_open = rwhitespace (satisfy (fun c -> c ='('))

let p_close = rwhitespace (satisfy (fun c -> c =')'))

let p_add = p_op '+' (+)

let p_sub = p_op '-' (-)

let p_div = p_op '/' (/)

let p_mul = p_op '*' (*)

let p_add_sub = rwhitespace (p_add <|> p_sub)

let p_mul_div = rwhitespace (p_mul <|> p_div)

let rec p_expr str =
  rwhitespace (chainl1 p_mul_div_term p_add_sub) str
and p_mul_div_term str =
  rwhitespace (chainl1 p_term p_mul_div) str
and p_paren =
  rwhitespace (between p_open p_close p_expr)
and p_term =
  rwhitespace (p_paren <|> p_int)

let p_init_expr = lwhitespace p_expr

let parse = p_init_expr << Seq.toList

let rec loop () =
  printf "calculator> "
  let input = System.Console.ReadLine ()

  if input = "q"
  then printfn "Quitting..."; ()
  else
    match parse input with
      | Some (r, []) -> printfn "%f" r; loop ()
      | _ -> printfn "Error: Could not interpret input."; loop ()
    
loop ()

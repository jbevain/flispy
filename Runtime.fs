module FLispy.Runtime

open System

type Expression =
    | Number of int
    | String of string
    | Symbol of Symbol
    | Boolean of bool
    | Function of (Expression list -> Environment -> Expression)
    | List of Expression list
and Symbol = { Name: string }
and Frame = Map<Symbol, Expression ref> ref
and Environment = Frame list

let nil = List ([])

let mutable private symbols = Map.empty

let lookup_symbol str =
    match symbols |> Map.tryFind str with
    | Some (sym) -> sym
    | None ->
        let sym = { Name = str }
        symbols <- Map.add str sym symbols
        sym

let lookupRef sym env =
    match List.tryPick (fun (f: Frame) -> Map.tryFind sym f.Value) env with
    | Some (ref) -> ref
    | None -> sprintf "Symbol %s lookup failure" sym.Name |> failwith 

let lookup sym env =
    let ref = lookupRef sym env
    ref.Value

let appendFrame frame env =
    List.append [ref (frame)] env

let rec to_str result =
    match result with
    | Boolean (b) -> if b then "#t" else "#f"
    | Number (n) -> n.ToString ()
    | String (s) -> "\"" + s + "\""
    | Symbol (s) -> s.Name
    | List (l) ->  List.map to_str l |> String.concat " " |> sprintf "(%s)"
    | Function (_) -> "#<function>"

let rec eval (exp: Expression) (env:Environment) =
    match exp with
    | Number (_) | String (_) | Boolean (_) as literal -> literal
    | Symbol (sym) -> lookup sym env
    | List (h :: t) ->
        match eval h env with 
        | Function (f) -> f t env
        | _ -> h |> to_str |> sprintf "Expression %s is not callable" |> failwith
    | _ -> failwith "Uh oh"

let mapeval exps env =
    List.map (fun exp -> eval exp env) exps

let error func =
    sprintf "Error in function %s" func |> failwith

let car args env =
    match mapeval args env with
    | [List (h :: t)] -> h
    | _ -> error "car"

let cdr args env =
    match mapeval args env with
    | [List (h :: t)] -> List (t)
    | _ -> error "cdr"

let is_list args env =
    mapeval args env |> (function [List (_)] -> true | _ -> false) |> Boolean

let list args env =
    List (mapeval args env)

let is_null args env =
    mapeval args env |> (function [List (l)] -> List.isEmpty l | _ -> false) |> Boolean

let length args env =
    match mapeval args env with
    | [List (l)] -> Number (List.length l)
    | _ -> error "length"

let quote args env =
    match args with
    | [e] -> e
    | _ -> error "quote"

let cons args env =
    match mapeval args env with
    | [a; List (b)] -> List ([a] @ b)
    | _ -> error "cons"

let append args env =
    match mapeval args env with
    | [List (a); List (b)] -> List (a @ b)
    | _ -> error "append"

let if' args env =
    match args with
    | [cond; then'] -> 
        match eval cond env with
        | Boolean (b) -> if b then eval then' env else nil
        | _ -> eval then' env
    | [cond; then'; else'] ->
        match eval cond env with
        | Boolean (b) -> eval (if b then then' else else') env
        | _ -> eval then' env
    | _ -> error "if"

let math args env op =
    match mapeval args env with
    | Number (n) :: nbs ->
        let f x = function Number (y) -> op x y | _ -> error (op.ToString ())
        Number (List.fold f n nbs)
    | _ -> error (op.ToString ())

let not args env =
    match mapeval args env with
    | [Boolean (b)] -> Boolean (not (b))
    | [e] -> Boolean (false)
    | _ -> error "not"

let define args env =

    let update_frame sym value env =
        let frame = List.head env
        let val' = ref (eval value env)
        frame := Map.add sym val' frame.Value

    match args with
    | [Symbol (sym); value] ->
        update_frame sym value env
        nil
    | List (Symbol (sym) :: parms) :: exps ->
        let l = List ([Symbol (lookup_symbol ("lambda")); List (parms); List ([Symbol (lookup_symbol ("begin"))] @ exps)])
        update_frame sym l env
        nil
    | _ -> error "define"

let set args env =
    match args with
    | [Symbol (s); v] ->
        let ref = lookupRef s env
        ref := eval v env
        nil
    | _ -> error "set!"

let begin' args env =
    mapeval args env |> List.rev |> List.head

let eval' args env =
    match mapeval args env with
    | [e] -> eval e env
    | _ -> error "eval"

let apply args env =
    match mapeval args env with
    | Function (f) :: a -> f a env
    | _ -> error "apply"

let lambda args env =
    match args with
    | [List (parms); body] ->
        let lparms = parms |> List.map (function Symbol (s) -> s | _ -> error "lambda")

        Function (fun a e ->
            if List.length lparms <> List.length a then failwith "Argument mismatch"

            let bindings = mapeval a e |> List.map ref |> List.zip lparms |> Map.ofList
            let lenv = appendFrame bindings env

            eval body lenv
        )
    | _ -> error "lambda"

let compare args env op =
    match mapeval args env with
    | [Number (l); Number (r)] -> Boolean (op l r)
    | _ -> error (op.ToString ())

let display args env =
    match mapeval args env with
    | [e] ->
        Console.WriteLine (to_str (e))
        nil
    | _ -> error "display"

let global_env =
    [[
        "+", Function (fun args env -> math args env (+))
        "-", Function (fun args env -> math args env (-))
        "*", Function (fun args env -> math args env (*))
        "/", Function (fun args env -> math args env (/))
        "<", Function (fun args env -> compare args env (<))
        ">", Function (fun args env -> compare args env (>))
        "<=", Function (fun args env -> compare args env (<=))
        ">=", Function (fun args env -> compare args env (>=))
        "append", Function (append)
        "apply", Function (apply)
        "begin", Function (begin')
        "car", Function (car)
        "cdr", Function (cdr)
        "cons", Function (cons)
        "define", Function (define)
        "display", Function (display)
        "eval", Function (eval')
        "if", Function (if')
        "lambda", Function (lambda)
        "length", Function (length)
        "list", Function (list)
        "list?", Function (is_list)
        "not", Function (not)
        "null?", Function (is_null)
        "quote", Function (quote)
        "set!", Function (set)
    ] |> List.map (fun (n, f) -> (lookup_symbol (n), ref (f))) |> Map.ofList |> ref]

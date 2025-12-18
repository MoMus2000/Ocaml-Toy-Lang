open Token

type op =
  | Plus
  | Minus
  | Star
  | Slash

type expression =
  | IntLiteral of int
  | StrLiteral of string
  | PlaceHolder
  | Identifier of string
  | BinaryOp of expression * op * expression
  
type statement =
  | ExpressionStmt of expression

type program = statement list

let match_token token expected =
  match token with
  | t :: rest when t = expected -> Some rest
  | _ -> None

let parse_primary tokens = 
  match tokens with 
  | Int n :: rest -> (IntLiteral n, rest)
  | Str n :: rest -> (StrLiteral n, rest)
  | _ :: rest -> (PlaceHolder, rest)
  | [] -> (PlaceHolder, [])

let parse_expression tokens =
  parse_primary tokens

let string_of_expression expr =
  match expr with
  | IntLiteral n -> string_of_int n
  | StrLiteral s -> s
  | PlaceHolder -> "PlaceHolder"
  | _ -> failwith "TODO"

let string_of_statement stmt =
  match stmt with
  | ExpressionStmt expr -> "Expression: " ^ string_of_expression expr

let parse_statement tokens =
  match tokens with 
  | _ ->
      let (expr, rest) = parse_expression tokens
      in
      (ExpressionStmt expr, rest)
  
let rec parse(tokens: token list) : program =
  match tokens with
    [] -> []
    | _ -> 
      let (stmt, rest) = parse_statement tokens in
        stmt :: parse rest


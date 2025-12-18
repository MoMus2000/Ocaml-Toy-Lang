open Token

type op =
  | Plus
  | Minus
  | Star
  | Slash

type expression =
  | IntLiteral of int
  | StrLiteral of string
  | Identifier of string
  | UnaryOp    of op * expression
  | BinaryOp   of expression * op * expression
  
type statement =
  | ExpressionStmt of expression

type program = statement list

let parse_primary tokens = 
  match tokens with 
  | Int n :: rest -> (IntLiteral n, rest)
  | Str n :: rest -> (StrLiteral n, rest)
  | unexpected :: _ -> failwith ("Unexpected token in primary: " ^ string_of_token unexpected)
  | [] -> failwith "Unexpected end of input in primary"

let rec parse_unary tokens = 
  match tokens with
  | Token.Minus :: rest ->
      let (expr, rest) = parse_unary rest in
      (UnaryOp (Minus, expr), rest)
  | _ -> parse_primary tokens

let string_of_op op =
  match op with 
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"

let rec string_of_expression expr =
  match expr with
  | IntLiteral n -> string_of_int n
  | StrLiteral s -> s
  | UnaryOp (op, expr) -> 
      (match op with
      | Minus -> "-" ^ string_of_expression expr
      | _ -> failwith "Undefined"
      )
  | BinaryOp (lexpr, op, rexpr) ->
      Printf.sprintf "L: %s OP: %s R: %s" (string_of_expression lexpr) (string_of_op op) (string_of_expression rexpr);
  | _ -> failwith "TODO"

let parse_binary tokens = 
  let (lhs, rest) = parse_unary tokens in
   Printf.printf "parse_binary: lhs = %s\n" (string_of_expression lhs);
  let rec loop lhs rest = 
    match rest with
    | (op_token : token) :: rest' when
      (match op_token with
       | Plus | Minus | Star | Slash -> true
       | _ -> false) ->
         let (rhs, rest'') = parse_unary rest' in
         let operator = 
         (match op_token with 
         | Token.Plus -> Plus
         | Token.Minus -> Minus
         | Token.Star -> Star
         | Token.Slash -> Slash
         | _ -> failwith "Unreachable"
         )
         in
         let lhs = BinaryOp (lhs, operator, rhs) in
         loop lhs rest''
    | _ -> (lhs, rest)
  in
  loop lhs rest

let parse_expression tokens =
  parse_binary tokens



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


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

let precedence = function
  | Plus | Minus -> 1
  | Star | Slash -> 2


let token_precedence = function
  | Token.Plus | Token.Minus -> 1
  | Token.Star | Token.Slash -> 2
  | _ -> 0

let is_binary_token = function
  | Token.Plus | Token.Minus | Token.Star | Token.Slash -> true
  | _ -> false

let token_to_operator = function
  | Token.Plus  -> Plus
  | Token.Minus -> Minus
  | Token.Star  -> Star
  | Token.Slash -> Slash
  | undefined -> 
      Printf.printf "not a binary operator %s" (string_of_token undefined);
      failwith "Not a Bin"

let rec parse_binary min_prec tokens =
  let (lhs, rest) = parse_unary tokens in

  let rec loop lhs rest =
    match rest with
    | op_token :: rest'
      when is_binary_token op_token &&
      token_precedence op_token >= min_prec ->

        let prec = token_precedence op_token in
        let operator = token_to_operator op_token in

        let (rhs, rest'') = parse_binary (prec + 1) rest' in

        let lhs = BinaryOp (lhs, operator, rhs) in
        loop lhs rest''

    | _ -> (lhs, rest)
  in
  loop lhs rest


let parse_expression tokens =
  parse_binary 0 tokens

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
    | EOF::_ -> []
    | _ -> 
      let (stmt, rest) = parse_statement tokens in
        stmt :: parse rest


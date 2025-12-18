open Parser

type value =
  | IntValue of int
  | StringValue of string

let eval_unary op expr =
  match (op, expr) with
  | (Minus, IntValue d) -> IntValue (d * -1)
  | (_, _) -> failwith "Undefined"

let eval_binary expr1 op expr2 =
  match (expr1, op, expr2) with
  | (IntValue a, Plus, IntValue b) -> IntValue (a + b)
  | (IntValue a, Minus, IntValue b) -> IntValue (a - b)
  | (IntValue a, Star, IntValue b) -> IntValue (b * a)
  | (IntValue a, Slash, IntValue b) -> IntValue (a / b)
  | (_, _, _) -> failwith "Eval Binary"

let rec evaluate_expression expr =
  match expr with
  | IntLiteral n -> IntValue n
  | StrLiteral n -> StringValue n
  | UnaryOp (op, expr) ->
      let lhs = evaluate_expression expr in
      eval_unary op lhs
  | BinaryOp (exp1, op, exp2) ->
      let lhs = evaluate_expression exp1 in
      let rhs = evaluate_expression exp2 in
      eval_binary lhs op rhs
  | Identifier _ -> failwith "Identifier"

let evaluate (stmt) =
  match stmt with
  | ExpressionStmt stmt ->
      evaluate_expression stmt


type token =
 | Int   of int
 | Float of float
 | Ident of string
 | Plus
 | Str   of string
 | Minus
 | Star
 | Slash
 | Lparen
 | Rparen
 | Fun   of string
 | LBrace
 | RBrace
 | If
 | Else
 | Equal
 | Assign
 | Bang
 | BangEqual
 | SemiColon
 | Var
 | EOF

let string_of_token = function
  | Int i     -> Printf.sprintf "Int(%d) " i
  | Float f   -> Printf.sprintf "Float(%f) " f
  | Ident s   -> Printf.sprintf "Ident(%s) " s
  | Str s     -> Printf.sprintf "String(%s) " s
  | Var       -> "Var "
  | Plus      -> "PLUS "
  | Minus     -> "MINUS "
  | Star      -> "STAR "
  | Slash     -> "SLASH "
  | Lparen    -> "LPAREN "
  | Rparen    -> "RPAREN "
  | Fun _     -> "FUN "
  | LBrace    -> "LBRACE "
  | RBrace    -> "RBRACE "
  | If        -> "IF "
  | Else      -> "ELSE "
  | Assign    -> "ASSIGN "
  | Equal     -> "EQUALS "
  | Bang      -> "BANG "
  | BangEqual -> "BANGEQ "
  | SemiColon -> "SEMICOLON "
  | EOF       -> "EOF "


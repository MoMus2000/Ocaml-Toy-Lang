open Token

let print_file_output content = 
  Printf.printf "%s\n" content

let is_digit c =
  '0' <= c && c <= '9'

let is_alpha_numeric c =
  match c with 
  | 'A' .. 'Z' -> true
  | 'a' .. 'z' -> true
  | '0' .. '9' -> true
  | '_' -> true
  | _ -> false

let peek source pos =
  if pos >= String.length source then
    None
  else
    Some source.[pos]

let rec consume_while pred source i =
  if pred source.[i] && i <= String.length source then begin
    consume_while pred source (i+1)
  end
  else
    i

let rec lex (source: string) (pos: int) : (Token.token * int) =
  if pos >= String.length source then
    (EOF, pos)
  else begin
    let c = source.[pos] in
    match c with 
    | '\n' | '\t' | '\r' -> lex source (pos+1)
    | '(' -> (Lparen, pos + 1)
    | ')' -> (Rparen, pos + 1)
    | ' ' -> lex source (pos + 1)
    | '{' -> (LBrace, pos + 1)
    | '}' -> (RBrace, pos + 1)
    | '+' -> (Plus, pos + 1)
    | '-' -> (Minus, pos + 1)
    | '/' -> (Slash, pos + 1)
    | '*' -> (Star, pos + 1)
    | ';' -> (SemiColon, pos+1)
    | '=' ->
        (
          match (peek source (pos + 1)) with
          | Some '=' -> (Equal, pos+2)
          | None | Some _ -> (Equal, pos+1)
        )
    | '!' ->
        (
          match (peek source (pos + 1)) with
          | Some '=' -> (BangEqual, pos+2)
          | None | Some _ -> (Bang, pos+1)
        )
    | '"' ->
        let start_pos = (pos+1) in
        let end_pos = (consume_while (fun a -> a <> '"') source (start_pos))
        in 
        (
          match (peek source (end_pos)) with
          | Some '"' -> 
            let str = String.sub source start_pos (end_pos - start_pos) in
            (Str (str), end_pos+1)
          | None | Some _ -> 
            let str = String.sub source start_pos (end_pos - start_pos) in
            let message = Printf.sprintf "Unterminated String %s" (str) in
              failwith message
        )

    | c when is_digit c ->
        let end_pos = consume_while is_digit source pos
        in
        let num_str = String.sub source pos (end_pos - pos) in
        (Int (int_of_string num_str), end_pos)

    | c when is_alpha_numeric c ->
        let end_pos = consume_while is_alpha_numeric source pos
        in
        let str = String.sub source pos (end_pos - pos) in
        (
          match str with
          | "fun" -> (Fun (str), end_pos)
          | "if" -> (If, end_pos)
          | "else" -> (Else, end_pos)
          | "var" -> (Var, end_pos)
          | _ -> (Ident (str), end_pos)
        )

    |  _  -> 
        let s = 
          Printf.sprintf "Undefined character %c encountered \n" source.[pos]
        in
        failwith s

  end

let lex_all src = 
  let rec loop acc pos =
    let (tok, pos) = lex src pos in
      match tok with
      | EOF -> 
          List.rev (EOF :: acc)
      | _ -> loop (tok::acc) pos
  in loop [] 0

let entry_point = 
  match Array.length Sys.argv with
  | 1 -> 
      Printf.printf "Need FilePath\n"
  | 2 ->
      let file_path = Sys.argv.(1) in
      let ic = open_in file_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;

      print_file_output content;

      let tokens = lex_all content in
      List.iter (fun t -> Printf.printf "%s" (string_of_token t)) tokens;

      let open Parser in
      let statements = parse tokens in
      print_endline "";
      List.iter (fun stmt -> 
      let open Evaluate in
      let value = evaluate stmt in
        match value with
        | IntValue s -> Printf.printf "%d\n" s
        | _ -> failwith "Yet to Implement"
      ) statements;
      print_endline ""

  | _ ->
      exit(1)


let () =
  entry_point


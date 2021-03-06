let rec zip a b = match a, b with
| [], [] -> []
| x::xs, y::ys -> (x, y)::(zip xs ys)
| _ -> failwith "Bad lenght"


let get_lexing_position lexbuf: (int * int) = 
  let p = Lexing.lexeme_start_p lexbuf in
  (p.pos_lnum, p.pos_cnum - p.pos_bol)

let parse_pos_error (pos: (int * int)) (msg: string): string = 
  let (line, col) = pos in Printf.sprintf "(%d, %d): %s" line col msg
# 1 "zad2.mll"
  
# 4 "zad2.ml"

let rec __ocaml_lex_refill_buf lexbuf _buf _len _curr _last =
  if lexbuf.Lexing.lex_eof_reached then
    256, _buf, _len, _curr, _last
  else begin
    lexbuf.Lexing.lex_curr_pos <- _curr;
    lexbuf.Lexing.lex_last_pos <- _last;
    lexbuf.Lexing.refill_buff lexbuf;
    let _curr = lexbuf.Lexing.lex_curr_pos in
    let _last = lexbuf.Lexing.lex_last_pos in
    let _len = lexbuf.Lexing.lex_buffer_len in
    let _buf = lexbuf.Lexing.lex_buffer in
    if _curr < _len then
      Char.code (Bytes.unsafe_get _buf _curr), _buf, _len, (_curr + 1), _last
    else
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
  end

let rec __ocaml_lex_state6 lexbuf _last_action _buf _len _curr _last =
  let next_char, _buf, _len, _curr, _last =
    if _curr >= _len then
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
    else
      Char.code (Bytes.unsafe_get _buf _curr),
      _buf, _len, (_curr + 1), _last
  in
  begin match next_char with
    (* |'a' *)
    | 'a' ->
      (* *)
      lexbuf.Lexing.lex_curr_pos <- _curr;
      lexbuf.Lexing.lex_last_pos <- _last;
      1
    (* |'b' *)
    | 'b' ->
      __ocaml_lex_state6 lexbuf _last_action _buf _len _curr _last
    | _ ->
      let _curr = _last in
      lexbuf.Lexing.lex_curr_pos <- _curr;
      lexbuf.Lexing.lex_last_pos <- _last;
      _last_action
  end

and __ocaml_lex_state7 lexbuf _last_action _buf _len _curr _last =
  (* *)
  let _last = _curr in
  let _last_action = 0 in
  let next_char, _buf, _len, _curr, _last =
    if _curr >= _len then
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
    else
      Char.code (Bytes.unsafe_get _buf _curr),
      _buf, _len, (_curr + 1), _last
  in
  begin match next_char with
    (* |'a' *)
    | 'a' ->
      let next_char, _buf, _len, _curr, _last =
        if _curr >= _len then
          __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
        else
          Char.code (Bytes.unsafe_get _buf _curr),
          _buf, _len, (_curr + 1), _last
      in
      begin match next_char with
        (* |'b' *)
        | 'b' ->
          let next_char, _buf, _len, _curr, _last =
            if _curr >= _len then
              __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
            else
              Char.code (Bytes.unsafe_get _buf _curr),
              _buf, _len, (_curr + 1), _last
          in
          begin match next_char with
            (* |'a' *)
            | 'a' ->
              __ocaml_lex_state7 lexbuf 0 (* = last_action *) _buf _len _curr _last
            | _ ->
              let _curr = _last in
              lexbuf.Lexing.lex_curr_pos <- _curr;
              lexbuf.Lexing.lex_last_pos <- _last;
              0 (* = last_action *)
          end
        | _ ->
          let _curr = _last in
          lexbuf.Lexing.lex_curr_pos <- _curr;
          lexbuf.Lexing.lex_last_pos <- _last;
          0 (* = last_action *)
      end
    | _ ->
      let _curr = _last in
      lexbuf.Lexing.lex_curr_pos <- _curr;
      lexbuf.Lexing.lex_last_pos <- _last;
      0 (* = last_action *)
  end


let rec language lexbuf =
  let __ocaml_lex_result =
    let _curr = lexbuf.Lexing.lex_curr_pos in
    let _last = _curr in
    let _len = lexbuf.Lexing.lex_buffer_len in
    let _buf = lexbuf.Lexing.lex_buffer in
    let _last_action = -1 in
    lexbuf.Lexing.lex_start_pos <- _curr;
    let next_char, _buf, _len, _curr, _last =
      if _curr >= _len then
        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
      else
        Char.code (Bytes.unsafe_get _buf _curr),
        _buf, _len, (_curr + 1), _last
    in
    begin match next_char with
      (* |'b' *)
      | 'b' ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        3
      (* |eof *)
      | 256 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        4
      (* |'a' *)
      | 'a' ->
        (* *)
        let _last = _curr in
        let _last_action = 2 in
        let next_char, _buf, _len, _curr, _last =
          if _curr >= _len then
            __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
          else
            Char.code (Bytes.unsafe_get _buf _curr),
            _buf, _len, (_curr + 1), _last
        in
        begin match next_char with
          (* |'b' *)
          | 'b' ->
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'a' *)
              | 'a' ->
                __ocaml_lex_state7 lexbuf 2 (* = last_action *) _buf _len _curr _last
              (* |'b' *)
              | 'b' ->
                __ocaml_lex_state6 lexbuf 2 (* = last_action *) _buf _len _curr _last
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                2 (* = last_action *)
            end
          (* |'a' *)
          | 'a' ->
            (* *)
            lexbuf.Lexing.lex_curr_pos <- _curr;
            lexbuf.Lexing.lex_last_pos <- _last;
            1
          | _ ->
            let _curr = _last in
            lexbuf.Lexing.lex_curr_pos <- _curr;
            lexbuf.Lexing.lex_last_pos <- _last;
            2 (* = last_action *)
        end
      | _ ->
        let _curr = _last in
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        _last_action
    end
  in
  begin
    let _curr_p = lexbuf.Lexing.lex_curr_p in
    if _curr_p != Lexing.dummy_pos then begin
      lexbuf.Lexing.lex_start_p <- _curr_p;
      lexbuf.Lexing.lex_curr_p <-
        {_curr_p with Lexing.pos_cnum =
         lexbuf.Lexing.lex_abs_pos+lexbuf.Lexing.lex_curr_pos}
    end
  end;
  match __ocaml_lex_result with
  | 0 ->
let
# 4 "zad2.mll"
                s
# 199 "zad2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 4 "zad2.mll"
                          ( Printf.printf "aba %d times\n" (String.length s / 3))
# 203 "zad2.ml"

  | 1 ->
let
# 5 "zad2.mll"
                   c
# 209 "zad2.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) (lexbuf.Lexing.lex_curr_pos + -1) in
# 5 "zad2.mll"
                          ( Printf.printf "ab*a with %d bs\n" (String.length c) )
# 213 "zad2.ml"

  | 2 ->
# 6 "zad2.mll"
                          ( print_string "a\n"                                  )
# 218 "zad2.ml"

  | 3 ->
# 7 "zad2.mll"
                          ( print_string "b\n"                                  )
# 223 "zad2.ml"

  | 4 ->
# 8 "zad2.mll"
                          ( exit 0                                              )
# 228 "zad2.ml"

  | _ -> raise (Failure "lexing: empty token")


;;

# 9 "zad2.mll"
 
  let main () =
    let lexbuf = Lexing.from_channel stdin in
    while true do
      language lexbuf
    done

  let _ = Printexc.print main ()

# 245 "zad2.ml"

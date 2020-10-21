{ }

rule language = parse
    | "aba"+ as s         { Printf.printf "aba %d times\n" (String.length s / 3)}
    | 'a' ('b'* as c) 'a' { Printf.printf "ab*a with %d bs\n" (String.length c) }
    | 'a'                 { print_string "a\n"                                  }
    | 'b'                 { print_string "b\n"                                  }
    | eof                 { exit 0                                              }
{
  let main () =
    let lexbuf = Lexing.from_channel stdin in
    while true do
      language lexbuf
    done

  let _ = Printexc.print main ()
}

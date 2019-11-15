open Core
let parse filename =
  In_channel.with_file filename ~f:(
    fun chan ->
      let lexbuf = Lexing.from_channel chan in
      let ast = Parser.program Lexer.initial lexbuf in
      ast
  )

open Core
open Printf

let say = prerr_endline

exception EXIT

(* Command line arguments *)

type mode = Typecheck | TypeInfer

type cmdline =
{ 
  mode: mode;
  dynamics: bool;
  verbose: bool;
  dump_ast: bool;
  filename: string;
}

let read_cmdline () =
  let open Getopt in
  try
    let mode = ref Typecheck in
    let dynamics = ref false in
    let verbose = ref false in
    let dump_ast = ref false in
    let filename = ref None in
    let opts = 
    [ ('i', "infer", set mode TypeInfer, None);
      ('d', "dynamics", set dynamics true, None);
      ('v', "verbose",   set verbose true, None);
      ('a', "dump-ast", set dump_ast true, None);
    ] in
    let file_opt f =
       match !filename with
        | None -> filename := Some f
        | Some s -> say "Error: more than one input file"; raise EXIT in

    let () = parse_cmdline opts file_opt in
    { 
      mode = !mode;
      dynamics = !dynamics;
      verbose = !verbose;
      dump_ast = !dump_ast;
      filename = match !filename with
          | Some s -> s
          | None -> say "Error: no input file provided"; raise EXIT }
  with Error s -> say s; raise EXIT

let say_if flag s =
  if flag then say (s ()) else ()

let main cmd =
  try
    let source = cmd.filename in
    say_if cmd.verbose (fun () -> "Parsing... " ^ source);
    let ast = Parse.parse source in
    say_if cmd.dump_ast (fun () -> Ast.to_string ast);
    exit 0


  with
  | EXIT -> exit 1
  | e -> Out_channel.output_string stderr (Exn.to_string e); exit 1


let () = main (read_cmdline ())
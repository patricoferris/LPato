open Core 
open Format 

let process_input file () = 
    let e = Front_end.front_end file in
    let v = Interp_stack.interpret_top e in
    let _ = fprintf std_formatter "%s \n %s" ("🦆 El Pato 🦆") (Interp_stack.string_of_value v) in
    print_flush ()

let () =
  Command.basic_spec ~summary:"🦆 LPATO 🦆"
    Command.Spec.(empty +> anon ("filename" %: file))
    process_input
  |> Command.run

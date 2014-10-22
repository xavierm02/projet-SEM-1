let () =
  let files =
    match Sys.argv |> Array.to_list with
    | [] -> ["exemples/test.toy"]
    | _ :: l -> l
  in
  files |> List.iter (fun file ->
    let tmp = "+" ^ (String.make (2 + String.length file)) '-' ^ "+\n" in
    "\n" ^
    tmp ^
    "| " ^ file ^ " |\n" ^
    tmp ^
    "\n" |> print_string;
    Step.go_step file;
    print_newline (); print_newline (); print_newline (); print_newline (); print_newline ();
  )

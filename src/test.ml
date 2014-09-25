open Step;;

match Array.length Sys.argv with
  | 2 -> go_step Sys.argv.(1)
  | _ -> failwith "Argument needed."

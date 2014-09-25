let () =
  let file = try Sys.argv.(1) with _ -> "exemples/test.toy" in
  Step.go_step file

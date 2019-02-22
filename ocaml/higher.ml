(* A higher/lower guessing game *)

let rec hilo n =
    let () = print_string "Type a number: "
    in let i = read_int ()
       in if i = n
             then let () = print_string "BRAVO"
                  in print_newline ()
             else let () = if i < n
                              then let () = print_string "Higher"
                                   in print_newline ()
                              else let () = print_string "Lower"
                                   in print_newline ()
                  in hilo n
;;

let main () =
    let n = int_of_string Sys.argv.(1) in
    hilo n
;;

if !Sys.interactive then () else main();;

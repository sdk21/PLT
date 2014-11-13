(* David Golub *)

type labels = { mutable next : int }

let new_labels () = { next = 0 }
let add_label ls =
    let labno = ls.next
    in ls.next <- labno + 1;
       labno
let get_label_name n =
    "_label" ^ string_of_int n

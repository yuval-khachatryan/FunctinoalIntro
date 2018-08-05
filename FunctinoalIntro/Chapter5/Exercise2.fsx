(* Solution to exercise 2
   A reimplementation of function revrev from exercise 4.15 usingList.fold or List.foldBack
   Maps a list of lists into reversed list of reversed elements *)

let revrev lst =
    let rev l = List.fold (fun tail x -> x::tail) [] l
    List.fold (fun l x -> (rev x) :: l) [] lst
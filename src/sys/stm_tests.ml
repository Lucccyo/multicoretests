open QCheck
open STM
(* open Util *)

(** parallel STM tests of Sys *)

module AConf =
struct
  type cmd =
    | File_exists of String
    (* | Remove of String
    | Mkdir of String * int *)
  [@@deriving show { with_path = false }]

  type state = char list
  (* type sut = Sys.t *)

  let arb_cmd s =
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(oneof
             [ return File_exists;
  
             ])

  let folder_size = 16

  let init_state  = List.init folder_size (fun _ -> 'a')

  let next_state file_name s = match file_name with
    | File_exists -> List.mem file_name s

  let init_sut () = Sys.mkdir 

  let cleanup _   = ()

  let precond c _s = match c with
    | _ -> true

  let run c file_name = match c with
    | File_exists       -> Res (bool, Sys.file_exists file_name)

  let postcond c (s:char list) res = match c, res with
    | File_exists, Res ((Int,_),i) -> i = List.length s
    | _, _ -> false
end

module ArraySTM = STM.Make(AConf)

;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [ArraySTM.agree_test         ~count ~name:"STM Array test sequential";
    ArraySTM.neg_agree_test_par ~count ~name:"STM Array test parallel" (* this test is expected to fail *)
])

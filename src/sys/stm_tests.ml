open QCheck
open STM
(* open Util *)

(** parallel STM tests of Sys *)

module AConf =
struct
  type cmd =
    | File_exists of string
    | Remove of string
    | Mkdir of string * int
  [@@deriving show { with_path = false }]

  type filesys = 
  | Directory of string * filesys list
  | File      of string
  
  (* type sut = Sys.t *)

  let arb_cmd _s =
    let str_gen = Gen.string in
    let perm_gen = Gen.(oneof [0o111 ; 0o222 ; 0o333 ; 0o444 ; 0o555 ; 0o666 ; 0o777])
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(oneof
             [ map (fun file_name   -> File_exists file_name) str_gen;
               map (fun file_name   -> Remove file_name) str_gen;
               map2 (fun (folder_name, perm) -> Mkdir folder_name) str_gen perm_gen;
             ])
  

  (* let folder_size = 16 *)

  let state = Directory ("/" , [])

  let next_state file_name fs = match file_name with
    | File_exists file_name -> List.mem file_name 
    | Remove ()     -> 
    | Mkdir (f_name, perm) -> 

  let init_sut () = Sys.mkdir 

  let cleanup _   = ()

  let precond c _s = match c with
    | _ -> true

  let run c file_name = match c with
    | File_exists          -> Res (bool, Sys.file_exists file_name)
    | Remove f_name        -> Res (result unit exn, Sys.remove f_name)
    | Mkdir (f_name, perm) -> Res (result unit exn, Sys.mkdir  f_name perm)

  let postcond c (s:char list) res = match c, res with
    | File_exists, Res ((Int,_),i) -> i = List.length s
    | Remove, Res ((Result (Unit,Exn),_),r) -> 
      if 
        then r = Error (Sys_error ":(")
        else r = Ok ()
    | Mkdir, Res ((Result (Unit,Exn),_),r) ->
      if 
        then r = Error (Sys_error ":(")
        else r = Ok ()
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

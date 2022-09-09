open QCheck
open STM
(* open Util *)

(** parallel STM tests of Sys module*)

module SConf =
struct

  type filesys = 
  | Directory of (int * string) * filesys list
  | File      of (int * string)

  type cmd =
    | File_exists of string list * string
    (* | Remove of string *)
    (* | Mkdir of string * int  *)
  [@@deriving show { with_path = false }]

  type state = filesys
  
  type sut = unit

  let arb_cmd _s =
    let  str_gen = Gen.(oneofl ["ccc" ; "hhh" ; "aaa"]) in
    (* let perm_gen = 
      ((Gen.generate1 (Gen.int_bound 7) * 100) + 
       (Gen.generate1 (Gen.int_bound 7) *  10) + 
       (Gen.generate1 (Gen.int_bound 7) *   1)) in *)
       
    (* let rec perm_gen sum mult = 
      match mult with 
      | 1 -> sum + (Gen.int_bound 7)
      | m -> perm_gen (sum + ((Gen.int_bound 7) * m)) (m/10) in *)
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(oneof
      (* change to a good path *)
             [ map (fun file_name -> File_exists ([], file_name)) str_gen;
               (* map  (fun file_name   -> Remove file_name) str_gen; *)
               (* map2 (fun (folder_name, perm) -> Mkdir folder_name) str_gen perm_gen; *)
             ])

  let static_path = (Sys.getcwd ()) ^ "/sandbox"

  let init_state  = Directory ((0o777, "/root") , [])

  let next_state file_name fs = match file_name with
    | File_exists (_path, _file_name) -> fs
    (* | Remove      file_name -> List.filter (fun str -> str != file_name) l !! List.filter return a list and doesn't change the one in place *)
    (* | Mkdir (file_name, perm) -> fs :: Directory ((perm, f_name) , []) *)

  let init_sut () = try Sys.mkdir (static_path ^ "/root") 0o777 with Sys_error _ -> ()

  let cleanup _   = ()

  let precond c _s = match c with
    | _ -> true

  let run c _file_name = match c with
    | File_exists (path_name, file_name) -> 
        Res (bool, Sys.file_exists ((String.concat "/" path_name) ^ "/" ^ file_name))
    (* | Remove f_name        -> Res (result unit exn, Sys.remove f_name) *)
    (* | Mkdir (f_name, perm) -> Res (result unit exn, Sys.mkdir  f_name perm) *)

  let rec filexists (fs_list: filesys list) path file_name = match fs_list with 
    | hd :: tl -> 
      (match hd with
        | Directory ((_perm, dir_name), dir_list) -> (
          try
            if dir_name = List.hd path 
              then filexists dir_list (List.tl path) file_name
              else filexists tl path file_name
          with Failure _ -> false)
        | File (_perm, f_name) -> assert (path = []); f_name = file_name)
    | []      ->  false

  let postcond c (s:filesys) res = match c, res with
    | File_exists (path_name, file_name), Res ((Bool,_),b) -> b = filexists [s] path_name file_name
    (* | Remove, Res ((Result (Unit,Exn),_),r) ->  r = Ok () *)
    (* | Mkdir, Res ((Result (Unit,Exn),_),r) ->
      if ...
        then r = Error (Sys_error ":(")
        else r = Ok () *)
    | _, _ -> false
end

module SysSTM = STM.Make(SConf)

;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main
  (let count = 1 in
   [SysSTM.agree_test         ~count ~name:"STM Sys test sequential";
    (* SysSTM.agree_test_par ~count ~name:"STM Sys test parallel"  *)
])

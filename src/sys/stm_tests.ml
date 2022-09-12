open QCheck
open STM
(* open Util *)

(** parallel STM tests of Sys module*)

module SConf =
struct
(* add path gen *)

  type filesys = 
  | Directory of {
      perm: int;
      dir_name: string;
      mutable dir_list: filesys list}
  | File of {
      perm: int;
      file_name: string}

  type cmd =
    | File_exists of string list * string
    (* | Remove of string *)
    | Mkdir of string list * string * int 
  [@@deriving show { with_path = false }]

  type state = filesys
  
  type sut = unit

  let arb_cmd _s =
    let  str_gen = Gen.(oneofl ["aaa"]) in
    let perm_gen = Gen.(oneofl [0o777; 0o444]) in
    (* let perm_gen = 
      ((Gen.generate1 (Gen.int_bound 7) * 100) + 
       (Gen.generate1 (Gen.int_bound 7) *  10) + 
       (Gen.generate1 (Gen.int_bound 7) *   1)) in *)
       
    (* let rec perm_gen sum mult = 
      match mult with 
      | 1 -> sum + (Gen.int_bound 7)
      | m -> perm_gen (sum + ((Gen.int_bound 7) * m)) (m/10) in *)
    let path_gen = Gen.(oneofl [["root"]]) in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(oneof
      (* change to a good path *)
             [ map2 (fun path file_name -> File_exists (path, file_name))path_gen str_gen;
               (* map  (fun file_name   -> Remove file_name) str_gen; *)
               map3 (fun path dir_name perm -> Mkdir (path, dir_name, perm)) path_gen str_gen perm_gen;
             ])

  let static_path = (Sys.getcwd ()) ^ "/sandbox"

  let init_state  = Directory {perm = 0o777; dir_name = "/root"; dir_list = []}

  let rec find (fs_list: filesys list) path name = match fs_list with  
    | hd :: tl -> 
      (match hd with
        | Directory d -> 
            if path = [] 
              then (if d.dir_name = name 
                      then Some hd
                      else None;)
              else (if d.dir_name = List.hd path
                      then find d.dir_list (List.tl path) name
                      else find tl path name)
        | File f -> if path = [] && f.file_name = name then Some hd else None)
    | []      ->  None

  (* let rec mkdir  (fs_list: filesys list) path dir_name perm = match fs_list with 
    | hd :: tl -> 
      (match hd with
        | Directory d -> (
          try
            if path = [] && not (direxists d.dir_list [] dir_name perm)
              then (d.dir_list <- (List.cons (Directory {perm; dir_name; dir_list = []}) d.dir_list))
              else if dir_name = List.hd path   
                then mkdir d.dir_list (List.tl path) dir_name perm
                else mkdir tl path dir_name perm
          with Sys_error _ -> ())
        | File _f -> ())
    | []      ->  () *)

  let mkdir s path dir_name perm = 
    match (find [s] path dir_name) with 
    | Some fs -> (match fs with 
      | Directory d ->  
        d.dir_list <- (List.cons (Directory {perm; dir_name; dir_list = []}) d.dir_list)
      | File _ -> ())
    | None   -> ()

  let next_state c fs = match c with
    | File_exists (_path, _file_name) -> fs
    (* | Remove      file_name -> List.filter (fun str -> str != file_name) l !! List.filter return a list and doesn't change the one in place *)
    | Mkdir (path, dir_name, perm) -> 
      (* List.iter (Format.printf "%s ") path; Format.printf "\t dir_name = %s\n" dir_name; *)
      (match find [fs] path dir_name with
      | Some _ -> raise Division_by_zero
      | None   -> Format.printf "[NS] on cree %s dans le model\n" dir_name; mkdir fs path dir_name perm; fs)

  (* let rec clear path = match Sys.is_directory path with
  | true ->
    Sys.readdir path |>
    Array.iter (fun name -> clear (Filename.concat path name));
    Unix.rmdir path
  | false -> Sys.remove path *)

  let init_sut () = 
    (* clear (static_path ^ "/root"); *)
    try 
      Sys.mkdir (static_path ^ "/root") 0o777 
    with Sys_error _ -> ()
    

  let cleanup _   = ()

  let precond c _s = match c with
    | _ -> true

  let run c _file_name = match c with
    | File_exists (path_name, file_name) -> 
        Res (bool, Sys.file_exists ((String.concat "/" path_name) ^ "/" ^ file_name))
    (* | Remove f_name        -> Res (result unit exn, Sys.remove f_name) *)
    | Mkdir (path, d_name, perm) -> Format.printf "[RU] on cree %s dans le sut\n" d_name; 
        let path = static_path ^ "/" ^ ((String.concat "/" path) ^ "/" ^ d_name) in
        Res (result unit exn, protect (Sys.mkdir path) perm) 

  (* let rec filexists (fs_list: filesys list) path file_name = match fs_list with 
    | hd :: tl -> 
      (match hd with
        | Directory d -> (
          try
            if d.dir_name = List.hd path 
              then filexists d.dir_list (List.tl path) file_name
              else filexists tl path file_name
          with Failure _ -> false)
        | File f -> assert (path = []); f.file_name = file_name)
    | []      ->  false *)

  (* let rec direxists (fs_list: filesys list) path dir_name perm = match fs_list with 
    | hd :: tl -> 
      (match hd with
        | Directory d -> (
          try
            if path = [] 
              then d.dir_name = dir_name && d.perm = perm 
              else if d.dir_name = List.hd path 
                then direxists d.dir_list (List.tl path) dir_name perm
                else direxists tl path dir_name perm
          with Sys_error _ -> false)
        | File _f -> false)
    | []      ->  false *)

  let filexists s path file_name = 
    match (find [s] path file_name) with 
      | Some _ -> true
      | None   -> false

  let postcond c (s:filesys) res = match c, res with
    | File_exists (path_name, file_name), Res ((Bool,_),b) -> b = filexists s path_name file_name
    (* | Remove, Res ((Result (Unit,Exn),_),r) ->  r = Ok () *)
    | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_),r) ->
      Format.printf "[] %s\n" dir_name;
      (match (find [s] path dir_name) with
        | Some _ -> r = Error (Sys_error (static_path ^ "/root" ^ "/" ^ dir_name ^ ": File exists"))
        | None   -> r = Ok ())
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

open QCheck
open STM

module SConf = 
struct

  type buf_error = {
    mutable file_exists: bool; 
    mutable permission_denied: bool; 
    mutable no_such_file_or_directory: bool
  }[@@deriving show { with_path = false }]
  
  type cmd =
    (* | File_exists of string list * string *)
    | Mkdir of string list * string * int * buf_error
    [@@deriving show { with_path = false }]

  type filesys = 
    | Directory of {
        perm: int;
        dir_name: string;
        mutable dir_list: filesys list
      }
    | File of {
        perm: int;
        file_name: string
      }

  type state = filesys
  
  type sut = unit

  let arb_cmd _s =
      let  str_gen = Gen.(oneofl ["aaa"]) in
      let path_gen = Gen.(oneofl [["/root"]]) in
      let perm_gen = Gen.(oneofl [0o777]) in
      let errors = {file_exists = false; permission_denied = false; no_such_file_or_directory = false} in 
      QCheck.make ~print:show_cmd 
        Gen.(oneof
              [ 
                 (* map2 (fun path file_name -> File_exists (path, file_name))path_gen str_gen; *)
                 map3 (fun path dir_name perm -> Mkdir (path, dir_name, perm, errors)) path_gen str_gen perm_gen;
              ])

  let static_path = (Sys.getcwd ()) ^ "/sandbox"

  let init_state  = Directory {perm = 0o777; dir_name = "/root"; dir_list = []}

  let rec find (fs_list: filesys list) path name e = match fs_list with  
      | hd :: tl -> 
        (match hd with
          | Directory d -> Format.printf "size dir_list in %s : %d\n" d.dir_name (List.length d.dir_list);
            Format.printf "%s--" d.dir_name;
              if path = [] 
                then (if d.dir_name = name 
                        then (Format.printf "1- FOUND\n";Some hd)
                        else (e.no_such_file_or_directory <- true; None;))
                else (if d.dir_name = List.hd path
                        then (Format.printf "PERM\n"; find d.dir_list (List.tl path) name e)
                        else find tl path name e)
          | File f -> Format.printf "file : %s\n" f.file_name; 
                    if path = [] && f.file_name = name 
                      then (Format.printf "2- FOUND\n"; Some hd) 
                      else ( e.no_such_file_or_directory <- true; None))
      | []      ->  e.no_such_file_or_directory <- true; None

  let rec mkdir (fs_list: filesys list) (path: string list) new_dir_name perm e = 
    match fs_list with  
      | hd :: tl -> 
        (match hd with
          | Directory d -> Format.printf "STATUS >>> dir_actual : %s\ttaille dir_list : %d\tet le path taille : %d et y'a : %s\n" d.dir_name (List.length d.dir_list) (List.length path) (List.hd path);
              if ((List.hd path) = d.dir_name) && (List.length path = 1) 
                then ( Format.printf "VICTOIRE !!!\n";
                  d.dir_list <- (List.cons (Directory {perm; dir_name = new_dir_name; dir_list = []}) d.dir_list); Some 1)
                else if (List.hd path) = d.dir_name 
                  then (Format.printf "LA SUITE >>> \n"; mkdir d.dir_list (List.tl path) new_dir_name perm e)
                  else mkdir tl path new_dir_name perm e
          | File _ -> e.no_such_file_or_directory <- true; None)
      | []      ->  e.no_such_file_or_directory <- true; None
    
  let next_state c fs = 
    match c with
      (* | File_exists (path, file_name) -> 
        Format.printf "path : ("; List.iter (Format.printf "%s ") path; Format.printf ")\t file_name : %s\n" file_name;
        fs *)
      | Mkdir (path, dir_name, perm, errors)  ->
        (match find [fs] path dir_name errors with
          | Some _ -> Format.printf "%s already exists\n" dir_name; fs
          | None   -> Format.printf "%s doesn't exists\n" dir_name;
                      Format.printf "ON ENVOIE A MKDIR >>> tete de path : %s\t dirname : %s\n"(List.hd path) dir_name; 
                      (match mkdir [fs] path dir_name perm errors with
                        | Some _ -> fs
                        | None -> fs))

  let init_sut () = 
      try 
        Sys.mkdir (static_path ^ "/root") 0o777 
      with Sys_error _ -> ()

  let cleanup _   = ()

  let precond c _s = match c with
      | _ -> true

  let run c _file_name = match c with
      (* | File_exists (path_name, file_name) -> Format.printf "RUN : %s\n" (static_path  ^ (String.concat "/" path_name) ^ "/" ^ file_name);
          Res (bool, Sys.file_exists (static_path ^ (String.concat "/" path_name) ^ "/" ^ file_name)) *)
      | Mkdir (path, dir_name, perm, _) -> Res (result unit exn, protect (Sys.mkdir (static_path ^ (String.concat "/" path) ^ "/" ^ dir_name))perm)

  (* let filexists s path file_name = 
    let e = {file_exists = false; permission_denied = false; no_such_file_or_directory = false} in
      match (find [s] path file_name e) with 
        | Some _ -> true
        | None   -> false *)

  let postcond c (_s:filesys) res = match c, res with
      (* | File_exists (path_name, file_name), Res ((Bool,_),b) -> Format.printf "RUN RES : %b" b;
        b = filexists s path_name file_name *)
      | Mkdir (_path, dir_name, _perm, errors), Res ((Result (Unit,Exn),_),r) ->
          if errors.file_exists 
            then r = Error (Sys_error (static_path ^ "/root" ^ "/" ^ dir_name ^ ": File exists"))
            else if errors.permission_denied
                  then r = Error (Sys_error (static_path ^ "/root" ^ "/" ^ dir_name ^ ": Permission denied"))
                  else if errors.no_such_file_or_directory
                          then r = Error (Sys_error (static_path ^ "/root" ^ "/" ^ dir_name ^ ": No such file or directory"))
                          else r = Ok ()
      | _,_ -> false
end

module SysSTM = STM.Make(SConf)

;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main
  (let count = 1 in
   [SysSTM.agree_test         ~count ~name:"STM Sys test sequential";
])




(* open QCheck
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
                      else (Format.printf "NO_SUCH_FILE\n"; None;))
              else (if d.dir_name = List.hd path
                      then (Format.printf "PERM\n"; find d.dir_list (List.tl path) name)
                      else find tl path name)
        | File f -> if path = [] && f.file_name = name then Some hd else ( Format.printf "NO_SUCH_FILE\n"; None))
    | []      ->  Format.printf "NO_SUCH_FILE\n"; None


  let rec check_path (fs_list: filesys list) path = match fs_list with  
    | hd :: tl -> 
      (match hd with
        | Directory d -> 
            if path = [] 
              then Some hd
              else (if d.dir_name = List.hd path
                      then (Format.printf "VERIF PERM\n" ; check_path d.dir_list (List.tl path))
                      else check_path tl path)
        | File _ -> if path = [] then Some hd else (Format.printf "NO_SUCH_FILE\n"; None))
    | []      ->  Format.printf "NO_SUCH_FILE\n"; None
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

  (* let rec mkdir fsl path dir_name perm = match fsl with *)
      (* | hd :: tl -> (match hd with
        | Directory d -> 
          if d.dir_name = (List.hd path) 
            then (VERIF PERM; )
        | File f -> ) *)
  let mkdir fs dir_name perm = match fs with
    | Directory d -> d.dir_list <- List.cons (Directory {perm; dir_name; dir_list = []}) d.dir_list
    | File _ -> Format.printf "ON VEUT UN DOSSIER\n"


    (* | hd :: tl -> (match fs with 
      | Directory d ->  
        d.dir_list <- (List.cons (Directory {perm; dir_name; dir_list = []}) d.dir_list)
      | File _ -> ())
    | None   -> () *)

  let next_state c fs = match c with
    | File_exists (_path, _file_name) -> fs
    (* | Remove      file_name -> List.filter (fun str -> str != file_name) l !! List.filter return a list and doesn't change the one in place *)
    | Mkdir (path, dir_name, perm) -> 
      List.iter (Format.printf "%s ") path; Format.printf "\t dir_name = %s\n" dir_name;
      (* let pre_dir  = List.hd (List.rev path) in
      let pre_path = List.tl (List.rev path) in *)
      (match check_path [fs] path with
      | Some s -> Format.printf "[NS] Le path est correct\n"; mkdir s dir_name perm; fs
      | None   -> Format.printf "[NS] Le path est incorrect et on a stocké les erreurs\n"; fs)

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
    | Mkdir (path, dir_name, _perm)Res (result unit exn, protect (Sys.mkdir path) perm) 
      Format.printf "BOUH %s\n" dir_name;
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
]) *)

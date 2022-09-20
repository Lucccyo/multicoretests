open QCheck
open STM

module SConf = 
struct
  
  type cmd =
    | File_exists of string list * string
    (* | Mkdir of string list * string * int  *)
    [@@deriving show { with_path = false }]

  type filesys = 
    | Directory of {
        perm: int;
        dir_name: string;
        fs_list: filesys list }
    | File of {
        perm: int;
        file_name: string }
    [@@deriving show { with_path = false }]
  
  type state = filesys
  
  type sut   = unit  
  
  let arb_cmd _s  =
    let  str_gen = Gen.(oneofl ["bbb";"aaa"]) in
    let path_gen = Gen.(oneofl [["/root"]]) in
    (* let perm_gen = Gen.(oneofl [0o777]) in *)
    QCheck.make ~print:show_cmd 
      Gen.(oneof
            [ 
                map2 (fun path name -> File_exists (path, name)) path_gen str_gen;
                (* map3 (fun path dir_name perm -> Mkdir (path, dir_name, perm)) path_gen str_gen perm_gen; *)
            ])

  let static_path = (Sys.getcwd ()) ^ "/sandbox"

  let init_state  = Directory {perm = 0o777; dir_name = "/root"; fs_list = [Directory {perm = 0o777; dir_name = "aaa"; fs_list = []} ; Directory {perm = 0o777; dir_name = "bbb"; fs_list = []}]}

let rec find (fsl: filesys list) path name = 
  match fsl with 
  | [] -> None 
  | Directory d :: tl -> 
    (match path with
    | [] -> if d.dir_name = name 
              then Some (Directory d)
              else find tl path name
    | hd_path :: tl_path -> if d.dir_name = hd_path
                then find d.fs_list tl_path name
                else find tl path name)
  | File f :: _tl -> if path = [] && f.file_name = name 
      then  Some (File f)
      else None

  (* let rec mkdir (fsl: filesys list) path dir_name perm = match fsl with
    | [] -> []
    | Directory d :: tl -> (match path with
        | hd_path :: tl_path -> if (hd_path = d.dir_name) && (List.length path = 1)
          then (
            let update = Directory {d with fs_list = (Directory {perm; dir_name; fs_list = []} :: d.fs_list)} in
              update :: [])
          else if hd_path = d.dir_name 
                then Directory {d with fs_list = (mkdir d.fs_list tl_path dir_name perm)} :: []
                else Directory d :: (mkdir tl path dir_name perm)
        | _ -> [])
    | File f :: tl     -> File f :: (mkdir tl path dir_name perm) *)
    
  let next_state c fs = 
    match c with
    | File_exists (_path, _name) -> fs
    (* | Mkdir (path, dir_name, perm) -> 
      (match find [fs] path dir_name with
        | Some _ -> fs
        | None   -> Format.printf "---adding %s to %s\n" dir_name (String.concat "/" path); List.hd (mkdir [fs] path dir_name perm)) *)

  let reset_root path = 
    if Sys.file_exists path then (
      let sub_dir = (Sys.readdir path) in
      if not (Array.length sub_dir = 0) 
        then ( 
          Array.iter (fun name -> Unix.rmdir (path ^ "/" ^ name)) sub_dir; 
          Unix.rmdir path)
        else Unix.rmdir path)
    else ()

  let init_sut () = try Sys.mkdir (static_path ^ "/root"    ) 0o777;
                        Sys.mkdir (static_path ^ "/root/aaa") 0o777;
                        Sys.mkdir (static_path ^ "/root/bbb") 0o777 with Sys_error _ -> ()

  let cleanup _   = reset_root (static_path ^ "/root")

  let precond c _s = match c with
      | _ -> true

  let run c _file_name = match c with
      | File_exists (path, name) -> Res (bool, Sys.file_exists (static_path ^ (String.concat "/" path) ^ "/" ^ name))
      (* | Mkdir (path, dir_name, perm) -> 
          Res (result unit exn, protect (Sys.mkdir (static_path ^ (String.concat "/" path) ^ "/" ^ dir_name))perm) *)

  let file_exists (fs: filesys) path name = 
      match (find [fs] path name) with 
        | Some fs -> Format.printf "Finds [%s] returns Some %s\n" name (show_filesys fs); true
        | None   -> Format.printf "Returns NONE\n" ;false

  let postcond c (fs: filesys) res = 
  match c, res with
      | File_exists (path, name), Res ((Bool,_),b) -> Format.printf "[FE]\n"; b = file_exists fs path name
      (* | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_),r) -> Format.printf "[MK]\n";
        (match path with
            | hd_path :: tl_path -> 
                let p = String.concat "/" path in
                if not (file_exists fs tl_path hd_path)
                  then(r = Error (Sys_error (static_path ^ p ^ "/" ^ dir_name ^ ": No such file or directory")))
                  else (if file_exists fs path dir_name
                          then(r = Error (Sys_error (static_path ^ p ^ "/" ^ dir_name ^ ": File exists")))
                          else (r = Ok ()))

            | _ -> r = Error (Sys_error (static_path ^ ( String.concat "/" path) ^ "/" ^ dir_name ^ ": No such file or directory"))) *)
      | _,_ -> false
end

module SysSTM = STM.Make(SConf)

;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main
  (let count = 1 in
   [SysSTM.agree_test         ~count ~name:"STM Sys test sequential";
   (* SysSTM.agree_test_par ~count ~name:"STM Sys test parallel" *)
])

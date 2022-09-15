open QCheck
open STM

module SConf = 
struct
  
  type cmd =
    | File_exists of string list * string
    | Mkdir of string list * string * int 
    [@@deriving show { with_path = false }]

  type filesys = 
    | Directory of {
        perm: int;
        dir_name: string;
        fs_list: filesys list }
    | File of {
        perm: int;
        file_name: string }
  
  type state = filesys
  
  type sut   = unit  

  let static_path = (Sys.getcwd ()) ^ "/sandbox"

  let init_state  = Directory {perm = 0o777; dir_name = "/root"; fs_list = []}
 
  let arb_cmd _s  =
    let  str_gen = Gen.(oneofl ["aaa"]) in
    let path_gen = Gen.(oneofl [["/root"]]) in
    let perm_gen = Gen.(oneofl [0o777]) in
    QCheck.make ~print:show_cmd 
      Gen.(oneof
            [ 
                map2 (fun path file_name -> File_exists (path, file_name)) path_gen str_gen;
                map3 (fun path dir_name perm -> Mkdir (path, dir_name, perm)) path_gen str_gen perm_gen;
            ])

  let rec find (fsl: filesys list) path name = match fsl with 
    | [] -> None 
    | Directory d :: tl -> if path = [] 
        then if d.dir_name = name 
                then Some (Directory d)
                else None
        else if d.dir_name = List.hd path
                then find d.fs_list (List.tl path) name
                else find tl path name
    | File f :: _tl -> if path = [] && f.file_name = name 
        then Some (File f) else None

  let rec mkdir (fsl: filesys list) path dir_name perm = match fsl with
    | [] -> []
    | Directory d :: tl -> if ((List.hd path) = d.dir_name) && (List.length path = 1)
        then let update = Directory {d with fs_list = (Directory {perm; dir_name; fs_list = []} :: d.fs_list)} in
            update :: []
        else if (List.hd path) = d.dir_name 
                then Directory {d with fs_list = (mkdir d.fs_list (List.tl path) dir_name perm)} :: []
                else Directory d :: (mkdir tl path dir_name perm)
    | File f :: tl     -> File f :: (mkdir tl path dir_name perm)
    
  let next_state c fs = match c with
    | File_exists (_path, _file_name) -> fs
    | Mkdir (path, dir_name, perm) -> 
      (match find [fs] path dir_name with
        | Some _ -> fs
        | None   -> List.hd (mkdir [fs] path dir_name perm))

  let rec rmrf path = match Sys.is_directory path with
    | true ->
        Sys.readdir path |>
        Array.iter (fun name -> rmrf (Filename.concat path name));
        Unix.rmdir path
    | false -> Sys.remove path

  let init_sut () = 
    if (Sys.file_exists (static_path ^ "/root")) then rmrf (static_path ^ "/root");
    Sys.mkdir (static_path ^ "/root") 0o777 

  let cleanup _   = ()

  let precond c _s = match c with
      | _ -> true

  let run c _file_name = match c with
      | File_exists (path, name) -> Res (bool, Sys.file_exists (static_path ^ (String.concat "/" path) ^ "/" ^ name))
      | Mkdir (path, dir_name, perm) -> 
          Res (result unit exn, protect (Sys.mkdir (static_path ^ (String.concat "/" path) ^ "/" ^ dir_name))perm)

  let filexists (fs: filesys) path name = 
      match (find [fs] path name) with 
        | Some _ -> true
        | None   -> false

  let postcond c (fs: filesys) res = match c, res with
      | File_exists (path, name), Res ((Bool,_),b) -> b = filexists fs path name
      | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_),r) ->
        let p = String.concat "/" path in
        if not (filexists fs [] "/root")
          then r = Error (Sys_error (static_path ^ p ^ "/" ^ dir_name ^ ": No such file or directory"))
          else if filexists fs path dir_name
                 then r = Error (Sys_error (static_path ^ p ^ "/" ^ dir_name ^ ": File exists"))
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
   SysSTM.agree_test_par ~count ~name:"STM Sys test parallel"
])

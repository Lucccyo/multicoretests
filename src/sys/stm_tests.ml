open QCheck
open STM

module SConf = 
struct
  
  type cmd =
    (* | File_exists of string list * string *)
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

  let init_state  = Format.printf "--INIT STATE\n"; Directory {perm = 0o777; dir_name = "/root"; fs_list = []}

  let init_sut () = Format.printf "--INIT SUT\n"; (try Sys.mkdir (static_path ^ "/root") 0o777 with Sys_error _ -> ());
  Format.printf "sut :Dans root y'a : %d sub dir\n" (Array.length (Sys.readdir (static_path ^ "/root")))

  let arb_cmd _s  =
    Format.printf "--ARB CMD\n";
    let  str_gen = Gen.(oneofl ["ccsc"; "vfdv"]) in
    let path_gen = Gen.(oneofl [["/root"]]) in
    let perm_gen = Gen.(oneofl [0o777]) in
    QCheck.make ~print:show_cmd 
      Gen.(oneof
            [ 
                (* map2 (fun path name -> File_exists (path, name)) path_gen str_gen; *)
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
        then ( Format.printf "on créé %s dasn STATE\n" (static_path ^ (String.concat "/" path) ^ "/" ^ dir_name);
          let update = Directory {d with fs_list = (Directory {perm; dir_name; fs_list = []} :: d.fs_list)} in
            update :: [])
        else if (List.hd path) = d.dir_name 
                then Directory {d with fs_list = (mkdir d.fs_list (List.tl path) dir_name perm)} :: []
                else Directory d :: (mkdir tl path dir_name perm)
    | File f :: tl     -> File f :: (mkdir tl path dir_name perm)
    
  let next_state c fs = 
    Format.printf "--NEXT STATE\n";
    match c with
    (* | File_exists (_path, _name) -> fs *)
    | Mkdir (path, dir_name, perm) -> 
      (match find [fs] path dir_name with
        | Some _ -> fs
        | None   -> List.hd (mkdir [fs] path dir_name perm))

  let reset_root path = 
    if Sys.file_exists path then (
      let sub_dir = (Sys.readdir path) in
      if not (Array.length sub_dir = 0) 
        then ( 
          Array.iter (fun name -> Unix.rmdir (path ^ "/" ^ name)) sub_dir; 
          Unix.rmdir path)
        else Unix.rmdir path)
    else ()
    (* Sys.mkdir path 0o777 *)

  let cleanup _   = Format.printf "--CLEANUP\n"; reset_root (static_path ^ "/root")

  let precond c _s = match c with
      | _ -> true

  let run c _file_name = Format.printf "--RUN\n"; match c with
      (* | File_exists (path, name) -> Res (bool, Sys.file_exists (static_path ^ (String.concat "/" path) ^ "/" ^ name)) *)
      | Mkdir (path, dir_name, perm) -> Format.printf "on créé %s dasn SUT\n" (static_path ^ (String.concat "/" path) ^ "/" ^ dir_name);
          Res (result unit exn, protect (Sys.mkdir (static_path ^ (String.concat "/" path) ^ "/" ^ dir_name))perm)

  let filexists (fs: filesys) path name = 
      match (find [fs] path name) with 
        | Some _ -> true
        | None   -> false

  let postcond c (fs: filesys) res = Format.printf "--POST COND\n"; 
  (match fs with 
  | Directory d -> Format.printf "state : dans %s y'a %d sous trucs\n" d.dir_name (List.length d.fs_list)
  | _ -> ());
  match c, res with
      (* | File_exists (path, name), Res ((Bool,_),b) -> b = filexists fs path name *)
      | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_),r) ->
        let p = String.concat "/" path in
        if not (filexists fs (List.tl path) (List.hd path))
          then(Format.printf "ERR No such file\n"; r = Error (Sys_error (static_path ^ p ^ "/" ^ dir_name ^ ": No such file or directory")))
          else if filexists fs path dir_name
                 then(Format.printf "<<<  ERR %s\n"(static_path ^ p ^ "/" ^ dir_name ^ ": File exists"); r = Error (Sys_error (static_path ^ p ^ "/" ^ dir_name ^ ": File exists")))
                 else (Format.printf "ERR OK\n";r = Ok ())
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

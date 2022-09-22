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
    [@@deriving show { with_path = false }]
  
  type state = filesys
  
  type sut   = unit  
  
  let arb_cmd _s  =
    let  str_gen = Gen.(oneofl ["a" ; "b" ; "c"]) in
    let  name_gen = Gen.(oneofl ["aaa" ; "bbb" ; "ccc" ; "ddd" ; "eee"]) in
    (* let path_gen = Gen.(oneofl [["root"] ; ["root" ; "aaa"] ; ["root" ; "bbb"] ; ["root" ; "b" ; "c" ; "b"]]) in *)
    let path_gen = Gen.map (fun path -> "root" :: path) (Gen.list_size (Gen.int_bound 5) name_gen) in
    let perm_gen = Gen.(oneofl [0o777]) in
    (* let perm_gen = Gen.map3 (fun d1 d2 d3 -> d1*100 + d2*10 + d3*1) (Gen.int_bound 7) (Gen.int_bound 7) (Gen.int_bound 7) in *)
    QCheck.make ~print:show_cmd 
      Gen.(oneof
            [ 
                map2 (fun path name -> File_exists (path, name)) path_gen str_gen;
                map3 (fun path dir_name perm -> Mkdir (path, dir_name, perm)) path_gen str_gen perm_gen;
            ])

  let static_path = (Sys.getcwd ()) ^ "/sandbox"

  let init_state  = 
    Directory {perm = 0o777; dir_name = "root"; fs_list = []}

  let rec is_perm_ok (fsl: filesys list) path = 
    match fsl with
    | [] -> true 
    | Directory d :: tl -> (match path with
      | [] -> true
      | hd_path :: tl_path -> if d.dir_name = hd_path
        then if d.perm > 447
             then is_perm_ok d.fs_list tl_path
             else false
        else is_perm_ok tl path
      )
    | File f :: tl -> (match path with
      | [] -> true
      | hd_path :: _tl_path -> if f.file_name = hd_path
        then (assert (List.length path = 1); true)
        else is_perm_ok tl path
      )

  let rec find (fsl: filesys list) path name = 
    match fsl with 
    | [] ->
      (* Format.printf "on a pas trouvé %s\n"name;  *)
      None 
    | Directory d :: tl -> (match path with
      | [] -> if d.dir_name = name 
        then Some (Directory d)
        else find tl path name
      | hd_path :: tl_path -> if d.dir_name = hd_path
        then if d.perm > 447
             then find d.fs_list tl_path name
             else None
        else find tl path name)
    | File f :: _tl -> if path = [] && f.file_name = name 
        then Some (File f)
        else None

  let rec mkdir (fsl: filesys list) path dir_name perm = match fsl with
    | [] -> []
    | Directory d :: tl -> (match path with
      | hd_path :: tl_path -> if (hd_path = d.dir_name) && (List.length path = 1)
        then (
          let b = perm > 447 in
          if b
          then (
            let update = Directory {d with fs_list = (Directory {perm; dir_name; fs_list = []} :: d.fs_list)} in
            update :: [])
          else Directory d :: [])
        else if hd_path = d.dir_name 
             then Directory {d with fs_list = (mkdir d.fs_list tl_path dir_name perm)} :: []
             else Directory d :: (mkdir tl path dir_name perm)
      | _ -> [])
    | File f :: tl -> File f :: (mkdir tl path dir_name perm)
    
  let next_state c fs = 
    match c with
    | File_exists (_path, _name) -> fs
    | Mkdir (path, dir_name, perm) -> (match find [fs] path dir_name with
      | Some _ -> fs
      | None   -> List.hd (mkdir [fs] path dir_name perm))

  let reset_root path = Sys.command ("rm -r -d -f " ^ path)

  let init_sut () = try Sys.mkdir (static_path ^ "/" ^ "root") 0o777 with Sys_error _ -> ()

  let cleanup _   =ignore (reset_root (static_path ^ "/" ^ "root"))

  let precond _c _s = true 

  let run c _file_name = match c with
    | File_exists (path, name) -> Res (bool, Sys.file_exists (static_path ^ "/" ^ (String.concat "/" path) ^ "/" ^ name))
    | Mkdir (path, dir_name, perm) -> 
      (* Format.printf "ce quon mkdir :: %s\n" (static_path ^ "/" ^ (String.concat "/" path) ^ "/" ^ dir_name); *)
      Res (result unit exn, protect (Sys.mkdir (static_path ^ "/" ^ (String.concat "/" path) ^ "/" ^ dir_name))perm)

  let file_exists (fs: filesys) path name = 
    match (find [fs] path name) with 
    | Some _ -> true
    | None   -> false

  let postcond c (fs: filesys) res = 
    let p path dir_name = static_path ^ "/" ^ (String.concat "/" path) ^ "/" ^ dir_name in
    (* Format.printf "\n\n\nSTATUS >> %s\n" (show_filesys fs); *)
    match c, res with
    | File_exists (path, name), Res ((Bool,_),b) -> 
      (* Format.printf "\n\npath :: %s \t FILEXISTS COMMAND : %b\t %s\n" (p path name) (file_exists fs path name) (show_filesys fs);  *)
           b = file_exists fs path name

    | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p path dir_name) ^ ": Permission denied"         -> 
        (* Format.printf "\n\npath :: %s \t PERM %s\n" ((p path dir_name) ^ ": Permission denied") (show_filesys fs); *)
        let b = not (is_perm_ok [fs] path) in
        assert (b);
        b

    | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p path dir_name) ^ ": File exists"               -> 
        (* Format.printf "\n\npath :: %s \t FILE EXIST %s\n" ((p path dir_name) ^ ": File exists" ) (show_filesys fs); *)
        let b = file_exists fs path dir_name in
        assert (b);
        b

    | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_), Error (Sys_error (s) ))
      when s = (p path dir_name) ^ ": No such file or directory" -> 
        (* Format.printf "\n\npath :: %s \t NO SUCH %s\n" ((p path dir_name)^ ": No such file or directory") (show_filesys fs); *)
        let b = match path with
        | [] -> false
        | hd_path :: tl_path -> not (file_exists fs tl_path hd_path) in
        assert (b);
        b

    | Mkdir (path, dir_name, _perm), Res ((Result (Unit,Exn),_), Ok ()) -> 
      assert (is_perm_ok [fs] path); (*good perm*)
      assert (not (file_exists fs path dir_name)); (*not already exists*)
      assert (match path with (*path is good*)
        | [] -> true
        | hd_path :: tl_path -> file_exists fs tl_path hd_path);
      (* Format.printf "\n\n OKKK\n" ;  *)
      true
    | Mkdir (_path, _dir_name, _perm), Res ((Result (Unit,Exn),_), _r) -> assert(false)
    | _,_ -> false
end

module SysSTM = STM.Make(SConf)

;;
Util.set_ci_printing ()
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [SysSTM.agree_test         ~count ~name:"STM Sys test sequential";
   (* SysSTM.agree_test_par ~count ~name:"STM Sys test parallel" *)
])

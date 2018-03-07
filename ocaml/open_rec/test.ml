module Record = struct
  type 't t ='t Map.M (String).t [@@deriving sexp]
  type 't alist = (string * 't) list [@@deriving yojson]

  let to_yojson
      (yojson_of_t : 't -> Yojson.Safe.json)
      (x : 't t)
    : Yojson.Safe.json  =
    (alist_to_yojson yojson_of_t) (Map.to_alist x)

  let of_yojson
      (t_of_yojson : Yojson.Safe.json ->
       't Ppx_deriving_yojson_runtime.error_or)
      (x : Yojson.Safe.json)
    : 't t Ppx_deriving_yojson_runtime.error_or =
    let ls =
      match (alist_of_yojson t_of_yojson x) with
      | Ok ls -> ls
      | _ -> failwith "record_of_yojson : failure" in
    Ok (Map.of_alist_exn (module String) ls)
end

type 't record' = 't Record.t [@@deriving sexp]
let record'_to_yojson = Record.to_yojson
let record'_of_yojson = Record.of_yojson

type 'record t' = [
  | `Int of int
  | `String of string
  | `Record of 'record
][@@deriving sexp, yojson]

type t = t record' t'
[@@deriving sexp,yojson]

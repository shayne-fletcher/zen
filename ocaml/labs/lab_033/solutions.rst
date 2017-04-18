(*Joel Bjornson*)
module type RESULT = sig

  type ('a, 'b) result =
    ('a, 'b) Pervasives.result = | Ok of 'a | Error of 'b

  (**[>>] is [R.( >>| ) ]*)
  val ( >>| ) : ('a, 'b) result -> ('a -> 'c)  -> ('c, 'b) result

  (**[>>=] is [R.( >>= ) ]*)
  val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result)  -> ('c, 'b) result

  module R : sig
    (**Type alias*)
    type ('a, 'b) t = ('a, 'b) result

    (**[ok a] is [Ok a]*)
    val ok : 'a -> ('a, 'b) t

    (**[error b] is [Error b]*)
    val error : 'b -> ('a, 'b) t

    (**[return a] is [ok a]*)
    val return : 'a -> ('a, 'b) t

    (**[fail b] is [error b]*)
    val fail : 'b -> ('a, 'b) t

    (**[reword_error reword r] is [r] if [r = Ok _] else if [r = Error e] then [Error (reword e)] *)
    val reword_error : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

    (**[get_ok r] is [v] if [r = Ok v] else @raise Invalid_argument otherwise*)
    val get_ok : ('a, 'b) t -> 'a

    (**[get_error r] is [b] if [r = Error b] else @raise Invalid_argument otherwise*)
    val get_error : ('a, 'b) t -> 'b

    (**[map f r] if [r = Ok x] is [Ok (f x)] else [r]*)
    val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t

    (**[bind r f] if [r = Ok x] is [f x] else [r]*)
    val bind : ('a, 'b) t -> ('a -> ('c, 'b) t)  -> ('c, 'b) t

    (**[join r] if [r = Ok x] is [x] else [r]*)
    val join : (('a, 'b) t, 'b) t -> ('a, 'b) t

    (**[r >>| f] is [map f r]*)
    val ( >>| ) : ('a, 'b) t -> ('a -> 'c)  -> ('c, 'b) t

    (**[r >>= f] is [bind r f]*)
    val ( >>= ) : ('a, 'b) t -> ('a -> ('c, 'b) t)  -> ('c, 'b) t

    (**A type for error messages*)
    type msg = [`Msg of string]

    (**[msg s] returns an error message of [s]*)
    val msg : string -> [> msg]

    (**[msgf fmt] computes a function according to a format. ['a] unifies with the function type, [[> msg]] is the return type of the function computed*)
    val msgf : ('a, Format.formatter, unit, [> msg]) format4 -> 'a

    val pp_msg : Format.formatter -> msg -> unit

    (**[error_msg s] is [R.error (`Msg s)] *)
    val error_msg : string -> ('a, [> msg]) result

    val error_msgf : ('a, Format.formatter, unit, ('d, [> msg]) result) format4 -> 'a

    val reword_error_msg : ?replace:bool -> (string -> msg) -> ('a, msg) result -> ('a, [> msg]) result

    val error_to_msg : pp_error:(Format.formatter -> 'b -> unit) -> ('a, 'b) result -> ('a, [> msg]) result

    val error_msg_to_invalid_arg : ('a, msg) result -> 'a

    val open_error_msg : ('a, msg) result -> ('a, [> msg]) result

    type exn_trap = [`Exn_trap of exn * Printexc.raw_backtrace]

    val pp_exn_trap : Format.formatter -> exn_trap -> unit

    val trap_exn : ('a -> 'b) -> 'a -> ('b, [> exn_trap]) result

    val error_exn_trap_to_msg : ('a, exn_trap) result -> ('a, [> msg]) result

  (**{1 Pretty printing}*)

    val pp_result :
      ok : (Format.formatter -> 'a -> unit) ->
      error : (Format.formatter -> 'b -> unit) ->
      Format.formatter -> ('a, 'b) result -> unit

    val is_ok : ('a, 'b) result -> bool

    val is_error : ('a, 'b) result -> bool

    val compare :
      ok : ('a -> 'a -> int) ->
      error : ('b -> 'b -> int) ->
      ('a, 'b) result -> ('a, 'b) result -> int

    val to_option : ('a, 'b) result -> 'a option

    val of_option :
      none : (unit -> ('a, 'b) result) -> 'a option -> ('a, 'b) result

    val to_presult : ('a, 'b) result -> [> `Ok of 'a | `Error of 'b]

    val of_presult : [< `Ok of 'a | `Error of 'b] -> ('a, 'b) result

    val ignore_error : use : ('b -> 'a) -> ('a, 'b) result -> 'a

    val kignore_error :
      use : ('b -> ('a, 'c) result) -> ('a, 'b) result -> ('a, 'c) result

  end

end

module Result : RESULT = struct

  type ('a, 'b) result = ('a, 'b) Pervasives.result = | Ok of 'a | Error of 'b

  let map f = function
    | Ok x    -> Ok (f x)
    | Error y -> Error y

  let join = function
    | Ok r    -> r
    | Error y -> Error y

  let ( >>= ) r f = join @@ map f r

  let ( >>| ) r f = map f r

  module R = struct
    type ('a, 'b) t = ('a, 'b) result

    let ok x = Ok x

    let error e = Error e

    let return x = ok x

    let fail e = error e

    let reword_error f = function
      | Ok x -> Ok x
      | Error e -> Error (f e)

    let get_ok = function
      | Ok x -> x
      | Error _ -> raise (Invalid_argument "get_ok")

    let get_error = function
      | Ok _ -> raise (Invalid_argument "get_error")
      | Error e -> e

    let map f = map f

    let bind r = (>>=) r

    let join r = join r

    let ( >>| ) r f = map f r

    let ( >>= ) r = (>>=) r

    type msg = [`Msg of string]

    let msg s = `Msg s

    let msgf f =
      Format.kfprintf
        (fun _ -> msg @@ Format.flush_str_formatter ())
        Format.str_formatter
        f

    let pp_msg (f : Format.formatter) = function
      | `Msg s -> Format.fprintf f "%s" s

    let error_msg s = error @@ msg s

    let error_msgf f =
      Format.kfprintf
        (fun _ -> error_msg @@ Format.flush_str_formatter ())
        Format.str_formatter
        f

    let reword_error_msg ?(replace = false) f = function
      | Ok x  -> Ok x
      | Error (`Msg s)  when replace -> error_msg s
      | Error (`Msg s) -> Error (`Msg s)

    let error_to_msg ~pp_error = function
      | Ok x ->
        Ok x
      | Error x ->
        pp_error Format.str_formatter x;
        error_msg @@ Format.flush_str_formatter ()

    let error_msg_to_invalid_arg = function
      | Ok x -> x
      | Error m -> raise (Invalid_argument "get_ok")

    let open_error_msg = function
      | Ok x -> Ok x
      | Error (`Msg x) -> Error (`Msg x)

    type exn_trap = [`Exn_trap of exn * Printexc.raw_backtrace]

    let pp_exn_trap fmt = function
      | `Exn_trap (e, bt) ->
        let es = Printexc.to_string e in
        let bts = Printexc.raw_backtrace_to_string bt in
        Format.fprintf fmt "%s %s" es bts

    let trap_exn f x =
      try
        Ok (f x)
      with
      | e ->
        Error (`Exn_trap (e, Printexc.get_raw_backtrace()))

    let error_exn_trap_to_msg = function
      | Ok x -> ok x
      | Error et ->
        pp_exn_trap Format.str_formatter et;
        error_msg @@ Format.flush_str_formatter ()

    let pp_result ~ok ~error f = function
      | Ok x    -> ok f x
      | Error e -> error f e

    let is_ok = function
      | Ok _    -> true
      | Error _ -> false

    let is_error r = not @@ is_ok r

    let compare ~ok ~error r1 r2 = 0

    let to_option = function
      | Ok x    -> Some x
      | Error _ -> None

    let of_option ~none  = function
      | Some x  -> Ok x
      | None    -> none ()

    let to_presult = function
      | Ok x -> `Ok x
      | Error e -> `Error e

    let of_presult = function
      | `Ok x -> Ok x
      | `Error e -> Error e

    let ignore_error ~use = function
      | Ok x    -> x
      | Error e -> use e

    let kignore_error ~use = function
      | Ok x -> ok x
      | Error e -> use e

  end
end

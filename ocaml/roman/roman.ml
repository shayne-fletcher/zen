let explode =
  fun s ->
    let n = String.length s in
    let rec loop acc i =
      if i = n then List.rev acc
      else loop (String.get s i :: acc) (i + 1) in
    loop [] 0

let dec_of_sym c =
  match c with
  | 'I' -> 1
  | 'V' -> 5
  | 'X' -> 10
  | 'L' -> 50
  | 'C' -> 100
  | 'D' -> 500
  | 'M' -> 1000
  | _ -> failwith (Printf.sprintf "dec_of_char : unrecognized symbol %c" c)

let zip xs ys =
  let rec zip_rec acc xs ys =
    match xs, ys with
    | x :: _, [] -> List.rev ((x, None) :: acc)
    | x :: xs, y :: ys -> zip_rec ((x, Some y) :: acc) xs ys
    | _ -> failwith "zip : unexpected"
  in zip_rec [] xs ys

let dec_of_rom ps =
  let rec dec_of_rom_rec acc ps =
    match ps with
    | [] -> acc
    | (curr, next) :: ps ->
      match next with
      | None ->
        dec_of_rom_rec (acc + (dec_of_sym curr)) ps
      | Some next ->
        let x = dec_of_sym curr
        and y = dec_of_sym next in
        if x < y then
          dec_of_rom_rec (acc + y - x) (List.tl_exn ps)
        else
          dec_of_rom_rec (acc + x) ps in
  dec_of_rom_rec 0 ps

let convert s =
  let sym = explode s in
  if List.length sym <= 1 then
    dec_of_sym (List.hd_exn sym)
  else
    dec_of_rom  (zip sym (List.tl_exn sym))

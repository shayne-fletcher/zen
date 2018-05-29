open! Core
open! Soup.Infix

let pre soup =
  soup $$ "pre" |>
  Soup.filter (fun n -> Soup.count (n $$ "code") = 0) |>
  Soup.iter (fun n ->
      let buf = Buffer.create 1024 in
      let code = Soup.require (Soup.leaf_text n) in
      (Odoc_ocamlhtml.html_of_code buf ~with_pre:true code;
       let node = Soup.parse (Buffer.contents buf) in
       Soup.clear n;
       Soup.append_child n node)
    )

let code soup =
  soup $$ "code" |>
  Soup.filter (fun n -> List.is_empty (Soup.classes n)) |>
  Soup.iter (fun n ->
      Soup.replace n (
        Soup.create_element "code"
          ~class_:"code"
          ~inner_text:(Soup.require (Soup.leaf_text n))
      )
    )

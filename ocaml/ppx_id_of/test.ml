type t = A [@id 2] | B of int [@id 4] [@@id_of]

module M = struct
  type t =
  | Foo of int [@id 42]
  | Bar [@id 43] [@@id_of]

  module N = struct
    type t = Baz [@id 8] | Quux of string * int [@id 7] [@@id_of]

    module Q = struct
      type t = U [@id 0] [@@id_of]
    end
  end
end;;

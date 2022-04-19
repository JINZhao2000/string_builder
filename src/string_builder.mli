type string_builder
val word: string -> string_builder
val concat: string_builder -> string_builder -> string_builder
val char_at: string_builder -> int -> char
val sub_string: string_builder -> int -> int -> string_builder
val cost: string_builder -> int
val random_string: int -> string_builder
val list_of_string: string_builder -> string list
val balance: string_builder -> string_builder
val gains: int -> int -> int * int * float * int
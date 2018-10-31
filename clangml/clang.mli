include module type of struct
  include Clang__bindings
end

val iter_children : (cxcursor -> unit) -> cxcursor -> unit

val list_of_children : cxcursor -> cxcursor list

val iter_type_fields : (cxcursor -> unit) -> cxtype -> unit

val list_of_type_fields : cxtype -> cxcursor list

module Ast : sig
  include module type of struct
    include Clang__ast
  end

  val of_cxtranslationunit : cxtranslationunit -> translation_unit
end

include module type of struct
  include Clang__bindings
end

val list_of_children : cxcursor -> cxcursor list

module Ast : sig
  include module type of struct
    include Clang__ast
  end

  val of_cxtranslationunit : cxtranslationunit -> translation_unit
end

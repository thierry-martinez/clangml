open Clang
open Clang.Ast

let () =
  Format.printf "%a" Clang.Printer.translation_unit [%c {|int x = 4;|}]

let () =
  Format.printf "%a" Clang.Printer.translation_unit [%c {|
int f(int x) {
  x++;
}
|}]

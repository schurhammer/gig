import ast
import closure_conversion
import codegen
import core as c
import glance
import monomorphise

import shellout

import gleam/dict
import gleam/io
import gleam/string

pub fn read_file(file_name: String) -> String {
  let assert Ok(content) = shellout.command("cat", [file_name], ".", [])
  content
}

const bool = c.TypeApp("Bool", [])

const int = c.TypeApp("Int", [])

pub const prelude = [
  #("panic", c.Poly(1, c.Mono(c.TypeFun(c.TypeVar(1), [])))),
  #("equal", c.Poly(1, c.Mono(c.TypeFun(bool, [c.TypeVar(1), c.TypeVar(1)])))),
  // bool
  #("True", c.Mono(bool)), #("False", c.Mono(bool)),
  #("and_bool", c.Mono(c.TypeFun(bool, [bool, bool]))),
  #("or_bool", c.Mono(c.TypeFun(bool, [bool, bool]))),
  // int
  #("add_int", c.Mono(c.TypeFun(int, [int, int]))),
  #("sub_int", c.Mono(c.TypeFun(int, [int, int]))),
  #("mul_int", c.Mono(c.TypeFun(int, [int, int]))),
  #("div_int", c.Mono(c.TypeFun(int, [int, int]))),
  #("print_int", c.Mono(c.TypeFun(int, [int]))),
]

pub fn compile(gleam_file_name: String) {
  let input = read_file(gleam_file_name)
  let assert Ok(module) = glance.module(input)
  let core = ast.module_to_core(module)
  let assert Ok(typed) = c.w_module(dict.from_list(prelude), core)
  let mono = monomorphise.run(typed)
  let cc = closure_conversion.cc_module(mono)
  let code = codegen.module(cc)

  let template = read_file("./src/gig.c")

  let output = string.replace(template, "///CODEGEN_CONTENT///", code)

  // output the c file
  let assert [file_name, ..] = string.split(gleam_file_name, ".")
  let c_file = file_name <> ".c"
  let cmd = "echo '" <> output <> "' > " <> c_file
  let assert Ok(_) = shellout.command("bash", ["-c", cmd], ".", [])

  // compile the c file
  let assert Ok(_) = shellout.command("gcc", ["-o", file_name, c_file], ".", [])
}

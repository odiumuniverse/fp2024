(** Copyright 2024-2025, Azamat Ishbaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let space = function
  | ' ' | '\n' | '\t' -> true
  | _ -> false
;;

let is_keyword = function
  | "let" | "rec" | "fun" | "if" | "then" | "else" | "true" | "false" | "in" -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_mes = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let skip_spaces = skip_while space
let parse_token p = skip_spaces *> p
let parse_strtoken s = skip_spaces *> Angstrom.string s
let parse_parens p = parse_strtoken "(" *> p <* parse_strtoken ")"

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let parse_bool =
  parse_token
    (choice
       [ parse_strtoken "true" *> return true; parse_strtoken "false" *> return false ])
  >>| fun x -> ConstBool x
;;

let parse_int = parse_token (take_while1 is_digit) >>| fun x -> ConstInt (int_of_string x)
let parse_unit = parse_strtoken "()" *> return ConstUnit
let parse_nil = parse_strtoken "[]" *> return ConstNil

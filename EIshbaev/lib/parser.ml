(** Copyright 2024-2025, Azamat Ishbaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
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

let skip_spaces = skip_while space
let ptoken p = skip_spaces *> p
let pstrtoken s = skip_spaces *> string s
let pparens p = pstrtoken "(" *> p <* pstrtoken ")"
let parse_int = ptoken (take_while1 is_digit) >>| fun x -> ConstInt (int_of_string x)
let parse_unit = pstrtoken "()" *> return ConstUnit
let parse_nil = pstrtoken "[]" *> return ConstNil

let parse_bool =
  ptoken (choice [ pstrtoken "true" *> return true; pstrtoken "false" *> return false ])
  >>| fun x -> ConstBool x
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let parse_name =
  let first_char = satisfy (fun ch -> Char.is_alpha ch) >>| fun ch -> Char.escaped ch in
  let remainder =
    take_while (fun ch -> Char.is_alpha ch || Char.is_digit ch || Char.equal ch '_')
  in
  ptoken @@ lift2 (fun x y -> x ^ y) first_char remainder
  >>= fun str -> if is_keyword str then fail "unlucky botay fp" else return str
;;

let parse_const = parse_int <|> parse_unit <|> parse_nil >>| fun x -> ExprConst x
let parse_var = parse_name >>| fun e -> ExprVar e

let parse_let pexpr =
  let rec pbody pexpr =
    parse_name
    >>= fun name -> pbody pexpr <|> pstrtoken "=" *> pexpr >>| fun e -> ExprFunc (name, e)
  in
  pstrtoken "let"
  *> lift4
       (fun r name e1 e2 -> ExprLet (r, name, e1, e2))
       (pstrtoken "rec" *> return Rec <|> return NotRec)
       (pstrtoken "()" <|> parse_name)
       (pstrtoken "=" *> pexpr <|> pbody pexpr)
       (pstrtoken "in" *> pexpr >>| (fun x -> Some x) <|> return None)
;;

(** If - Then - Else parse *)
let parse_branch pexpr =
  ptoken
  @@ lift3
       (fun cond i t -> ExprCond (cond, i, t))
       (pstrtoken "if" *> pexpr)
       (pstrtoken "then" *> pexpr)
       (pstrtoken "else" *> pexpr <|> return (ExprConst ConstUnit))
;;

(** Pattern parse *)
let parse_pconst = parse_int <|> parse_bool >>| fun x -> PatConst x

let parse_pvar = parse_name >>| fun x -> PatVar x

let parse_pattern =
  fix
  @@ fun parse_pattern ->
  let ppat =
    pparens parse_pattern
    <|> parse_pconst
    <|> (pstrtoken "_" >>| fun _ -> PatWild)
    <|> (pstrtoken "[]" >>| fun _ -> PatEmpty)
    <|> parse_pvar
  in
  let ppat =
    lift2
      (fun p -> function
        | hd :: tl -> PatTuple (p, hd, tl)
        | _ -> p)
      ppat
      (many (pstrtoken "," *> ppat))
  in
  let ppat =
    lift2
      (fun p -> function
        | hd :: tl -> PatConc (p, hd, tl)
        | _ -> p)
      ppat
      (many (pstrtoken "::" *> ppat))
  in
  let ppat =
    lift2
      (fun p -> function
        | hd :: tl -> PatOr (p, hd, tl)
        | _ -> p)
      ppat
      (many (pstrtoken "|" *> ppat))
  in
  ppat
;;

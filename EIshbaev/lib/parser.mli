(** Copyright 2024-2025, Azamat Ishbaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Ast

val parse : string -> (structure, string) result

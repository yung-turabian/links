(* This module contains common datatypes used in ASTs in both the frontend
   (Sugartypes) and typechecker (Types). *)

open Utility
open Graph

let show_subkindclasses
  = Settings.(flag "show_subkindclasses"
              |> depends Debug.enabled
              |> convert parse_bool
              |> sync)

module Name = struct
  type t = string
    [@@deriving show]
end

module Linearity = struct
  type t = Any | Unl
    [@@deriving eq,show]

  let is_any = function
    | Any -> true
    | _   -> false

  let is_nonlinear = function
    | Unl -> true
    | _   -> false

  let to_string ?(is_eff=false) = function
    | Any -> "Any"
    | Unl -> if is_eff then "Lin" else "Unl"

  let min l r =
    match l, r with
    | Unl, _ | _, Unl -> Unl
    | Any, Any -> Any
end

(* Convenient aliases for constructing values *)
let lin_any = Linearity.Any
let lin_unl = Linearity.Unl

module DeclaredLinearity = struct
  type t = Lin | Unl
    [@@deriving show]

  let is_linear = function
    | Lin -> true
    | _   -> false

  let is_nonlinear = function
    | Unl -> true
    | _   -> false
end


(* Convenient aliases for constructing values *)
let dl_lin = DeclaredLinearity.Lin
let dl_unl = DeclaredLinearity.Unl

module Restriction = struct
  type t = string
  [@@deriving eq,show]

  let graph : (t, t list) Hashtbl.t = Hashtbl.create 20

  (* Default restrictions *)
  let () =
    (* Any is the most general restriction *)
    Hashtbl.add graph "Any" [];
    (* Mono is a child of Any *)
    Hashtbl.add graph "Mono" ["Any"];
    (* Base and Session are children of Mono *)
    Hashtbl.add graph "Base" ["Mono"; "Any"];
    Hashtbl.add graph "Session" ["Mono"; "Any"];
    (* Effect is a direct child of Any *)
    Hashtbl.add graph "Eff" ["Any"]

  let exists name = Hashtbl.mem graph name

  (* Get all ancestors (transitive closure of parents) *)
  let ancestors r =
    let rec collect acc current =
      let direct_parents = try Hashtbl.find graph current with Not_found -> [] in
      List.fold_left (fun acc parent ->
        if List.mem parent acc then acc
        else collect (parent::acc) parent
      ) acc direct_parents
    in
    if exists r then collect [] r else []

    (** Computes the the minimum of two restricitons, if both are on the same level then get the more general. *)
    let min r1 r2 =
      if r1 = r2 then Some r1
      else 
        let is_r1_ancestor_of_r2 = List.mem r1 (ancestors r2) in
        let is_r2_ancestor_of_r1 = List.mem r2 (ancestors r1) in
        match is_r1_ancestor_of_r2, is_r2_ancestor_of_r1 with
        | true, false -> Some r2  (* r1 is more general than r2 *)
        | false, true -> Some r1  (* r2 is more general than r1 *)
        | _ ->
            (* Neither is ancestor of the other - find common ancestors *)
            let ancestors_r1 = ancestors r1 in
            let ancestors_r2 = ancestors r2 in
            let common = List.filter (fun a -> List.mem a ancestors_r2) ancestors_r1 in
            match common with
            | [] -> None
            | _ ->
                List.fold_left (fun acc a ->
                  match acc with
                  | None -> Some a
                  | Some current ->
                      if List.mem a (ancestors current) then Some current
                      else if List.mem current (ancestors a) then Some a
                      else Some current
                ) None common

  let add ?(parents=["Any"]) name =
    if exists name then
      Debug.if_set (show_subkindclasses)
        (fun () -> ("Restriction " ^ name ^ " already exists"))
    else if List.for_all exists parents then
      Hashtbl.add graph name parents
    else
      Debug.if_set (show_subkindclasses)
        (fun () -> ("Some parent restrictions not found: "));

      List.iter (fun r -> Debug.if_set (show_subkindclasses)
        (fun () -> (r))) parents

  let all_restrictions () =
    let nodes = Hashtbl.fold (fun k _ acc -> k::acc) graph [] in
    let edges = 
      Hashtbl.fold (fun child parents acc ->
        List.map (fun parent -> (child, parent)) parents @ acc
      ) graph [] in
    topological_sort nodes edges

  let to_string r = r

   (* Visualization helper *)
   let to_dot () =
    let buf = Buffer.create 256 in
    Buffer.add_string buf "digraph Restrictions {\n";
    Buffer.add_string buf "  rankdir=BT;\n";  (* Bottom-to-top ranking *)
    Hashtbl.iter (fun child parents ->
      List.iter (fun parent ->
        Buffer.add_string buf (Printf.sprintf "  \"%s\" <: \"%s\";\n" child parent))
        parents
    ) graph;
    Buffer.add_string buf "}\n";
    Buffer.contents buf

    (** These can be deleted. *)
    let () =
      assert (min "Any" "Base" = Some "Base");
      assert (min "Base" "Any" = Some "Base");
      assert (min "Base" "Mono" = Some "Base");
      assert (min "Mono" "Base" = Some "Base");

      assert (min "Any" "Session" = Some "Session");
      assert (min "Session" "Any" = Some "Session");

      assert (min "Base" "Session" = Some "Mono");
      assert (min "Session" "Base" = Some "Mono");
      assert (min "Eff" "Session" = Some "Any")

end

(* Convenient aliases for constructing default values *)
let res_any     = "Any"
let res_base    = "Base"
let res_mono    = "Mono"
let res_session = "Session"
let res_effect  = "Eff"

module Subkind = struct
  type t = Linearity.t * Restriction.t
    [@@deriving eq,show]
  
  let to_string (lin, res) =
    Printf.sprintf "(%s,%s)"
      (Linearity.to_string   lin)
      (Restriction.to_string res)

  let to_string_opt = function
  | Some (lin, res) -> to_string (lin, res)
  | _ -> ""

  let restriction (_, res) = res
  let linearity (lin, _) = lin
  let as_unl (_, res) = (lin_unl, res)
  let as_session (lin, _) = (lin, res_session)
end

let default_subkind : Subkind.t = (lin_unl, res_any)
let any_subkind : Subkind.t = (lin_any, res_any)

(* NOTICE: `lin_any` here means this eff_row_var can be unified with
    linear or unlimited row types *)

let lincont_enabled = Settings.get Basicsettings.CTLinearity.enabled

let default_effect_lin : Linearity.t = if lincont_enabled then lin_any else lin_unl

let default_effect_subkind : Subkind.t =(default_effect_lin, res_any)

(* NOTE: should remain static *)
module PrimaryKind = struct
  type t =
    | Type
    | Row
    | Presence
    [@@deriving show,eq]

  let to_string = function
    | Type     -> "Type"
    | Row      -> "Row"
    | Presence -> "Presence"
end

(* Convenient aliases for constructing values *)
let pk_type     = PrimaryKind.Type
let pk_row      = PrimaryKind.Row
let pk_presence = PrimaryKind.Presence

let default_kind : PrimaryKind.t = PrimaryKind.Type

module Kind = struct
  type t = PrimaryKind.t * Subkind.t
    [@@deriving eq,show]

  let subkind (_, sk) = sk
  let restriction kind =
    Subkind.restriction (subkind kind)
  let as_unl (pk, sk) =
    (pk, Subkind.as_unl sk)
  let as_session (pk, sk) =
    (pk, Subkind.as_session sk)

  let primary_kind (pk, _) = pk

  let to_string (pk, sk) = 
    let pk = 
      match pk with
      | None -> ""
      | Some k -> PrimaryKind.to_string k
    in
    let sk = 
      match sk with
      | None -> ""
      | Some k -> Printf.sprintf "%s%s" "::" (Subkind.to_string k)
    in

    Printf.sprintf "%s%s" pk sk
end

module Quantifier = struct
  type t = int * Kind.t
    [@@deriving show]

  let to_var = function
    | (var, _) -> var

  let to_kind : t -> Kind.t = function
    | (_, k) -> k

  let to_primary_kind : t -> PrimaryKind.t = function
    | (_, (pk, _)) -> pk

  let to_subkind : t -> Subkind.t = function
    | (_, (_, sk)) -> sk

  let to_string = Format.asprintf "%a" pp

  let eq : t -> t -> bool = fun lvar rvar ->
    to_var lvar = to_var rvar
end

module Timestamp = struct
  type t = Timestamp of CalendarShow.t | MinusInfinity | Infinity
    [@@deriving ord]

  let timestamp ts = Timestamp ts
  let now () = Timestamp (CalendarShow.now ())
  let infinity = Infinity
  let minus_infinity = MinusInfinity

  let pp ppf =
    let open Format in
    function
      | Timestamp ts  -> CalendarShow.pp ppf ts
      | Infinity      -> pp_print_string ppf "'infinity'"
      | MinusInfinity -> pp_print_string ppf "'-infinity'"

  let show x =
      let open Format in
      fprintf str_formatter "%a" pp x;
      flush_str_formatter ()

  let to_string = show

  let parse_string str =
    let open Lexing in

    let print_position outx lexbuf =
        let pos = lexbuf.lex_curr_p in
        Format.fprintf outx "%s:%d:%d" pos.pos_fname
          pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in

    let bad_date msg =
        Errors.RuntimeError
          (Printf.sprintf "Ill-formed date: %s (%s)" str msg) in

    let lexbuf = from_string str in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "<string>" };
    try TimestampParser.timestamp TimestampLexer.lex lexbuf with
      | TimestampParser.Error ->
          let open Format in
          fprintf str_formatter "%a" print_position lexbuf;
          let err = flush_str_formatter () in
          raise (bad_date err)

  (** Parses a user timestamp string. Lack of an offset is assumed to mean
      local time. *)
  let parse_user_string str =
    match parse_string str with
      | `Timestamp (cal, Some offset) ->
            Timestamp (CalendarShow.convert cal
              (CalendarLib.Time_Zone.UTC_Plus offset)
              (CalendarLib.Time_Zone.UTC))
      | `Timestamp (cal, None) ->
            Timestamp (CalendarShow.convert cal
              (CalendarLib.Time_Zone.Local)
              (CalendarLib.Time_Zone.UTC))
      | `Infinity -> Infinity
      | `MinusInfinity -> MinusInfinity

  (** Parses a database timestamp string. Lack of an offset is assumed to mean
      UTC. *)
  let parse_db_string str =
    match parse_string str with
      | `Timestamp (cal, offset) ->
          let offset = OptionUtils.from_option 0 offset in
          Timestamp (CalendarShow.convert cal
              (CalendarLib.Time_Zone.UTC_Plus offset)
              (CalendarLib.Time_Zone.UTC))
      | `Infinity -> Infinity
      | `MinusInfinity -> MinusInfinity
end

module Location = struct
  type t = Client | Server | Unknown
    [@@deriving show]

  let is_client = function
    | Client -> true
    | _      -> false

  let is_server = function
    | Server -> true
    | _      -> false

  let is_unknown = function
    | Unknown -> true
    | _      -> false

  let to_string = function
    | Client -> "client"
    | Server -> "server"
    | Unknown -> "unknown"
end

(* Convenient aliases for constructing values *)
let loc_client  = Location.Client
let loc_server  = Location.Server
let loc_unknown = Location.Unknown

module Freedom = struct
  type t = [`Flexible | `Rigid]
    [@@deriving show]
end

module Label = struct
  type t = string
    [@@deriving show]
end

module ForeignLanguage = struct
  type t =
    | JavaScript
    [@@deriving show]

  let of_string = function
    | "javascript" -> JavaScript
    | _ -> raise (Invalid_argument "of_string")

  let to_string = function
    | JavaScript -> "javascript"
end

module Primitive = struct
  type t = Bool | Int | Char | Float | XmlItem | DB | String | DateTime
    [@@deriving show]

  let to_string = function
    | Bool     -> "Bool"
    | Int      -> "Int"
    | Char     -> "Char"
    | Float    -> "Float"
    | XmlItem  -> "XmlItem"
    | DB       -> "Database"
    | String   -> "String"
    | DateTime -> "DateTime"
end

module Constant = struct
  type t =
    | Float  of float
    | Int    of int
    | Bool   of bool
    | String of string
    | Char   of char
    | DateTime of Timestamp.t
      [@@deriving show, ord]

  let type_of = function
    | Float    _ -> Primitive.Float
    | Int      _ -> Primitive.Int
    | Bool     _ -> Primitive.Bool
    | Char     _ -> Primitive.Char
    | String   _ -> Primitive.String
    | DateTime _ -> Primitive.DateTime

  module DateTime = struct
    let now () = DateTime (Timestamp.timestamp (CalendarShow.now()))
    let beginning_of_time = DateTime (Timestamp.minus_infinity)
    let forever = DateTime (Timestamp.infinity)
  end

  (* SQL standard for escaping single quotes in a string *)
  let escape_string s =
    Str.global_replace (Str.regexp "'") "''" s

  (* This function is actually specific to database query generation; it should
     be moved to the database module(s). *)
  let to_string = function
    | Bool value  -> string_of_bool value
    | Int value   -> string_of_int value
    | Char c      -> "'"^ Char.escaped c ^"'"
    | String s    -> "'" ^ escape_string s ^ "'"
    | Float value -> string_of_float' value
    | DateTime ts -> Timestamp.show ts
end

module QueryPolicy = struct
  type t = Flat | Nested | Mixing | Delat
    [@@deriving show]
end

(* Temporality of queries, inserts, and temporal joins *)
module Temporality = struct
  type t = Current | Transaction | Valid
  [@@deriving yojson]

  let current = Current
  let transaction = Transaction
  let valid = Valid

  let pp ppf =
      let open Format in
      function
        | Current -> pp_print_string ppf "Current"
        | Transaction -> pp_print_string ppf "Transaction"
        | Valid -> pp_print_string ppf "Valid"

  let show x = Format.asprintf "%a" pp x
end

module TemporalField = struct
  let data_field = "!data"
  let from_field = "!from"
  let to_field = "!to"
end

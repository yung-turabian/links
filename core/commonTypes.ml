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

(* TODO: Considering getting this information form subkind environment instead *)
module Restriction = struct
  type t = string
  [@@deriving eq,show]

  let graph : (t, t list) Hashtbl.t = Hashtbl.create 20

  (* Initialize with default restrictions *)
  let () =
    (* Any is the most general restriction *)
    Hashtbl.add graph "Any" [];
    (* Mono is a child of Any *)
    Hashtbl.add graph "Mono" ["Any"];
    (* Base and Session are children of Mono *)
    Hashtbl.add graph "Base" ["Mono"; "Any"];
    Hashtbl.add graph "Session" ["Mono"; "Any"];
    (* Effect is a direct child of Any *)
    Hashtbl.add graph "Effect" ["Any"]


  (* Check if a restriction exists *)
  let exists name = Hashtbl.mem graph name
  
  (* Find all descendants of a restriction (transitive closure) *)
  let descendants r =
    let rec collect acc current =
      let all_children = 
        Hashtbl.fold (fun child parents acc ->
          if List.mem current parents then child::acc else acc
        ) graph [] in
      List.fold_left (fun acc child ->
        if List.mem child acc then acc
        else collect (child::acc) child
      ) acc all_children
    in
    if exists r then collect [] r else []

  (* Compute the minimum of two restrictions (most specific common ancestor) *)
  let min r1 r2 =
    if r1 = r2 then Some r1
    else 
      let ancestors_r1 = Hashtbl.find graph r1 in
      let ancestors_r2 = Hashtbl.find graph r2 in
      (* Find common ancestors and return the most specific one *)
      let common = List.filter (fun a -> List.mem a ancestors_r2) ancestors_r1 in
      match common with
      | [] -> None
      | _ -> 
          (* Find the most specific common ancestor (minimum in the hierarchy) *)
          List.fold_left (fun acc a ->
            match acc with
            | None -> Some a
            | Some current ->
                if List.mem current (Hashtbl.find graph a)
                then Some current
                else if List.mem a (Hashtbl.find graph current)
                then Some a
                else Some current
          ) None common

  (* Add a new restriction dynamically *)
  let add ?(parents=["Any"]) name =
    if exists name then
      Debug.if_set (show_subkindclasses)
        (fun () -> ("Restriction " ^ name ^ " already exists"))
    else if List.for_all exists parents then
      Hashtbl.replace graph name parents
    else
      Debug.if_set (show_subkindclasses)
        (fun () -> ("Some parent restrictions not found"))

  (* Get all restrictions in topological order (most general first) *)
  let all_restrictions () =
    let nodes = Hashtbl.fold (fun k _ acc -> k::acc) graph [] in
    let edges = 
      Hashtbl.fold (fun child parents acc ->
        List.map (fun parent -> (child, parent)) parents @ acc
      ) graph [] in
    topological_sort nodes edges

  (* Check if the graph is acyclic (should be true for restrictions) *)
    let is_acyclic () =
      let nodes = Hashtbl.fold (fun k _ acc -> k::acc) graph [] in
      let edges = 
        Hashtbl.fold (fun child parents acc ->
          List.map (fun parent -> (child, parent)) parents @ acc
        ) graph [] in
      List.length (strongly_connected_components nodes edges) = List.length nodes

  (* Convert a restriction to its string representation *)
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

end

(* Convenient aliases for constructing default values *)
let res_any     = "Any"
let res_base    = "Base"
let res_mono    = "Mono"
let res_session = "Session"
let res_effect  = "Effect"

module Subkind = struct
  type t = Linearity.t * Restriction.t
    [@@deriving eq,show]
  
  let to_string (lin, res) =
    Printf.sprintf "(%s,%s)"
      (Linearity.to_string   lin)
      (Restriction.to_string res)

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

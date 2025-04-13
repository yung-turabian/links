(*
  This pass resolves type variables and reports errors w.r.t. their
  scoping. Specifically, this means that the pass replaces all TUnresolved from
  {Sugartypes.SugarTypeVar.t} by appropriate TResolved*. The latter contain
  Unionfind points and the pass guarantees that syntactic/unresolved variables
  referring to the same variable will be assigned the same Unionfind point.
  This pass reports errors about type variables occuring in positions where
  they are not allowed to (e.g., an unbound, named variable occuring in a
  context where no type variables must be bound implicitly).
  However, see the restriction on anonymous effect variables below.

  Likewise, all occurences of QUnresolved from {Sugartypes.SugarQuantifier.t}
  are replaced by appropriate QResolved*

  Handling of anonymous effect variables: This pass ignores all anonymous effect
  variables, i.e., it leaves them as TUnresolved. Further, it does not report
  errors about such variables in places where they are not allowed.  Both must
  be dealt with later.

  Furthermore all occurences of KUnresolved from {Sugartypes.SugarKind.t}
  are replaced by appropriate KResolved*
*)

open CommonTypes
open Utility
open Sugartypes


type tyvar_map_entry =
  | TVUnkinded of Subkind.t option * Freedom.t
  | TVType     of Subkind.t option * Types.meta_type_var
  | TVRow      of Subkind.t option * Types.meta_row_var
  | TVPresence of Subkind.t option * Types.meta_presence_var


(**
  Maps used for storing info about type variables.  Note that kinding info is
  stored both in {Sugartypes.SugarKind} and the meta_*_ Unionfind points of the
  resolved constructors.  However, in the former, the kinding info is optional,
  whereas it is mandatory in the second. We use the convention that information
  in Sugartypes.SugarKind takes precedence over the kinding info in the
  Unionfind points. This allows us to properly express the absence of kinding
  information. Once an entry of Sugartypes.SugarKinds is resolved, it must of course be
  consistent with the information in the Unionfind point
*)
type tyvar_map = tyvar_map_entry StringMap.t

let infer_kinds
  = Settings.(flag "infer_kinds"
              |> convert parse_bool
              |> sync)

module SEnv = Env.String

(* Errors *)

let internal_error message =
  Errors.internal_error ~filename:"desugarTypeVariables.ml" ~message

let found_non_var_meta_var =
  internal_error "Every meta_*_var in a SugarTypeVar must be a Var at this point"

let typevar_mismatch pos v1 v2 =
  let string_of_kinded_type_var (name, (kind, subkind), _freedom)  =
    match kind with
    | None -> name
    | Some kind ->
       let subkind = OptionUtils.opt_app Subkind.to_string "" subkind in
       name ^ "::" ^ PrimaryKind.to_string kind ^ subkind
  in
  let var, _, _ = v1 in
  Errors.Type_error
    ( pos,
      Printf.sprintf "Mismatch in kind for type variable `%s'.\n" var
      ^ Printf.sprintf "  Declared as `%s' and `%s'."
          (string_of_kinded_type_var v1)
          (string_of_kinded_type_var v2))

let duplicate_var pos var =
  Errors.Type_error (pos, Printf.sprintf "Multiple definitions of type variable `%s'." var)

let free_type_variable ?var pos =
  let desc = match var with
    | None -> "anonymous type variable"
    | Some name -> Printf.sprintf "type variable `%s'"  name
  in Errors.Type_error
       (pos,
        "Unbound " ^ desc ^ " in position where
        no free type variables are allowed")

let concrete_subkind ?(is_effect=false) =
  function
  | Some subkind -> subkind
  | None         -> if is_effect then default_effect_subkind else default_subkind


let default_kind : PrimaryKind.t = PrimaryKind.Type
let default_subkind : Subkind.t = (lin_unl, res_any)



let is_anonymous_name name =
  name.[0] = '$'

let is_anonymous stv =
  SugarTypeVar.get_unresolved_name_exn stv |> is_anonymous_name


let get_entry_var_info (entry : tyvar_map_entry ) :  (int * Kind.t  * Freedom.t) option =
  let extract_data  =
    function
    | Types.Var (var, k, freedom) -> var, k, freedom
    | _ -> raise found_non_var_meta_var
  in
  match entry with
  | TVUnkinded (_,_)    -> None
  | TVType (_, mtv)     -> Some (extract_data (Unionfind.find mtv))
  | TVRow  (_, mrv)     -> Some (extract_data (Unionfind.find mrv))
  | TVPresence (_, mpv) -> Some (extract_data (Unionfind.find mpv))



(* Given the signature on a var/function binder, returns whether or not
   we allow implictly bound type variables both in the signature itself
   and in the body of the binding *)
let sig_allows_implicitly_bound_vars :  datatype' option -> bool  =
  let open Datatype in
  function
  | Some (t_wp,  _) ->
     begin
       match SourceCode.WithPos.node t_wp with
         | Forall (_, _) -> false
         | _ -> true
     end
  | None -> true


let lookup_tyvar_exn name (map : tyvar_map) : (PrimaryKind.t option * Subkind.t option) * Freedom.t * tyvar_map_entry =
  let extract_freedom =
    (* Consistency check: If the tyvar map contains subkind info,
       then it must coindice with the subkind in the Unionfind point *)
    function
    | None,        Types.Var (_,  _           ,  freedom)                       -> freedom
    | Some map_sk, Types.Var (_,  (_, subkind),  freedom) when map_sk = subkind -> freedom
    | Some map_sk, Types.Var (_,  (_, subkind), _freedom) when map_sk <> subkind ->
       raise (internal_error "kind information in map and point diverged")
    | _ ->
       raise found_non_var_meta_var
  in
  let entry = StringMap.find name map in
  match entry with
  | TVUnkinded (sk, fd) -> (None, sk), fd, entry
  | TVType (sk, mtv) ->
     let fd = extract_freedom (sk, (Unionfind.find mtv)) in
     (Some PrimaryKind.Type, sk), fd, entry
  | TVRow (sk, mrv) ->
     let fd = extract_freedom (sk, (Unionfind.find mrv)) in
     (Some PrimaryKind.Row, sk), fd, entry
  | TVPresence (sk, mpv) ->
     let fd = extract_freedom (sk, (Unionfind.find mpv)) in
     (Some PrimaryKind.Presence, sk), fd, entry


(*
Note that we fill primary kind and subkind info here.
However, this is not the same as setting defaults. Instead,
the info in the map takes precedence.

*)
let make_opt_kinded_var k ?(is_eff=false) sk_opt freedom : Types.t =
  let var = Types.fresh_raw_variable () in
  let sk = concrete_subkind ~is_effect:is_eff sk_opt in
  Types.Var (var, (k, sk), freedom)

let get_var_info (info : Types.t) =
  match info with
  | Types.Var (var, k, fd) -> (var, k, fd)
  | _ -> raise found_non_var_meta_var

let make_fresh_entry pk_opt ?(is_eff=false) sk_opt freedom : tyvar_map_entry =
  let open PrimaryKind in
  match pk_opt with
    | None -> TVUnkinded (sk_opt, freedom)
    | Some Type ->
       let point = Unionfind.fresh (make_opt_kinded_var ~is_eff:is_eff Type sk_opt freedom) in
       TVType (sk_opt, point)
    | Some Row ->
       let point = Unionfind.fresh (make_opt_kinded_var ~is_eff:is_eff Row sk_opt freedom) in
       TVRow (sk_opt, point)
    | Some Presence ->
       let point = Unionfind.fresh (make_opt_kinded_var ~is_eff:is_eff Presence sk_opt freedom) in
       TVPresence (sk_opt, point)



(* does not do all sanity checks *)
(* Note that this always reuses existing points! *)
let update_entry pk ?(is_eff=false) sk_opt freedom existing_entry : tyvar_map_entry =
  let con_sk = concrete_subkind ~is_effect:is_eff sk_opt in
  let open PrimaryKind in
  match pk, existing_entry with
    | _, TVUnkinded _ ->
       make_fresh_entry (Some pk) sk_opt freedom
    | Type, TVType (_, point) ->
       let (var, (_, prev_sk), _) = get_var_info (Unionfind.find point) in
       (if con_sk <> prev_sk then Unionfind.change point (Types.Var (var, (Type, con_sk), freedom)));
       TVType (sk_opt, point)
    | Row, TVRow (_, point) ->
       let (var, (_, prev_sk), _) = get_var_info (Unionfind.find point) in
       (if con_sk <> prev_sk then Unionfind.change point (Types.Var (var, (Row, con_sk), freedom)));
       TVRow (sk_opt, point)
    | Presence, TVPresence (_, point) ->
       let (var, (_, prev_sk), _) = get_var_info (Unionfind.find point) in
       (if con_sk <> prev_sk then Unionfind.change point (Types.Var (var, (Presence, con_sk), freedom)));
       TVPresence (sk_opt, point)
    | _ -> raise (internal_error "inconsistent kind information")


let resolved_var_of_entry =
  let open Sugartypes.SugarTypeVar in
  function
    | TVUnkinded _->
       raise
         (internal_error "tried to resolve variable without known primary kind")
    | TVType (_, point) -> TResolvedType point
    | TVRow (_, point) -> TResolvedRow point
    | TVPresence (_, point) -> TResolvedPresence point



class typevar_visitor initial_map allow_implicits subkind_env =
object (o : 'self)
  inherit SugarTraversals.fold_map as super

  val tyvar_map : tyvar_map = initial_map
  (* New subkinds are resolved here into the environment first. *)
  val subkind_env : Types.subkind_environment = subkind_env

  method private get_subkind_env = subkind_env

  (** Ensure this variable has some kind, if {!infer_kinds} is disabled. *)
  method ensure_kinded : 'a. subkind_env: Types.subkind_environment -> 
    SourceCode.Position.t -> 
    string * SugarKind.t * Freedom.t -> 
      'self * (string * SugarKind.t * Freedom.t) =
  fun ~subkind_env pos ((name, kind, fd) as v) ->
  let open SugarKind in
  match kind with
  (** Catches ::Numeric, a named subkind,
    as opposed to defined by linearity and restriction *)
  | KUnresolved (None, (None, Some res)) ->
    if is_anonymous_name name then
      o, v
    else
      (** Confirm that subkind exists in the environment of subkinds. *)
      let pk, sk = 
        try 
          match SEnv.find res subkind_env with
          | `Decl (pk, sk) -> (pk, sk)
          | `Class ((pk, sk), _, _) -> (pk, sk)
        with NotFound _ ->
            raise (Errors.unbound_subkind pos res)
      in
      o, (name, KResolved (Some pk, Some sk), fd)

  (** Catches ::Type *)
  | KUnresolved (Some pk, (None, None)) -> 
      (*TODO: shoots an error if lin_unl *)
      o, (name, KResolved (Some pk, Some (lin_any, res_any)), fd)

  (** Catches ::Type(Any, Any) & ::Type(Unl) *)
  | KUnresolved (Some pk, (Some lin, Some res)) ->
    if SEnv.has res subkind_env then
      let sk = (lin, res) in
      o, (name, KResolved (Some pk, Some sk), fd)
    else
      failwith "Couldn't construct a subkind, the restriction doesn't exist"

  (** Catches ::(Any, Base) *)
  | KUnresolved (None, (Some lin, Some res)) ->
    if SEnv.has res subkind_env then
      let sk = (lin, res) in
      o, (name, KResolved (Some pk_type, Some sk), fd)
    else
      failwith ("Couldn't resolve subkind with linearity: " ^ 
        (Linearity.to_string lin) ^ " and restrition: " ^ (res))

  (** Catches unkinded quants *)
  | KUnresolved (None, (None, None)) when not (Settings.get infer_kinds) ->
      o, (name, KResolved(Some default_kind, None), fd)
  
  | KUnresolved _ ->
    failwith "failed to resolved subkind; this shouldn't happen"

  | KResolved _ -> o, v

  (** Allow implicitly bound type/row/presence variables in the current context? *)
  val allow_implictly_bound_vars = allow_implicits

  (* part of legacy compatibility, remove later *)
  val at_toplevel = true


  method reset_vars =
    (* just let unbind do its sanity check on all variables *)
    StringMap.iter (fun var _ -> ignore (o#unbind var o)) tyvar_map;
    {< tyvar_map = StringMap.empty >}

  method get_vars = tyvar_map

  method set_vars tyvar_map = {< tyvar_map >}

  method set_toplevelness at_toplevel = {< at_toplevel >}

  method get_toplevelness = at_toplevel

  method set_allow_implictly_bound_vars allow_implictly_bound_vars = {< allow_implictly_bound_vars >}

  method set_subkind_env subkind_env = {< subkind_env >}

  method bind
           name
           (entry: tyvar_map_entry)  =
    {< tyvar_map = StringMap.add name entry tyvar_map >}



  (** Return all information obtained about the given variable,
     possibly filling in default kinding info.
     Restore the info stored about the variable in a previous object *)
  method unbind name (restore_from : 'self) : 'self * (Kind.t * Freedom.t) * tyvar_map_entry =
    let (pk_opt, sk_opt), fd, cur_entry = lookup_tyvar_exn name tyvar_map in

    let pk = from_option default_kind pk_opt in
    let sk = from_option default_subkind sk_opt in

    let old_map = restore_from#get_vars in
    let o = match StringMap.lookup name old_map with
      | Some old_entry ->
         o#bind name old_entry
      | None ->
         {< tyvar_map = StringMap.remove name tyvar_map >}
    in
    o, ((pk, sk), fd), cur_entry





  (** Used for type/row/presence variables found along the way, including anonymous ones *)
  method add ?pos name (kind : SugarKind.t) ?(is_eff=false) freedom : 'self * SugarTypeVar.t =
    let anon = is_anonymous_name name in
    let pos = OptionUtils.from_option SourceCode.Position.dummy pos in
    let o, (_, kind, _) = o#ensure_kinded pos ~subkind_env (name, kind, freedom) in
    let pk = OptionUtils.from_option pk_type (SugarKind.get_resolved_pk_exn kind) in
    let sk = SugarKind.get_resolved_sk_exn kind in
    if not anon && StringMap.mem name tyvar_map then
      begin
        let (pk', sk'), freedom', existing_entry  = lookup_tyvar_exn name tyvar_map in
        let union = function
          | Some x, None | None, Some x -> Some x, Some x
          | x, y -> x, y
        in
        (* monotonically increase subkinding information *)
        let (sk_union, sk_union') = union (sk, sk') in
        let (pk_union, pk_union') = union (Some pk, pk') in
        let tv = (name, (pk_union, sk_union), freedom) in
        let tv' = (name, (pk_union', sk_union'), freedom') in
        (* check that new info is consistent with existing knowledge *)
        if tv <> tv' then
          raise (typevar_mismatch pos tv tv');

        (* Check if we learned anything new at all.
           If so, update the entry to put in the tyvar map. This also updates
           the *existing* unionfind point carried inside the entry *)
        let entry =
          if pk' <> pk_union' || sk' <> sk_union' then
            update_entry pk ~is_eff:is_eff sk freedom existing_entry
          else
            existing_entry
        in

        let resolved_var = resolved_var_of_entry entry in
        o#bind name entry, resolved_var
      end
    else
      begin
        (if (not allow_implictly_bound_vars)  && freedom = `Rigid then
           let name_opt = if anon then None else Some name in
           raise (free_type_variable ?var:name_opt pos));

        (* We create a new entry for the tyvar map.
           Since at this point we know the primary kind,
           this will always result in the creation of a unionfind point.
         *)
        let entry = make_fresh_entry (Some pk) ~is_eff:is_eff sk freedom in
        let o =
          if anon then o else o#bind name entry in
        let resolved_var = resolved_var_of_entry entry in
        o, resolved_var
      end



  (* Improve exceptions by adding location if otherwise absent *)
  method! datatype ty =
    Errors.rethrow_errors_if_better_position
      (SourceCode.WithPos.pos ty)
      super#datatype
      ty


  (**  Used for Forall and Typenames *)
  method quantified : 'a. subkind_env: Types.subkind_environment -> rigidify:bool -> 
    SugarQuantifier.t list -> ('self -> 'self * 'a) -> 'self * SugarQuantifier.t list * 'a =
    fun ~subkind_env ~rigidify unresolved_qs action ->
    let original_o = o in
    let bind_quantifier (o, names) sq =
      let (name, _, _) as v =
        SugarQuantifier.get_unresolved_exn sq in
      let pos = SourceCode.Position.dummy in
      if StringSet.mem name names then raise (duplicate_var pos name);
      let o, (_, kind, freedom) = o#ensure_kinded ~subkind_env pos v in
      let pk = SugarKind.get_resolved_pk_exn kind in
      let sk = SugarKind.get_resolved_sk_exn kind in
      let freedom = if rigidify then `Rigid else freedom in
      let entry = make_fresh_entry pk sk freedom in
      let o' = o#bind name entry in
      let names' = StringSet.add name names in
      (o', names')
    in
    let unbind_quantifier
          (sq : SugarQuantifier.t)
          (o, (rqs : SugarQuantifier.t list)) =
      let name = SugarQuantifier.get_unresolved_name_exn sq in
      let o, ((pk, sk), _freedom), entry = o#unbind name original_o in
      let var = match get_entry_var_info entry with
          | Some (var, _, _) -> var
          | None -> raise (internal_error "must have a Unionfind Var at this time")
      in
      let q : Quantifier. t = var, (pk, sk) in
      let rq = SugarQuantifier.mk_resolved q in
      o, (rq :: rqs)
    in
    let o, _ = List.fold_left bind_quantifier (o, StringSet.empty) unresolved_qs in
    let o, action_result = action o in
    let o, resolved_qs = List.fold_right unbind_quantifier unresolved_qs (o, []) in
    o, resolved_qs, action_result



  method! datatypenode =
    let open Datatype in
    function
    | TypeVar stv ->
       let (name, (is_eff, kind), freedom) = SugarTypeVar.get_unresolved_exn stv in
       let o, resolved_tv = o#add name kind ~is_eff:is_eff freedom in
       (* let resolved_tv = resolved_var_of_entry entry in *)
       o, TypeVar resolved_tv
    | Forall (unresolved_qs, body) ->
       let o, resolved_qs, body = o#quantified ~subkind_env ~rigidify:false unresolved_qs (fun o' -> o'#datatype body) in
       o, Forall (resolved_qs, body)

    | Mu (stv, t) ->
       let original_o = o in
       let name = SugarTypeVar.get_unresolved_name_exn stv in
       let entry = make_fresh_entry (Some PrimaryKind.Type) None `Rigid in
       let o = o#bind name entry in
       let o, t = o#datatype t in
       let o, _, _ = o#unbind name original_o in
       let resolved_tv = resolved_var_of_entry entry in

       (* At this point, the body of the Unionfind point inside of entry is a Var.
          DesugarDatatypes changes that to `Recusive *)
       o, Mu (resolved_tv, t)
    | dt -> super#datatypenode dt


  method! row_var =
    let open Datatype in function
    | EffectApplication _ as ea ->
        super#row_var ea
    | Closed -> o, Closed
    | Open srv as orig when is_anonymous srv ->
       (* This transformation pass does not check whether anonymous row variables
          appear in contexts where implictly scoped variables are allowed.  *)
       o, orig
    | Open srv ->
       let (name, (is_eff, kind), freedom) = SugarTypeVar.get_unresolved_exn srv in
       let (_, l, r) = SugarKind.get_unresolved_exn kind in
       let o, resolved_rv = o#add name (SugarKind.mk_unresolved (Some pk_row) (l, r)) ~is_eff:is_eff freedom in
       o, Datatype.Open resolved_rv
    | Recursive (stv, r) ->
       let original_o = o in

       let name = SugarTypeVar.get_unresolved_name_exn stv in
       let entry = make_fresh_entry (Some pk_row) (Some default_subkind) `Rigid in
       let o = o#bind name entry in
       let o, t = o#row r in
       let o, _, _ = o#unbind name original_o in
       let resolved_rv = resolved_var_of_entry entry in

       (* At this point, the body of the Unionfind point inside of entry is a Var.
          DesugarDatatypes changes that to `Recusive *)
       o, Recursive (resolved_rv, t)

  method! fieldspec = let open Datatype in function
    | Absent -> o, Absent
    | Present t ->
       let o, t = o#datatype t in
       o, Present t
    | Var utv ->
       let (name, (is_eff, kind), freedom) = SugarTypeVar.get_unresolved_exn utv in
       let (_, l, r) = SugarKind.get_unresolved_exn kind in
       let o, resolved_pv = o#add name (SugarKind.mk_unresolved (Some pk_presence) (l, r)) ~is_eff:is_eff freedom in
       o, Var resolved_pv

  method! phrase p =
    match SourceCode.WithPos.node p with
    | TAbstr (tyvars, exp) ->
       let handle_tabstr o =
         let prev_at_toplevel = o#get_toplevelness in
         let o = o#set_toplevelness false in

         let (o, p) = o#phrase exp in
         let o = o#set_toplevelness prev_at_toplevel in
         o, p
       in
       let (o, new_tyvars, new_phrase) = o#quantified ~subkind_env ~rigidify:true tyvars handle_tabstr in
       o, SourceCode.WithPos.with_node p (TAbstr (new_tyvars, new_phrase))
    | _ -> super#phrase p

  method! function_definition : function_definition -> 'self * function_definition
      = fun { fun_binder;
              fun_linearity;
              fun_definition = (tyvar, lit);
              fun_location;
              fun_signature;
              fun_frozen;
              fun_unsafe_signature; } ->
    let implicits_allowed = sig_allows_implicitly_bound_vars fun_signature in
    let o = o#set_allow_implictly_bound_vars implicits_allowed in

    (* To simulate the legacy scoping behavior, we traverse the signature before the body *)
    let o, fun_binder = o#binder fun_binder in
    let o, tyvar = o#list (fun o -> o#quantifier) tyvar in
    let o, fun_signature = o#option (fun o -> o#datatype') fun_signature in
    let o, lit = o#funlit lit in
    let o, fun_location = o#location fun_location in

    let o = o#set_allow_implictly_bound_vars allow_implictly_bound_vars in
    (o, { fun_binder;
          fun_linearity;
          fun_definition = (tyvar, lit);
          fun_location;
          fun_signature;
          fun_frozen;
          fun_unsafe_signature; })


    (* See {function_definition} *)
  method! recursive_functionnode  : recursive_functionnode -> 'self * recursive_functionnode
    = fun { rec_binder;
            rec_linearity;
            rec_definition = ((tyvar, ty), lit);
            rec_location;
            rec_signature;
            rec_unsafe_signature;
            rec_frozen } ->
      let implicits_allowed = sig_allows_implicitly_bound_vars rec_signature in
      let o = o#set_allow_implictly_bound_vars implicits_allowed in

      let o, rec_binder = o#binder rec_binder in
      let o, tyvar = o#list (fun o -> o#quantifier) tyvar in
      let o, ty = o#option (fun o (t, x)-> let o, t = o#typ t in o, (t, x)) ty in
      let o, rec_signature = o#option (fun o -> o#datatype') rec_signature in
      let o, lit = o#funlit lit in
      let o, rec_location = o#location rec_location in

      let o = o#set_allow_implictly_bound_vars allow_implictly_bound_vars in
      (o, { rec_binder;
            rec_linearity;
            rec_definition = ((tyvar, ty), lit);
            rec_location;
            rec_signature;
            rec_unsafe_signature;
            rec_frozen})

  
  method! aliasnode (name, unresolved_qs, body) =

    (* Don't allow unbound named type variables in type definitions.
       We do allow unbound *anoynmous* variables, because those may be
       effect variables that the effect sugar handling will generalize the
       type binding over.
       Hence, we must re-check the free variables in the type definiton later on. *)

    let o = o#set_allow_implictly_bound_vars false in
    (* Aliases must never use type variables from an outer scope *)
    let o = o#reset_vars in

    let o, resolved_qs, body = o#quantified ~subkind_env ~rigidify:true unresolved_qs (fun o' -> o'#aliasbody body) in

    let o = o#set_allow_implictly_bound_vars allow_implictly_bound_vars in
    let o = o#set_vars tyvar_map in

    o, (name, resolved_qs, body)


  method super_bindingnode = super#bindingnode



  method! bindingnode b =
    (* legacy type variables scoping hack *)
    let apply f x =
      let o, b = f x in
      let o = o#set_toplevelness at_toplevel in
      let o =
        if at_toplevel then
          o#reset_vars
        else
          o
      in
      o,b
    in
     match b with
    | Funs fs ->
       (* Toplevel mutuals don't share type variables *)
       let (o, fs) =
         o#list
           (fun o ->
             let o = o#set_toplevelness false in
             apply o#recursive_function)
           fs
       in
       (o, (Funs fs))
    | Val (_pat, (_qs, _body), _loc, signature) ->
       (* For Val bindings, signature determines whether implicitly
          bound vars are allowed *)
       let implicits_allowed = sig_allows_implicitly_bound_vars signature in
       let o = o#set_allow_implictly_bound_vars implicits_allowed in
       let o = o#set_toplevelness false in

       let (o, b) = apply o#super_bindingnode b in

       let o = o#set_allow_implictly_bound_vars allow_implictly_bound_vars in
       (o, b)
    (*| Class { class_name; class_tyvars; class_methods } ->
      Debug.if_set (CommonTypes.show_subkindclasses)
        (fun () -> ("Entering desugarTypeVariables"));

      let o, class_name = o#name class_name in

      (* Add all type declarations in the group to the alias
       * environment, as mutuals. Quantifiers need to be desugared. *)
      let mutual_env = 
        SEnv.bind class_name (`Class ((pk_type, default_subkind), [], [])) subkind_env 
      in

      
      let o, updated_class_tyvar = 
        match class_tyvars with
        | [] -> 
            Restriction.add ~parents:["Any"; "Mono"] class_name;
            Debug.if_set (CommonTypes.show_subkindclasses)
              (fun () -> ("Adding restriction: " ^ class_name));
            
            o, []
        | _ ->
          o#list (fun o q ->
            let (tyvar_name, kind, fd) = SugarQuantifier.get_unresolved_exn q in
            match kind with
              | SugarKind.KResolved (pk, sk) ->
                Debug.if_set (CommonTypes.show_subkindclasses)
                  (fun () -> ("SugarKind is resolved in quantifier: " ^ tyvar_name));

                (match pk with
                | Some k -> 
                  Debug.print ("Primary kind: " ^ (PrimaryKind.to_string k))
                | None -> Debug.print ("No primary kind"));  

                let sk = 
                  match sk with
                  | Some k -> 
                    Debug.print ("Subkind kind: " ^ (Subkind.to_string k));
                    k
                  | None -> 
                    Debug.print ("No sub kind, will assume linearity is lin_unl and res is Any.");
                    default_subkind
                in

                (match pk, sk with
                | Some _pk_type, (_lin_any, "Any") ->
                    Restriction.add ~parents:["Any"; "Mono"] class_name;
                    Debug.if_set (CommonTypes.show_subkindclasses)
                      (fun () -> ("Adding restriction: " ^ class_name));
                    Debug.if_set (CommonTypes.show_subkindclasses)
                      (fun () -> Restriction.to_dot ())
                | _, sk -> 
                    Restriction.add ~parents:[Subkind.show sk] class_name;
                    Debug.if_set (CommonTypes.show_subkindclasses)
                      (fun () -> ("Adding restriction: " ^ Subkind.show sk));
                    Debug.if_set (CommonTypes.show_subkindclasses)
                      (fun () -> Restriction.to_dot ())
                | _ -> failwith "dd"
                );


                let kind = SugarKind.mk_resolved pk (Some sk) in

                o, SugarQuantifier.mk_unresolved tyvar_name kind fd

              | SugarKind.KUnresolved (pk, (lin, _res)) -> 
                Debug.if_set (CommonTypes.show_subkindclasses)
                  (fun () -> ("SugarKind is unresolved in quantifier: " ^ tyvar_name));

                Restriction.add ~parents:["Any"; "Mono"] class_name;
                Debug.if_set (CommonTypes.show_subkindclasses)
                  (fun () -> ("Adding restriction: " ^ class_name));

                let kind = SugarKind.mk_unresolved pk (lin, Some class_name) in
                let o, (_, kind, fd) = 
                  o#ensure_kinded (SourceCode.Position.dummy) 
                  ~subkind_env:mutual_env 
                  (tyvar_name, kind, fd) 
                in
                let q = SugarQuantifier.mk_unresolved tyvar_name kind fd in
                o, q
        ) class_tyvars
      in
      
      (* Determine shared primary kind *)
      (*let def_kind = 
        let sugar_kinds = List.map (fun q -> SugarQuantifier.get_unresolved_kind_exn q) updated_class_tyvar in
        let pks = List.map (fun k -> SugarKind.get_resolved_pk_exn k) sugar_kinds in
        let sks = List.map (fun k -> SugarKind.get_resolved_sk_exn k) sugar_kinds in
        let print_pks pks = 
          List.iter (fun pk -> Debug.print (PrimaryKind.to_string pk)) pks
        in
        if pks <> [] then
          match ListUtils.find_fstdiff pks with
          | Some k -> 
                (match k with
                  | Some kk -> 
                    Debug.print ("Must be of all same kind. Kinds present: ");
                    (*TODO: make primary kinds non-optional in resolved sugarkind*)
                    (*print_pks pks;*)
                    failwith "fix error code"
                  | _ -> failwith "bad")
          | None -> 
            try (List.hd pks), (List.hd sks)
            with Not_found -> None, None  
        else
          (* TODO: fix for these single class calls *)
          Some pk_type, Some default_subkind
      in*)

      let o = o#set_allow_implictly_bound_vars false in
      let o = o#reset_vars in

      (* Process the class *)
      let o, class_tyvars, class_methods = 
      o#quantified ~rigidify:true ~subkind_env:mutual_env updated_class_tyvar (fun o' -> 
        o'#list (fun o' (b, dt) ->
          let o', b = o'#name b in
          let o', dt = o'#datatype' dt in
          o', (b, dt)) 
          class_methods
      )
      in

      (*List.iter (fun q -> Debug.print(SugarQuantifier.show q)) class_tyvars;*)

      let o = o#set_allow_implictly_bound_vars allow_implictly_bound_vars in
      let o = o#set_vars tyvar_map in

      (* Add all class declaration to the subkind
       * environment, as mutuals. *)
      let subkind_env = 
          (*let pk, sk = 
            match def_kind with
            | Some pk, Some sk -> pk, sk
            | _ -> failwith "Did not resolve kind, unknown why kinds can't be definite in KResolved."
          in*)
          SEnv.bind class_name (`Class ((pk_type, (lin_unl, class_name)), [], [])) subkind_env 
      in

      let o = o#set_subkind_env subkind_env in

      Debug.if_set (CommonTypes.show_subkindclasses)
        (fun () -> ("Exiting desugarTypeVariables"));
      o, Class {class_name; class_tyvars; class_methods}*)
    | Foreign alien ->
       (* For alien bindings, signature determines whether implicitly
          bound vars are allowed *)
       let handle (o, b, dt) =
         let o, b = o#binder b in
         let o, dt = o#datatype' dt in
         o, (b, dt)
       in
       let o, declarations =
         (* (Binder.with_pos * datatype') list *)
         o#list
           (fun o (b, (dt : datatype')) ->
             let implicits_allowed = sig_allows_implicitly_bound_vars (Some dt) in
             let o = o#set_allow_implictly_bound_vars implicits_allowed in
             let o = o#set_toplevelness false in
             let o, (b, dt) = apply handle (o, b, dt) in
             let o = o#set_allow_implictly_bound_vars allow_implictly_bound_vars in
             o, (b, dt))
           (Alien.declarations alien)
       in
       let o, language = o#foreign_language (Alien.language alien) in
       o, Foreign (Alien.modify ~declarations ~language alien)
    | _ ->
       let o = o#set_toplevelness false in
       apply o#super_bindingnode b
end


let program subkind_env p =
  let v = new typevar_visitor StringMap.empty true subkind_env in
  snd (v#program p)


let sentence subkind_env =

function
  | Definitions bs ->
     let v = new typevar_visitor StringMap.empty true subkind_env in
     let _, bs = v#list (fun o b -> o#binding b) bs in
     Definitions bs
  | Expression  p  ->
     let v = new typevar_visitor StringMap.empty true subkind_env in
     let _o, p = v#phrase p in
      Expression p
  | Directive   d  ->
     Directive d

let standalone_signature subkind_env t =
  let allow_implicits = sig_allows_implicitly_bound_vars (Some (t, None)) in
  let v = new typevar_visitor StringMap.empty allow_implicits subkind_env in
  snd (v#datatype t)



module Untyped = struct
  open Transform.Untyped

  let name = "type_variables"

  let program state program' =
    let open Types in
    let tyenv = Context.typing_environment (context state) in
    let subkind_env = tyenv.subkind_env in
    let program' = program subkind_env program' in
    return state program'

  let sentence state sentence' =
    let open Types in
    let tyenv = Context.typing_environment (context state) in
    let subkind_env = tyenv.subkind_env in
    let sentence'' = sentence subkind_env sentence' in
    return state sentence''
end

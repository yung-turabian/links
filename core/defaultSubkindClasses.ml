(* Default subkind classes.

  Subkinds are now handled dynamically as opposed to predefined within the language.
  As such to not break the previous internal reliance on certain subkinds, we define them here.

  Adds the signature to the environment; if none is provided will default to primary kind 'Type':

  class Eq : a::Type { # or class Eq : a {
		sig == : (a, a) -> Bool;
		sig <> : (a, a) -> Bool;
  }

  Env :: (sig == : (a, a) -> Bool)


  So when instance is called:

  instance Eq Int {
    (==) : eqInt();
  }

  a new function is added to the environment:

  sig == : (Int, Int) -> Bool
  eqInt(m, n)
*)

open CommonTypes

(* Subkind (Class) environment *)
module SubkindEnv = Env.String

let subkind_env : Types.subkind_environment = 
  List.fold_left
    (fun env (name, t) ->
      SubkindEnv.bind name t env)
      SubkindEnv.empty
    [ 
      "Mono"     , `Decl (None, (lin_any, "Mono"));
      "Any"      , `Decl (None, (lin_any, "Any"));
      "Lin"      , `Decl (None, (lin_unl, "Any")); (* for linear effect vars *)
      "Base"     , `Decl (None, (lin_unl, "Base"));
      "Session"  , `Decl (None, (lin_any, "Session"));
      "Eff"      , `Decl (Some pk_row , (default_effect_lin, "Eff"));
      (*"Eq"       , `Class ((pk_type, (lin_unl, "Eq")), [], Utility.StringMap.empty);*)
      (*"Num"    , `Class ((pk_type , (lin_unl, "Effect")), [], []);*)
    ]
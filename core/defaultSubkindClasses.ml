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
  op f == g {
    eqInt(f, g)
  }


  To derive from another class:

  class Ord : a::Eq {
    ...
  }

  Or to specify a specific restriction, if not will create one:

  class Eq : a::Type(Any, Base) {
  ...
  }

  TODO: the previous example is a potential flaw for bloat, perhaps find a way to resolve to best fit subkind?

  Changing the previous order:

  - Now Eq<:Type and Base<:Eq


*)

open CommonTypes

(* Subkind (Class) environment *)
module SignatureEnv = Env.String

(* TODO: subkinds' signatures are optional*)
let subkind_env : Types.subkind_environment = 
  List.fold_left
    (fun env (name, t) ->
      SignatureEnv.bind name t env)
      SignatureEnv.empty
    [ 
      "Mono"     , `Decl (pk_type, (lin_any, "Mono"));
      "Any"      , `Decl (pk_type, (lin_any, "Any"));
      "Lin"      , `Decl (pk_type, (lin_unl, "Any")); (* for linear effect vars *)
      "Base"     , `Decl (pk_type, (lin_unl, "Base"));
      "Session"  , `Decl (pk_type, (lin_any, "Session"));
      "Eff"      , `Decl (pk_row , (default_effect_lin, "Effect"));
      (*"Num"    , `Class ((pk_type , (lin_unl, "Effect")), [], []);*)
    ]
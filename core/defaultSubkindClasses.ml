(* Desugaring of Subkind Classes.

  Adds the signature to the environment.

  class Eq <: Type : a {
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



*)

open CommonTypes

(* Subkind (Class) environment *)
module SignatureEnv = Env.String


let subkind_env : Types.subkind_environment = 
  List.fold_left
    (fun env (name, t) ->
      SignatureEnv.bind name t env)
      SignatureEnv.empty
    [ 
      "Base"    , (pk_type, (lin_unl, "Base"));
    ]
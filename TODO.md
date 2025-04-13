+ class function (dl_unl, false) location is client; block is just a wrapper for the other function

+ Class method names might be better as patterns

+ We must resolve in desugarTYVAR but not how we were. We must do this instance.

+ Altered typeSugar to not add datatypes to environment, instead add to subkind environment. Along with a dict that takes tuple as key, i.e. (+, Int)

+ Subkind classes dont need to have quantifiers but could use maps

+ Use a stringmap for the operator dict

+ Class env, subkinds should be found not class's restriction name.

+ Instead of sigs add datatype to env in typeSugar and transformSugar, check "++" for the minimum places

+ Make constraints dynamic objects and then we are very close to being finished
+ Update repl.ml and types.ml so you can print subkind environment.

+ Remove subkind_env from desugar datatypes.

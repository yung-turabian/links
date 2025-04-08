+ Update ClassMethods, closer but still some kinks. Don't need to
take it all the way to IR but up till sugartoir and set the associated binder to its value.
    + However, instance operators may need binders since they arevariables pointing towards something.

+ Update repl.ml and types.ml so you can print subkind environment.

1. Subkinds need to create new restrctions and dynamically add constraints,
    - This relationship acts as a key->value, in that respective order.

+ Subkind declarations need to be handled like typename Aliases, unbounded
 after parsing
    + Constructor pattern is when either a type (Int) or an alias is passed.

+ types.ml 993, dynamic creation of Constraints

+ Find an optimal way to remove minus/fminus from parsing table as it
  interferes with method defs

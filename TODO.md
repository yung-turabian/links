# TODO

+ To be allowed to have subclass use superclass functions, update parent constraints can_type_be in Constraint mod.

+ A superclass should adopt all operations, if there are any
    + It will get all templates and implementations and when a new instance is created for subclass, add those functions

+ Not defining a pk or sk will fuck up the constraint check

## Future plans

+ Deriving system, where Links will figure out an instance for say Show or Eq

+ SO YOU CAN USE MINUS, when parsing operators should be of Section.Name (or Section.Minus) instead read as strings.
    + Has to be reworked how minus is interpreted

+ Update repl.ml and types.ml so you can print subkind environment.

+ Better error handling.
    + Check that an instance completes the class' contracts

+ If not polymorphic functions can be resolved at compile time but if not then will be resolved at runtime in evalir.

+ Make prelude_mods work as right now it results in NotFound because classes/instances cant be accessed from other modules.

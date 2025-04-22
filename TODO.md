# TODO

+ A superclass should adopt all operations, if there are any
    + It will get all templates and implementations and when a new instance is created for subclass, add those functions


## Future plans

+ SO YOU CAN USE MINUS, when parsing operators should be of Section.Name (or Section.Minus) instead read as strings.
    + Has to be reworked how minus is interpreted

+ Update repl.ml and types.ml so you can print subkind environment.

+ Better error handling.
    + Check that an instance completes the class' contracts

+ If not polymorphic functions can be resolved at compile time but if not then will be resolved at runtime in evalir.

+ Make prelude_mods work as right now it results in NotFound because classes/instances cant be accessed from other modules.

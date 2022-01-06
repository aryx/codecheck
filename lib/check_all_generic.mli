
val check_file: 
  ?verbose:bool ->
  ?find_entity: Entity_generic.entity_finder option ->
  AST_generic.program -> unit

val check: 
 show_progress:bool ->
 rank:bool ->
 filter:int -> r2c:bool -> string -> string list -> unit

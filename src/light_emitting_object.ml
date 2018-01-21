open Light
open Actor
open Structures
       
(* module Light_Emitting_Object = (
 *   struct
 *   end :
 *     sig
 *       include ACTOR_BASE
 *       include LIGHT_BASE with type point := point (\* It's called "destructive substitution". ~ stackoverflow*\)
 *     end
 * );; *)

module Light_Emitting_Object (* (Actor:ACTOR) (Light:LIGHT_BASE) *) :
sig
  include ACTOR  with type 'a constrArgsActor = (module ACTOR_BASE with type actor = 'a)
  include LIGHT_BASE with type point := point
  type _ constrArgsLight
  val createLight : 'a constrArgsLight -> 'a -> light
end
  = struct
  (* module Actor = Actor
   * module Light = Light *)


  type point = point3D
  type ray = Structures.ray                       
  type 'a constrArgsActor =
    (module ACTOR_BASE with
              type actor = 'a)
      
  type actor = {isCollision    : (ray->bool);
                collisionPoint : (ray->point);
                normalVector   : (point->point)}
                  
                          
  let isCollision    actor ray   = actor.isCollision ray;;
  let collisionPoint actor ray   = actor.collisionPoint ray;;
  let normalVector   actor point = actor.normalVector point;;
 
  let createActor (type a) =
    function (module Actor :
              ACTOR_BASE with
                       type actor = a) ->
      function (a:a) -> 
        {isCollision    = (Actor.isCollision    a);
         collisionPoint = (Actor.collisionPoint a);
         normalVector   = (Actor.normalVector   a)};;


                   
                   
  type intensity = float
  type vector = vector3D
  
  type 'a constrArgsLight =
    (module LIGHT_BASE with
              type light = 'a)
  
  type light = {lightFactor:(vector -> point -> intensity);
                vectorFromPointToLight:(point -> vector option);}
  
  let createLight (type a) =
    function (module Light :
              LIGHT_BASE with type light = a) ->
      function l -> {lightFactor = (Light.lightFactor l);
                     vectorFromPointToLight = (Light.vectorFromPointToLight l);};;
  
  let lightFactor light vector = light.lightFactor vector;;
  let vectorFromPointToLight light point = light.vectorFromPointToLight point;;
                   
                   
end;;
       

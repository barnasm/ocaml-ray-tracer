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

module Light_Emitting_Object
  = struct
  type point = point3D
  type ray = Structures.ray                       
  type 'a constrArgsActor =
    (module ACTOR_BASE with
              type actor = 'a)
      
  type actor = {isCollision    : (ray->bool);
                collisionPoint : (ray->point);
                normalVector   : (point->point)}
                  
                   
  type intensity = float
  type vector = vector3D
  
  type 'a constrArgsLight =
    (module LIGHT_BASE with
              type light = 'a)
  
  type light = {lightFactor:(vector -> point -> intensity);
                vectorFromPointToLight:(point -> vector option);}

  let createLEO (type a) (type l)
        (module Act : ACTOR_BASE with type actor = a)
        (module Lig : LIGHT_BASE with type light = l)
        actor light =
    Actor.createActor (module Act) actor, Light.createLight (module Lig) light;;
  
  let getActor (a,l) = a;;
  let getLight (a,l) = l;;
end;;
       

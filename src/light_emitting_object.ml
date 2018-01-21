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
         (Actor : ACTOR)
         (Light : LIGHT_BASE)
       : (sig
           include ACTOR
           include (LIGHT_BASE with type point := point)
           type 'a constrArgsLight
           type leo 
           val createLight : 'a constrArgsLight -> 'a -> light

           val createLEO   : (* 'a constrArgsActor -> *) actor ->
                             (* 'l constrArgsLight -> *) light -> leo
                             
         end)
  = struct
  include Actor
  
  (* type point = point3D *)
  (* type ray = Structures.ray *)
               
  (* type 'a constrArgs =
   *   (module ACTOR_BASE with
   *             type actor = 'a) *)

  (* type actor = {isCollision    : (ray->bool);
   *               collisionPoint : (ray->point);
   *               normalVector   : (point->point)} *)

  type intensity = float
  type vector = vector3D
  type 'a constrArgsLight =
    (module LIGHT_BASE with
              type light = 'a)
  
                   
  type light = {lightFactor:(vector -> point -> intensity);
                vectorFromPointToLight:(point -> vector option);}

  type leo = {actor:actor; light:light}
  (* let isCollision    actor ray   = actor.isCollision ray;;
   * let collisionPoint actor ray   = actor.collisionPoint ray;;
   * let normalVector   actor point = actor.normalVector point;; *)
  
  (* let createActor (type a) =
   *   function (module Actor :
   *             ACTOR_BASE with
   *                      type actor = a) ->
   *     function (a:a) -> 
   *       {isCollision    = (Actor.isCollision    a);
   *        collisionPoint = (Actor.collisionPoint a);
   *        normalVector   = (Actor.normalVector   a)};; *)

  let lightFactor light vector = light.lightFactor vector;;
  let vectorFromPointToLight light point = light.vectorFromPointToLight point;;
  
  let createLight (type a) =
    function (module Light :
              LIGHT_BASE with type light = a) ->
      function l -> {lightFactor = (Light.lightFactor l);
                     vectorFromPointToLight = (Light.vectorFromPointToLight l);};;

  let createLEO (* (type a) (type l) *) =
        (* function (module Actor : ACTOR_BASE with type actor = a) ->
         *   function (a:a) -> *)
    function a ->
      (* function (module Light : LIGHT_BASE with type light = l) -> *)
      function l ->
                (* let a = createActor (module Actor : ACTOR_BASE  with type actor = a) a in *)
                (* let l = createLight (module Light) l in *)
        {actor=a; light=l};;
end;;
       

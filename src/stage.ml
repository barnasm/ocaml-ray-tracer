open Structures;;
open Screen;;
open Camera;;
open Actor;;
open Light;;

type obj = | Camera of (module CAMERA)
           | Screen of (module SCREEN)
           | Light  of (module LIGHT)
           | Actor  of (module ACTOR);;

(* type actor = | t of Triangle.actor;; *)
           
(* let test = Camera(Camera_Point);; *)
(* type stageObjs = obj list;; *)

let ftest (type a) (module M : ACTOR with type constrArgs = a) ca =
  let m = M.createActor ca in
  0
;;

let ftest2 (type r) (type a) (module Actor : ACTOR with type ray = r and type actor = a) ray actor =
  Actor.isCollision ray actor
;;

open Structures;;

module type ACTOR = sig
  type actor
  type point
  type ray
  type constrArgs

  val createActor : constrArgs -> actor
  val isCollision : ray -> actor -> bool 
  val collisionPoint : ray -> actor -> point
  val normalVector : point -> actor -> point (* treat the result as a normal vector *)
end;;

type sphereConstrArgs = {pos:point3D; radius:float};;
module Sphere
       : (ACTOR with
            type point = point3D and
            type ray = ray and
            type constrArgs = sphereConstrArgs)
  = struct
  type point = point3D
  type actor = Sphere of point * float
  type ray = Structures.ray
  type constrArgs = sphereConstrArgs

  let createActor args = (Sphere(args.pos,args.radius));; 
  let isCollision ray (Sphere(cnt, rad)) =
    let distanceSq x0 x1 x2 =
      ((distance3d' x1 x0) *. (distance3d' x2 x1) -. ((x1 --- x0) -.- (x2 --- x1))**2.) /. (distance3d' x2 x1)
    in
    if (rad*.rad) < (distanceSq cnt ray.start ray.pxPnt) then false else true ;;

  (* closest point *) 
  let collisionPoint {start=o; pxPnt=p} (Sphere(c, r)) =
    let l = normalize @@ p---o in
    let f = (l -.- (o---c)) in
    let s = (f *. f) -. (o ||-|| c) +. (r *. r) in
    if s < 0.
    then (* o -+- (l -*- (~-.f)) *)failwith "sphere collision point doesn't exist\n"
    else
      let s = sqrt s in
      o -+- (l -*- min (~-. f +. s) (~-. f -. s));;

  let normalVector p (Sphere(c,_)) =
    p --- c;;
end;;

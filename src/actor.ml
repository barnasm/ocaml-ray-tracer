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
    then failwith "sphere collision point doesn't exist\n"
    else
      let s = sqrt s in
      o -+- (l -*- min (~-. f +. s) (~-. f -. s));;

  let normalVector p (Sphere(c,_)) =
    p --- c;;
end;;

type triangleConstrArgs = {p1:point3D; p2:point3D; p3:point3D};;
module Triangle
       : (ACTOR with
            type point = point3D and
            type ray = ray and
            type constrArgs = triangleConstrArgs)
  = struct
  type point = point3D
  type actor = Triangle of triangleConstrArgs (* is it ok?? *)
  type ray = Structures.ray
  type constrArgs = triangleConstrArgs

  let createActor args = Triangle(args);;

  let mtir = ref (false, {x=0.; y=0.; z=0.});;
  let mollerTrumboreIntersection ray (Triangle(triangle)) =
    let edge1 = triangle.p2 --- triangle.p1 in
    let edge2 = triangle.p3 --- triangle.p1 in
    let rayVec = ray.pxPnt --- ray.start in
    let h = rayVec -**- edge2 in
    let a = edge1 -.- h in
    let f = 1. /. a in
    let s = ray.start --- triangle.p1 in
    let u = f *. (s -.- h) in
    let q = s -**- edge1 in
    let v = f *. (rayVec -.- q) in
    let t = f *. (edge2 -.- q) in
    if a = 0. || u < 0. || u > 1. || v < 0. || u+.v > 1. || t = 0.
    then (mtir := (false, {x=0.; y=0.; z=0.}); !mtir)
    else (mtir := (true, ray.start -+- (rayVec -*- t)); !mtir);;
  
  let isCollision ray triangle = let (r,_) = mollerTrumboreIntersection ray triangle in r;;
  let collisionPoint ray triangle =
    let (t,p) = !mtir in
    if t = false
    then failwith "triangle collision point doesn't exist\n"
    else p;;

  let normalVector p (Triangle(triangle)) =
    (triangle.p1 --- triangle.p2) -**- (triangle.p1 --- triangle.p3);;
end;;

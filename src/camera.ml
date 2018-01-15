open Structures;;

module type CAMERA = sig
  type point
  type camera

  val getCamPos : camera -> point
  (* val getRayStart : camera -> point list *)
  val createCamera : ?radius:float -> point -> camera
end;;

module Camera_Point : (CAMERA with type point = point3D) = struct
  type point = point3D
  type camera = Camera of point * float

  let getCamPos (Camera(pos,_)) = pos;;
  let getRayStart (Camera(pos,_)) = [pos];; 
  let createCamera ?(radius = 0.) pos = Camera(pos, radius);;
end;;

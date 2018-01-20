open Structures;;

module type CAMERA = sig
  type point
  type camera

  val getCamPos : camera -> point
  val getRayStart : camera -> point list
  val createCamera : ?radius:float -> point -> camera
end;;

module Camera_Point : (CAMERA with type point = point3D) = struct
  type point = point3D
  type camera = Camera of point * float

  let getCamPos (Camera(pos,_)) = pos;;
  let getRayStart (Camera(pos, rad)) =
    (* Random.self_init(); *)
    let rndR () = (sqrt (Random.float 1.)) *. rad in
    let rec aux n acc = match n with
        0 -> acc
      | _ ->
         let angle = Random.float (2. *. _PI) in
         let rr = rndR() in
         aux (n-1) ((pos -+- {x= rr *. (cos angle); y= rr *. (sin angle); z= 0.})::acc)
    in
    pos :: aux 0 [];; 
  let createCamera ?(radius = 1.5) pos = Camera(pos, radius);;
end;;

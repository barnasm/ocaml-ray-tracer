open Structures;;

type constrArgsLens = {point:point3D};;
module type LENS = sig
  type lens
  type point = point3D
  type ray = Structures.ray
  type constrArgs = constrArgsLens

  val createLens : constrArgs -> lens
  val lensMap : lens -> point -> ray -> ray                                   
end;;

module Lens_Screen_Focus : (LENS)
  = struct
  type constrArgs = constrArgsLens
  type lens = constrArgs
  type point = point3D
  type ray = Structures.ray
                
  let createLens point = point;;
  let lensMap lens cam ray =
    let s = ray.pnt -+- (ray.start --- cam) in
    let e = (lens.point.z) /. (angleCos lens.point (ray.pnt --- ray.start))  in
    (* print_p3d s;
     * print_p3d (ray.pnt -+- (normalize(ray.pnt --- ray.start) -*- e));
     * print_string "\n"; *)
    {start= s; pnt= ray.pnt -+- (normalize(ray.pnt --- ray.start) -*- e)};;
  
end;;

module Lens_Camera_Focus : (LENS)
  = struct
  type constrArgs = constrArgsLens
  type lens = constrArgs
  type ray = Structures.ray
  type point = point3D

  let createLens dist = dist;;
  let lensMap lens cam ray = let dst = distance3d cam lens.point in
                             {start= ray.start; pnt= cam -+- ((normalize (ray.pnt --- cam)) -*- dst)};;
end;;


module type CAMERA = sig
  module Lens : LENS
           
  type point
  type camera
  type vector
  type ray = Lens.ray
  type lens = Lens.lens
         
  val createCamera : ?radius:float -> ?vec:vector3D -> lens -> point -> camera
  val getCamPos : camera -> point
  val lensMap : camera -> ray -> ray
  val getRayStart : camera -> point list
end;;

module Camera_Disk (Lens : LENS) : (CAMERA with type point = point3D) = struct
  module Lens = Lens
  
  type point = point3D
  type vector = vector3D
  type ray = Lens.ray
  type lens = Lens.lens
  type camera = Camera of point * float * vector * lens

  let createCamera ?(radius = 7.0) ?(vec={x=0.; y=0.; z=1.}) lens pos = Camera(pos, radius, vec, lens);;
  let getCamPos (Camera(pos,_,_,_)) = pos;;
  let lensMap (Camera(pos,_,_,lens)) = Lens.lensMap lens pos 
  let getRayStart (Camera(pos, rad, vec, _)) =
    (* Random.self_init(); *)
    let rndR () = (sqrt (Random.float 1.)) *. rad in
    let rec aux n acc = match n with
        0 -> acc
      | _ ->
         let angle = Random.float (2. *. _PI) in
         let rr = rndR() in
         let x = rr *. (cos angle) and
             y = rr *. (sin angle)
         in

         let u =
           let a = {x = ~-.(vec.y); y = vec.x; z = 0.} and
               b = {x = 0.        ; y = vec.z; z = ~-.(vec.y)} and
               c = {x = vec.z     ; y = 0.   ; z = ~-.(vec.x)}
           in
           if (vec ||-|| a) > (vec ||-|| b)
           then
             if (vec ||-|| a) > (vec ||-|| c)
             then a
             else c
           else
             if (vec ||-|| b) > (vec ||-|| c)
             then b
             else c
         in
         let w = vec -**- u in
         (* aux (n-1) ((pos -+- {x= rr *. (cos angle); y= rr *. (sin angle); z= 0.})::acc) *)
         aux (n-1) ((pos -+- (((normalize u) -*- x) -+- ((normalize w) -*- y)))::acc)
    in
    pos :: aux 0 [];; 
  
end;;

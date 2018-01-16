open Structures;;

module type SCREEN = sig
  type point
  type vector
  type resolution
  type screen
         
  val getPxPosList : screen -> point list array array
  (* val getPxPosLazy : unit ->  point list Lazy.t *)
  val getResolution : screen -> resolution
  val pxPostion : screen -> int -> int -> point list
  val createScreen : screen -> screen
                                  
end;;

type screenRectangleConstrArgs = {p1:point3D; p2:point3D; p3:point3D; resolution:resolution};;
module Screen_Rectangle
       :(SCREEN with type point = point3D and
                     type vector = vector3D and
                     type resolution = resolution and
                     type screen = screenRectangleConstrArgs)
  = struct
  type point = point3D
  type vector = vector3D
  type resolution = Structures.resolution
  type screen = screenRectangleConstrArgs

  let createScreen scr = scr;;
  let getResolution scr = scr.resolution;;

  let pxPostion scr r c =
    let res  = scr.resolution in
    let
      pxw = (scr.p2 --- scr.p1) -/- float_of_int(res.width) and
      pxh = (scr.p3 --- scr.p1) -/- float_of_int(res.height) 
    in
    let center = scr.p1
                 -+- ( pxw -*- (float_of_int c +. 0.5))
                 -+- ( pxh -*- (float_of_int r +. 0.5))
    in
    [(* center; *)
      center --- (pxw-/-4.) --- (pxh-/-4.);
      center -+- (pxw-/-4.) --- (pxh-/-4.);
      center --- (pxw-/-4.) -+- (pxh-/-4.);
      center -+- (pxw-/-4.) -+- (pxh-/-4.);
    ];;

  let getPxPosList scr =
    let res  = scr.resolution in
    let rec iter width height acc =
      for i = 0 to height do
        for j = 0 to width do acc.(i).(j) <- pxPostion scr i j; done;
      done;
      acc
    in
    iter (res.width -1) (res.height -1) (Array.make_matrix res.height res.width []);;
end;;

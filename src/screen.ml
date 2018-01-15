open Structures;;

module type SCREEN = sig
  type point
  type vector
  type resolution
  type screen
         
  val getPxPosList : screen -> point list array array
  (* val getPxPosLazy : unit ->  point list Lazy.t *)
  val getResolution : screen -> resolution
  val pxPostion : screen -> int -> int -> point
  (* val screenCreate : point point resolution -> screen *)
                                  
end;;

module Screen_Axis_Parallel_Rectangle
       (* :(SCREEN with type point = point3D and
        *               type vector = vector3D and
        *               type resolution = resolution) *)
  = struct
  type point = point3D
  type vector = vector3D
  type resolution = Structures.resolution
  type screen = Screen of point * point * resolution
  (* let mutable width = 256 and mutable height = 256;; *)

  let createScreen p1 p2 res = Screen (p1, p2, res);;
  let getResolution (Screen(_,_,res)) = res;;

  let pxPostion (Screen(p1,p2,res)) r c =
    let
      pxw = (abs_float (p1.x -. p2.x)) /. float_of_int(res.width) and
      pxh = (abs_float (p1.y -. p2.y)) /. float_of_int(res.height)
    in
    let center = {x= p1.x +. (pxw /. 2.) +. pxw *. float_of_int(c);
                  y= p1.y -. (pxh /. 2.) -. pxh *. float_of_int(r);
                  z=0.}
    in
    [(* center; *)
      {center with x = center.x -. (pxw/.4.); y = center.y -. (pxh/.4.)};
      {center with x = center.x +. (pxw/.4.); y = center.y -. (pxh/.4.)};
      {center with x = center.x -. (pxw/.4.); y = center.y +. (pxh/.4.)};
      {center with x = center.x +. (pxw/.4.); y = center.y +. (pxh/.4.)};
    ];;
  
  let getPxPosList (Screen(p1,p2,res) as scr) =
    let rec iter width height acc =
      for i = 0 to height do
        for j = 0 to width do acc.(i).(j) <- pxPostion scr i j; done;
      done;
      acc
    in
    iter (res.width -1) (res.height -1) (Array.make_matrix res.height res.width []);;
end;;

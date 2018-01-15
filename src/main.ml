open ImgFile;;
open Screen;;
open Camera;;
open Structures;;
open Actor;;

let print_p3d p =
  print_float p.x; print_string "\t";
  print_float p.y; print_string "\t";
  print_float p.z; print_string "\n";;

let p1 = {x= float_of_int(-ImgFile.width) /. 2. ; y= float_of_int(ImgFile.height) /. 2. ;z=0.};;
let p2 = {x= float_of_int(ImgFile.width) /. 2.  ; y= float_of_int(-ImgFile.height) /. 2. ;z=0.};;

let camera = Camera_Point.createCamera {x=0.; y=0.; z= ~-.556.};;
let screen = Screen_Axis_Parallel_Rectangle.createScreen p1 p2 {width=ImgFile.width; height=ImgFile.height};;
let pxpos = Screen_Axis_Parallel_Rectangle.getPxPosList screen;;
let actor = Sphere.createActor {pos={x=0.; y=0.; z= 128.}; radius=64.};;

let computePxColAA l =
  let n = 255 / (List.length l) in
  let rec aux l v = match l with
      [] -> v
    | h::t when
           let ray = {start= Camera_Point.getCamPos camera; pxPnt= h} in
           Sphere.isCollision ray actor
      ->
       let ray = {start= Camera_Point.getCamPos camera; pxPnt= h} in
       let nv = Sphere.normalVector (Sphere.collisionPoint ray actor) actor in
       let f = angleFactor {x= 1. ; y= 0. ;z= ~-.0.3} nv in
       aux t (v+ int_of_float(float_of_int n *. f +.0.1))
       (* aux t (v+n) *)
    | h::t -> aux t v
  in let v = aux l 0 in
  Graphics.rgb v v v;;

Graphics.open_graph "";;
Graphics.resize_window ImgFile.width ImgFile.height;;
let prevImgMx = Graphics.dump_image @@ Graphics.create_image ImgFile.width ImgFile.height;;

for i = 0 to ImgFile.height-1 do
  for j = 0 to ImgFile.width-1 do
    prevImgMx.(i).(j) <- computePxColAA pxpos.(i).(j)
  done;
done;;

ImgFile.dump_to_file prevImgMx;;

Graphics.draw_image (Graphics.make_image prevImgMx) 0 0;;

Graphics.wait_next_event [Graphics.Key_pressed];;
(* Graphics.loop_at_exit [Graphics.Key_pressed] (fun _ -> ());; *)
Graphics.close_graph ();;


(* with
 *   | Graphic_failure("fatal I/O error") -> print_string "exit - not saved\n" *)

let s = {x= 0. ; y= 0. ;z=0.};;
let p = {x= 0. ; y= 0. ;z=1.};;



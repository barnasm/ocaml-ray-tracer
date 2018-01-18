open ImgFile;;
open Structures;;
open Screen;;
open Camera;;
open Actor;;
open Stage;;

Random.self_init();;

let print_p3d p =
  print_float p.x; print_string "\t";
  print_float p.y; print_string "\t";
  print_float p.z; print_string "\n";;

let res = {width=ImgFile.width; height=ImgFile.height} ;;
let p1 = {x= float_of_int(-ImgFile.width) /. 2. ; y= float_of_int(ImgFile.height)  /. 2. ;z=0.};;
let p2 = {x= float_of_int(ImgFile.width)  /. 2. ; y= float_of_int(ImgFile.height)  /. 2. ;z=0.};;
let p3 = {x= float_of_int(-ImgFile.width) /. 2. ; y= float_of_int(-ImgFile.height) /. 2. ;z=0.};;

let camera = Camera_Point.createCamera {x=0.; y=0.; z= ~-.556.};;
let screen = Screen_Rectangle.createScreen {p1=p1; p2=p2; p3=p3; resolution=res};;
(* let pxpos = Screen_Rectangle.getPxPosList screen;; *)

let actor = Sphere.createActor {pos={x=0.; y=0.; z= 128.}; radius=128.};;
let actor2 = Triangle.createActor {p1={x=0.; y=0.; z=256.}; p2={x=64.; y=64.; z= ~-.10.}; p3={x=64.; y=0.; z=0.}};;
let actors = [];;
let actors = Actor.createActor (module Sphere) (Sphere.createActor {pos={x=0.; y=0.; z= 128.}; radius=128.}) :: actors;;
let actors = Actor.createActor (module Triangle) actor2 :: actors;;

print_p3d @@ Triangle.normalVector actor2 p1;;
print_int @@ if Triangle.isCollision actor2 {start= Camera_Point.getCamPos camera; pxPnt= {x=0.; y=0.; z= 0.}} = true
             then 1
             else 0;;
print_string "\n";;
print_p3d @@  Triangle.collisionPoint actor2 {start= Camera_Point.getCamPos camera; pxPnt= {x=0.; y=0.; z= 0.}} ;;

let isCollision (type r) (type a) (module Actor : ACTOR with type ray = r and type actor = a) ray actor =
  Actor.isCollision ray actor
;;

let computePxColAA ray =
  let aux actor = 
    if isCollision (module Sphere) actor ray
    then let nv = Sphere.normalVector actor (Sphere.collisionPoint actor ray) in
         let f = angleFactor {x= 1. ; y= 0. ;z= 0.} nv in
         {r=int_of_float(255.*.f); g=int_of_float(25.*.f); b=int_of_float(55.*.f)}
    else {r=0; g=0; b=0}
  in
  List.fold_left aux [] actors
;;
let checkRay ray cols = (computePxColAA ray)::cols;;
let colorAvg cols =
  let c,n = List.fold_left (fun (sumSqcol,cnt) cCur ->
                ({r=sumSqcol.r+cCur.r*cCur.r ; g=sumSqcol.g+ cCur.g*cCur.g ; b=sumSqcol.b+ cCur.b*cCur.b}), cnt+1)
              ({r=0; g=0; b=0}, 0) cols
  in
  {r= int_of_float(sqrt (float_of_int (c.r/n)));
   g= int_of_float(sqrt (float_of_int (c.g/n)));
   b= int_of_float(sqrt (float_of_int (c.b/n)))}
;;

Graphics.open_graph "";;
Graphics.resize_window ImgFile.width ImgFile.height;;
let prevImgMx = Graphics.dump_image @@ Graphics.create_image ImgFile.width ImgFile.height;;

let genBitmap () = 
  for i = 0 to ImgFile.height-1 do
    for j = 0 to ImgFile.width-1 do
      let c =
        colorAvg @@ List.fold_left
                      (fun cList camPnt ->
                        List.fold_left
                          ( fun cList pxPnt -> checkRay {start= camPnt; pxPnt= pxPnt} cList )
                          cList (Screen_Rectangle.pxPostion screen i j)(* (prevImgMx.(i).(j)) *) )
                      [] (Camera_Point.getRayStart camera)
      in
      prevImgMx.(i).(j) <- Graphics.rgb c.r c.g c.b(* prevImgMx.(i).(j) <- computePxColAA pxpos.(i).(j) *)
    done;
  done;;
genBitmap();;

ImgFile.dump_to_file prevImgMx;;

Graphics.draw_image (Graphics.make_image prevImgMx) 0 0;;

Graphics.wait_next_event [Graphics.Key_pressed];;
(* Graphics.loop_at_exit [Graphics.Key_pressed] (fun _ -> ());; *)
Graphics.close_graph ();;


(* with
 *   | Graphic_failure("fatal I/O error") -> print_string "exit - not saved\n" *)

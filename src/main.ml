open ImgFile;;
open Structures;;
open Screen;;
open Camera;;
open Actor;;
open Stage;;

Random.self_init();;

(* let create (type a) (type c) (type cp)
 *       (module ActorObj : ACTOR with type actor = a and type constrArgs = a)  (constrArgs: a)= 
 *   ActorObj.createActor constrArgs;; *)

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

let actor = Sphere.createActor {pos={x=0.; y=0.; z= 128.}; radius=128.};;
let actor2 = Triangle.createActor {p1={x= ~-.128.; y= ~-.128.; z= 0.};
                                   p2={x=    128.; y=    128.; z= 0.};
                                   p3={x=     64.; y=  ~-.64.; z= 64.}};;

let actor3 = Triangle.createActor {p1={x=128.; y= 128.; z= 0.};
                                   p2={x=128.; y= 63.;  z= 0.};
                                   p3={x=64. ; y= 128.; z= 0.}};;
let actors = [];;
let actors = Actor.createActor (module Sphere)  actor  :: actors;;
let actors = Actor.createActor (module Triangle) actor3 :: actors;;
let actors = Actor.createActor (module Triangle) actor2 :: actors;;
(* let a = Actor.createActor actor2 (module Triangle);; *)
(* let  m = (module Triangle:ACTOR);; *)
(* let a = Actor.createActor2 ((module Triangle:ACTOR), actor2);; *)

let minDst ((d1,_,_)as a1) ((d2,_,_)as a2) =
  (* todo if distances are equal then create invisible triangle with normal vector turned in a ray direction or take average of normal vectors  *)
  if d1 < d2 then a1 else a2;;

let computePxColAA ray =
  let light = {x= 1. ; y= 0. ;z= 0.} in
  let dstFrom = ray.start in 

  let aux curHitPnt actor =
    if Actor.isCollision actor ray
    then let cp =  Actor.collisionPoint actor ray in
         let dst = (distance3d' dstFrom cp) in
         match curHitPnt with
           None -> Some (dst, cp, actor)
         | Some curMin -> Some (minDst curMin (dst, cp, actor))
    else curHitPnt
  in
  match List.fold_left aux None actors with
    None -> {r=0; g=0; b=0}
  | Some (_, cp, actor) ->
     let nv = Actor.normalVector actor cp in
     let f = angleFactor light nv in
     {r=int_of_float(255.*.f); g=int_of_float(255.*.f); b=int_of_float(255.*.f)}
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
                      (fun colList camPnt ->
                        List.fold_left
                          ( fun colList pxPnt -> checkRay {start= camPnt; pxPnt= pxPnt} colList )
                          colList (Screen_Rectangle.pxPostion screen i j)(* (prevImgMx.(i).(j)) *) )
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

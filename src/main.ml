open ImgFile;;
open Structures;;
open Screen;;
open Camera;;
open Light;;
open Actor;;
open Stage;;
open Light_emitting_object;;

Random.self_init();;
let minDst ((d1,_,_)as a1) ((d2,_,_)as a2) =
  (* todo if distances are equal then create invisible triangle with normal vector turned in a ray direction or take average of normal vectors  *)
  if d1 < d2 then a1 else a2;;

let cloestActorHitPoint ray =
  let dstFrom = ray.start in 
  let aux curHitPnt actor =
    if Actor.isCollision actor ray
    then let cp =  Actor.collisionPoint actor ray in
         let dst = (distance3d' dstFrom cp) in
         if dst < 0.00000001 then curHitPnt
         else
           match curHitPnt with
             None -> Some (dst, cp, actor)
           | Some curMin -> Some (minDst curMin (dst, cp, actor))
    else curHitPnt
  in
  List.fold_left aux None actors
;;

let cloestActorHitPoint2 ray vec =
  let dstFrom = ray.start in 
  let aux curHitPnt actor =
    if Actor.isCollision actor ray
    then let cp =  Actor.collisionPoint actor ray in
         let dst = (distance3d' dstFrom cp) in
         let v = cp --- ray.start in
         if dst < 0.00001 || (angleCos vec v) > 0. then curHitPnt
         else
           match curHitPnt with
             None -> Some (dst, cp, actor)
           | Some curMin -> Some (minDst curMin (dst, cp, actor))
    else curHitPnt
  in
  List.fold_left aux None actors
;;

let inShadow point light anv f =
  (* f;; *)
  match Light.vectorFromPointToLight light point with
    None -> f
  | Some (vec) ->
     match cloestActorHitPoint2 {start=point; pnt=(point --- vec)} vec  with
       None -> f
     | Some(d, cp, actor) ->
        (* f*.(1.-.f)**(1000./.(d**0.5)) *)
        if d < (distance3d' vec {x=0.;y=0.;z=0.})
        then ((((normalize anv) -.- (normalize vec)) -. 1.) /. ~-.2.) *. f*.(1.-.f)**(500./.(d**1.9))
        else f 
;;
    
let computePxColAA ray =
  match cloestActorHitPoint ray with
    None -> {r=0; g=0; b=0}
  | Some (_, cp, actor) ->
     let nv = Actor.normalVector actor cp in
     (* let actCol = Actor.getColor actor in *)
     (* let f = Light_Point.lightFactor light3 nv cp in *)
     let f = min 1. @@ List.fold_left (fun f l -> f +. (inShadow cp l nv (Light.lightFactor l nv cp))) 0. lights in
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
                          ( fun colList pxPnt ->
                            checkRay (Camera.lensMap camera {start= camPnt; pnt= pxPnt}) colList )
                          colList (Screen_Rectangle.pxPostion screen i j)(* (prevImgMx.(i).(j)) *) )
                      [] (Camera.getRayStart camera)
      in
      prevImgMx.(i).(j) <- Graphics.rgb c.r c.g c.b(* prevImgMx.(i).(j) <- computePxColAA pxpos.(i).(j) *)
    done;
  done;;
genBitmap();;

ImgFile.dump_to_file prevImgMx;;

(* Graphics.draw_image (Graphics.make_image prevImgMx) 0 0;; *)

(* Graphics.wait_next_event [Graphics.Key_pressed];; *)
(* Graphics.loop_at_exit [Graphics.Key_pressed] (fun _ -> ());; *)
Graphics.close_graph ();;


(* with
 *   | Graphic_failure("fatal I/O error") -> print_string "exit - not saved\n" *)

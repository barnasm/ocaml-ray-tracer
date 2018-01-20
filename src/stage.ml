open Structures;;
open Screen;;
open Camera;;
open Actor;;
open Light;;

(* module type STAGE = sig
 *   type stage
 *   type actors
 *   val addActor  : Actor.actor -> actors
 *   val getActors : ()          -> actors
 * end;; *)

(* module Stage = struct
 *   (\* type actors = Actor.actor list *\)
 *   (\* type lights = Light list *\)
 *   (\* type camera = Camera *\)
 *   let addActor a = a::act
 *   let 
 * end;; *)

(* let ftest2 (type r) (type a) (module Actor : ACTOR with type ray = r and type actor = a) ray actor =
 *   Actor.isCollision ray actor
 * ;; *)

let res = {width=ImgFile.width; height=ImgFile.height} ;;
let ss = 32.;;
let p1 = {x= ~-.ss ; y= ss; z= 0.};;
let p2 = {x= ss ; y= ss; z= 0.};;
let p3 = {x= ~-.ss ; y= ~-.ss; z=0.};;
(* let p1 = {x= float_of_int(-ImgFile.width) /. 2. ; y= float_of_int(ImgFile.height)  /. 2. ;z=0.};;
 * let p2 = {x= float_of_int(ImgFile.width)  /. 2. ; y= float_of_int(ImgFile.height)  /. 2. ;z=0.};;
 * let p3 = {x= float_of_int(-ImgFile.width) /. 2. ; y= float_of_int(-ImgFile.height) /. 2. ;z=0.};; *)


let camera = Camera_Point.createCamera {x=0.; y=0.; z= ~-.64.};;
let screen = Screen_Rectangle.createScreen {p1=p1; p2=p2; p3=p3; resolution=res};;

let p1 = {x= ~-.128. ; y= ~-.64. ;z=0.};;
let p2 = {x= 128.    ; y= ~-.64. ;z=0.};;
let p3 = {p1 with z= 256.};;
let p4 = {p2 with z= 256.};;
let p5 = {p3 with y= 128.};;
let p6 = {p4 with y= 128.};;

(* #################################################################################### *)
let crArgs = {pos={x=0.; y=0.; z= 128.}; radius=32.} ;;
let actor = Sphere.createActor {pos={x=0.; y=0.; z= 128.}; radius=32.};;

let actor2 = Triangle.createActor {p1=p2; p2=p1; p3=p3};;
let actor3 = Triangle.createActor {p1=p2; p2=p3; p3=p4};;
let actors = [];;

let actors = Actor.createActor (module Sphere)  actor  :: actors;;
let actors = Actor.createActor (module Sphere)
               (Sphere.createActor {crArgs with pos={x= ~-.32.; y= ~-.32.; z= 64.}})  :: actors;;
let actors = Actor.createActor (module Sphere)
               (Sphere.createActor {crArgs with pos={x= 32.; y= 32.; z= 192.}})  :: actors;;
let actors = Actor.createActor (module Sphere)
               (Sphere.createActor {crArgs with pos={x= 64.; y= 64.; z= 256.}})  :: actors;;
let actors = Actor.createActor (module Sphere)
               (Sphere.createActor {pos={x= 64.; y= ~-.48.; z= 128.}; radius=16.})  :: actors;;


let actors = Actor.createActor (module Triangle) actor3 :: actors;;
let actors = Actor.createActor (module Triangle) actor2 :: actors;;
let actors = Actor.createActor (module Triangle) (Triangle.createActor {p1=p4; p2=p3; p3=p5}) :: actors;;
let actors = Actor.createActor (module Triangle) (Triangle.createActor {p1=p4; p2=p5; p3=p6}) :: actors;;

(* #################################################################################### *)
let light  = Light_Environment.createLight {intensity=0.05};;
let light2 = Light_Direction.createLight   {intensity=0.6; direction= ({x= 0.5 ; y= ~-.1. ;z= 0.5} -*- 1000.)};;
let light3 = Light_Point.createLight   {intensity=1000.9; pos= {x= 64.; y= ~-.4.; z= 128.}; factor=0.8};;

let lights = [];;
let lights = Light.createLight (module Light_Environment) light ::lights;;
let lights = Light.createLight (module Light_Direction) light2 ::lights;;
let lights = Light.createLight (module Light_Point) light3 ::lights;;
(* let lights = Light.createLight (module Light_Direction)
 *                (Light_Direction.createLight
 *                   {intensity=0.7;
 *                    direction= {x= ~-.1. ; y= 1. ;z= 0.}}) ::lights;; *)

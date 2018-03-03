open Structures;;
open Screen;;
open Camera;;
open Actor;;
open Light;;
open Light_emitting_object;;

(* let i = float_of_string @@ Sys.argv.(1);; *)
let res = {width=ImgFile.width; height=ImgFile.height} ;;

let ss = 32.;;
let p1 = {x= ~-.ss ; y= ss; z= 0.};;
let p2 = {x= ss ; y= ss; z= 0.};;
let p3 = {x= ~-.ss ; y= ~-.ss; z=0.};;
(* let p1 = {x= float_of_int(-ImgFile.width) /. 2. ; y= float_of_int(ImgFile.height)  /. 2. ;z=0.};;
 * let p2 = {x= float_of_int(ImgFile.width)  /. 2. ; y= float_of_int(ImgFile.height)  /. 2. ;z=0.};;
 * let p3 = {x= float_of_int(-ImgFile.width) /. 2. ; y= float_of_int(-ImgFile.height) /. 2. ;z=0.};; *)

(* CAMERA  #################################################################################### *)
module Camera = Camera_Disk (Lens_Camera_Focus);;
let screen = Screen_Rectangle.createScreen {p1=p1; p2=p2; p3=p3; resolution=res};;

let focus={x= ~-.32.; y= 0.; z= 64.};;
let lens   = Camera.Lens.createLens {point={x= ~-.32.; y= 0.; z= 164.}};;
let camera = Camera.createCamera lens {x=0.; y=10.; z= ~-.64.};;

let p1 = {x= ~-.128. ; y= ~-.64. ;z=0.};;
let p2 = {x= 128.    ; y= ~-.64. ;z=0.};;
let p3 = {p1 with z= 256.};;
let p4 = {p2 with z= 256.};;
let p5 = {p3 with y= 128.};;
let p6 = {p4 with y= 128.};;

(* ACTORS  #################################################################################### *)
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

(*LIGHTS #################################################################################### *)
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

(*LIGHT EMITTING OBJEST - test #################################################################################### *)
let pos = {x= ~-.8.; y= ~-.55.; z= 45.} ;;
let leo = Light_Emitting_Object.createLEO (module Sphere) (module Light_Point)
            (Sphere.createActor {pos=pos; radius=4.})
            (Light_Point.createLight   {intensity=36.9; pos=pos; factor=1.0});;

(* let actors = Light_Emitting_Object.getActor leo :: actors;; *)     
let lights = Light_Emitting_Object.getLight leo :: lights;;

let _PI = 3.14159265358979312 ;;

type resolution = {width:int; height:int};;
type point3D  = {x:float; y:float; z:float};;
type vector3D = point3D;;
type color = {r:int; g:int; b:int};;
type ray = {start:point3D ; pnt:point3D};;

let print_p3d p =
  print_float p.x; print_string "\t";
  print_float p.y; print_string "\t";
  print_float p.z; print_string "\n";;

let distance3d' {x=x1;y=y1;z=z1} {x=x2;y=y2;z=z2} =
  (x1-.x2)**2. +. (y1-.y2)**2. +. (z1-.z2)**2.;;

let distance3d v u =
  sqrt(distance3d' v u);;

let crossProd3d {x=a1;y=a2;z=a3} {x=b1;y=b2;z=b3} =
  {x= a2*.b3 -. a3*.b2;
   y= a3*.b1 -. a1*.b3;
   z= a1*.b2 -. a2*.b1;};;

let dot3d {x=a1;y=a2;z=a3} {x=b1;y=b2;z=b3} =
  a1*.b1 +. a2*.b2 +. a3*.b3 ;;

let (---) {x=x1;y=y1;z=z1} {x=x2;y=y2;z=z2} = {x = x1-.x2; y = y1-.y2; z = z1-.z2}
let (-+-) {x=x1;y=y1;z=z1} {x=x2;y=y2;z=z2} = {x = x1+.x2; y = y1+.y2; z = z1+.z2}

let (-*-) {x=x1;y=y1;z=z1} s = {x = x1*.s; y = y1*.s; z = z1*.s}
let (-/-) {x=x1;y=y1;z=z1} s = {x = x1/.s; y = y1/.s; z = z1/.s}

let (-**-)  p1 p2 = crossProd3d p1 p2
let (-.-)   p1 p2 = dot3d       p1 p2
let (-||-)  p1 p2 = distance3d  p1 p2
let (||-||) p1 p2 = distance3d' p1 p2


let normalize ({x=x1;y=y1;z=z1} as p) =
  let dst = distance3d p {x=0.;y=0.;z=0.} in
  p -*- (1./.dst);;

let angleCos v1 v2 =
  (normalize v1) -.- (normalize v2);;

let angleRad v1 v2 =
  acos(angleCos v1 v2);;

let angle v1 v2 =
  (angleRad v1 v2) /. _PI *. 360.;;



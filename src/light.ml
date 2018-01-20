open Structures;;

module type LIGHT_BASE = sig
  type intensity = float
  type vector = vector3D
  type point = point3D
  type light
  val createLight : light -> light
  val lightFactor : light -> vector -> point -> intensity (* surface normal vector *)
  val vectorFromPointToLight  : light -> point -> vector option (* surface normal vector *)
end;;

module Light = struct
  type intensity = float
  type vector = vector3D
  type point = point3D
  type light = {lightFactor:(vector -> point -> intensity);
                vectorFromPointToLight:(point -> vector option);}

  let createLight (type a) =
    function (module Light :
              LIGHT_BASE with type light = a) ->
      function l -> {lightFactor = (Light.lightFactor l);
                     vectorFromPointToLight = (Light.vectorFromPointToLight l);};;

  let lightFactor light vector = light.lightFactor vector;;
  let vectorFromPointToLight light point = light.vectorFromPointToLight point;;
end;;


type lightEnvironmentConstrArgs = {intensity:float};;
module Light_Environment
         :(LIGHT_BASE with type light = lightEnvironmentConstrArgs)
  = struct
  type intensity = float
  type vector = Structures.vector3D
  type point = point3D
  type light = lightEnvironmentConstrArgs

  let createLight light = light;;
  let lightFactor light _ _ = light.intensity;;
  let vectorFromPointToLight _ _ = None;;
end;;

type lightDirectionConstrArgs = {intensity:float; direction:vector3D};;
module Light_Direction
         :(LIGHT_BASE with type light = lightDirectionConstrArgs)
  = struct
  type intensity = float
  type vector = vector3D
  type point = point3D
  type light = lightDirectionConstrArgs

  let createLight light = light;;
  let lightFactor light vec _  =
    (* let a = (normalize v1) -.- (normalize v2) in
     * if a > 0. then a else 0.;; *)
    (* (acos((normalize v1) -.- (normalize v2)) /. 2.) /. _PI;; *)
    (((normalize light.direction) -.- (normalize vec)) -. 1.) /. ~-.2. *. light.intensity;;
  let vectorFromPointToLight light _ = Some (light.direction);;
end;;

type lightPointConstrArgs = {intensity:float; pos:vector3D; factor:float};;
module Light_Point
         :(LIGHT_BASE with type light = lightPointConstrArgs)
  = struct
  type vector = vector3D 
  type intensity = float
  type point = point3D
  type light = lightPointConstrArgs

  let createLight light = light;;
  let lightFactor light vec point =
    (light.intensity /. (point ||-|| light.pos)) ** light.factor;;
  let vectorFromPointToLight light point = Some (point --- light.pos);;
end;;

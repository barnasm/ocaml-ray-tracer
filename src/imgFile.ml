let name = "./render/img.bmp"
and width  = 256
and height = 256;;

let image = Rgb24.make width height {Color.Rgb.r = 0; g=0; b=0; };;
                    
let save ?(name = name) () =
  Bmp.save name [] (Images.Rgb24 image);;      

let setPxColor x y col =
  Rgb24.set image x y col;;

let dump_to_file mx =
for i = 0 to height-1 do
  for j = 0 to width - 1 do
    let col = mx.(i).(j) in
    Rgb24.set image j i {Color.Rgb.r = (col lsr 16) land 255; g= (col lsr 8) land 255; b=col land (255); };
  done;
done;
save();;


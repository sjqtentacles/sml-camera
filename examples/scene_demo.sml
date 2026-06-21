(* sml-camera demo: places a ground grid and several wireframe cubes in world
   space, then projects everything through Scene.viewProjection (a perspective
   camera) and draws it with sml-raster -> assets/scene.png. *)

open Scene.Glm

fun rgba (r, g, b, a) : Image.rgba8 =
  { r = Word8.fromInt r, g = Word8.fromInt g
  , b = Word8.fromInt b, a = Word8.fromInt a }

val width = 640
val height = 460

val cam : Scene.camera =
  { eye = Vec3.v (4.5, 3.2, 6.0)
  , center = Vec3.v (0.0, 0.4, 0.0)
  , up = Vec3.v (0.0, 1.0, 0.0)
  , fovy = radians 50.0
  , aspect = real width / real height
  , near = 0.1, far = 100.0 }

val vp = Scene.viewProjection cam

(* world point -> SOME (screen x, y) when in front of the camera. *)
fun project (x, y, z) =
  let
    val c = Mat4.mulV (vp, Vec4.v (x, y, z, 1.0))
    val w = Vec4.w c
  in
    if w <= 0.001 then NONE
    else
      let
        val nx = Vec4.x c / w
        val ny = Vec4.y c / w
      in
        SOME ( Real.round ((nx * 0.5 + 0.5) * real (width - 1))
             , Real.round ((1.0 - (ny * 0.5 + 0.5)) * real (height - 1)) )
      end
  end

fun line3 img (p0, p1) color =
  case (project p0, project p1) of
      (SOME (x0, y0), SOME (x1, y1)) =>
        Raster.line img { x0 = x0, y0 = y0, x1 = x1, y1 = y1 } color
    | _ => img

(* ground grid on the y = 0 plane *)
val gridCol = rgba (54, 62, 78, 255)
val axisCol = rgba (90, 104, 130, 255)
fun ground img =
  let
    val n = 5
    fun lines (k, img) =
      if k > n then img
      else
        let
          val t = real k
          val col = if k = 0 then axisCol else gridCol
          val img = line3 img ((~(real n), 0.0, t), (real n, 0.0, t)) col
          val img = line3 img ((~(real n), 0.0, ~t), (real n, 0.0, ~t)) col
          val img = line3 img ((t, 0.0, ~(real n)), (t, 0.0, real n)) col
          val img = line3 img ((~t, 0.0, ~(real n)), (~t, 0.0, real n)) col
        in
          lines (k + 1, img)
        end
  in
    lines (0, img)
  end

(* a wireframe cube centered at (cx,cy,cz), half-extent h *)
val cubeEdges =
  [ (0,1),(1,3),(3,2),(2,0)      (* bottom *)
  , (4,5),(5,7),(7,6),(6,4)      (* top *)
  , (0,4),(1,5),(2,6),(3,7) ]    (* verticals *)

fun corner (cx, cy, cz, h) k =
  let
    val sx = if k mod 2 = 0 then ~h else h
    val sy = if (k div 4) mod 2 = 0 then ~h else h
    val sz = if (k div 2) mod 2 = 0 then ~h else h
  in
    (cx + sx, cy + sy, cz + sz)
  end

fun cube img (cx, cz, h, color) =
  let
    val cy = h  (* sit on the ground *)
    fun draw ((i, j), img) =
      line3 img (corner (cx, cy, cz, h) i, corner (cx, cy, cz, h) j) color
  in
    List.foldl draw img cubeEdges
  end

val cubes =
  [ (0.0,  0.0, 0.9, rgba (120, 210, 235, 255))
  , (~2.6, ~1.4, 0.6, rgba (240, 190, 96, 255))
  , (2.7,  1.6, 0.7, rgba (130, 220, 150, 255))
  , (~1.4, 2.6, 0.5, rgba (235, 120, 140, 255))
  , (2.2, ~2.4, 0.55, rgba (180, 150, 240, 255)) ]

val img =
  let
    val base = Raster.blank (width, height) (rgba (20, 23, 30, 255))
    val withGrid = ground base
  in
    List.foldl (fn ((cx, cz, h, col), img) => cube img (cx, cz, h, col)) withGrid cubes
  end

val () =
  let
    val os = BinIO.openOut "assets/scene.png"
  in
    BinIO.output (os, Image.encodePng img);
    BinIO.closeOut os;
    print "wrote assets/scene.png\n"
  end

(* scene.sml

   Implementation of the SCENE signature on top of the vendored sml-glm.
   Pure Basis; deterministic across MLton and Poly/ML. *)

structure Scene :> SCENE =
struct
  structure Glm = Glm

  structure V3 = Glm.Vec3
  structure V4 = Glm.Vec4
  structure M4 = Glm.Mat4
  structure Q = Glm.Quat

  type transform =
    { position : V3.t, rotation : Q.t, scale : V3.t }

  val identity =
    { position = V3.zero, rotation = Q.id, scale = V3.v (1.0, 1.0, 1.0) }

  fun translation p = { position = p, rotation = Q.id, scale = V3.v (1.0, 1.0, 1.0) }

  fun fromTRS (p, r, s) = { position = p, rotation = r, scale = s }

  fun toMat4 ({ position, rotation, scale } : transform) =
    let
      val t = M4.translate position
      val r = Q.toMat4 rotation
      val s = M4.scaleM scale
    in M4.mul (M4.mul (t, r), s) end

  fun compose (parent, child) = M4.mul (toMat4 parent, toMat4 child)

  fun apply (tf, p) = M4.transformPoint (toMat4 tf, p)

  (* ---------- camera ---------- *)

  type camera =
    { eye : V3.t, center : V3.t, up : V3.t,
      fovy : real, aspect : real, near : real, far : real }

  fun view (c : camera) =
    M4.lookAt { eye = #eye c, center = #center c, up = #up c }

  fun projection (c : camera) =
    M4.perspective { fovy = #fovy c, aspect = #aspect c, near = #near c, far = #far c }

  fun viewProjection c = M4.mul (projection c, view c)

  (* ---------- volumes & ray ---------- *)

  type aabb   = { min : V3.t, max : V3.t }
  type sphere = { center : V3.t, radius : real }
  type ray    = { origin : V3.t, dir : V3.t }

  type plane   = { normal : V3.t, d : real }
  type frustum = plane list

  fun normalizePlane { normal, d } =
    let val len = V3.length normal in
      if Real.== (len, 0.0) then { normal = normal, d = d }
      else { normal = V3.scale (1.0 / len, normal), d = d / len }
    end

  (* Extract planes from a row-major view-projection. sml-glm stores matrices
     column-major; toList gives column-major order. We index by (row,col).

     The matrix entries are read on demand through `e` rather than destructured
     into sixteen simultaneously-live `val` bindings; holding all sixteen reals
     live at once overflows Poly/ML's native-codegen FP-register budget
     ("asFPReg raised while compiling"). *)
  fun frustumOf m =
    let
      val xs = Vector.fromList (M4.toList m)
      (* column-major: element (row r, col c) is at index c*4 + r *)
      fun e (r, c) = Vector.sub (xs, c * 4 + r)
      fun pl (a, b, c, d) = normalizePlane { normal = V3.v (a, b, c), d = d }
      (* row r +/- row k, component by component *)
      fun comb (sgn, k, r) (c) = e (k, c) + sgn * e (r, c)
      fun plane (sgn, k, r) =
        pl (comb (sgn, k, r) 0, comb (sgn, k, r) 1,
            comb (sgn, k, r) 2, comb (sgn, k, r) 3)
    in
      [ plane ( 1.0, 3, 0),   (* left   : row3 + row0 *)
        plane (~1.0, 3, 0),   (* right  : row3 - row0 *)
        plane ( 1.0, 3, 1),   (* bottom : row3 + row1 *)
        plane (~1.0, 3, 1),   (* top    : row3 - row1 *)
        plane ( 1.0, 3, 2),   (* near   : row3 + row2 *)
        plane (~1.0, 3, 2) ]  (* far    : row3 - row2 *)
    end

  fun signedDist ({ normal, d }, p) = V3.dot (normal, p) + d

  fun containsPoint (fr, p) =
    List.all (fn pl => signedDist (pl, p) >= 0.0) fr

  fun intersectsSphere (fr, { center, radius } : sphere) =
    List.all (fn pl => signedDist (pl, center) >= ~radius) fr

  fun intersectsAabb (fr, { min, max } : aabb) =
    let
      (* for each plane, test the positive vertex (farthest along the normal) *)
      fun cull pl =
        let
          val n = #normal pl
          fun pick (lo, hi, c) = if c >= 0.0 then hi else lo
          val px = pick (V3.x min, V3.x max, V3.x n)
          val py = pick (V3.y min, V3.y max, V3.y n)
          val pz = pick (V3.z min, V3.z max, V3.z n)
        in signedDist (pl, V3.v (px, py, pz)) < 0.0 end
    in not (List.exists cull fr) end

  (* ---------- intersections ---------- *)

  val eps = 1e~9

  (* Core slab test on raw scalar components, kept as its own top-level
     function (not inlined into rayAabb). Operating on plain reals rather than
     repeatedly-inlined V3 accessors keeps Poly/ML's native codegen within its
     register budget ("asGenReg raised while compiling") on older x86-64. *)
  fun slabHit (ox,oy,oz, dx,dy,dz, lox,loy,loz, hix,hiy,hiz) =
    let
      fun slab (org, d, lo, hi, acc) =
        case acc of
          NONE => NONE
        | SOME (tmin, tmax) =>
            if Real.abs d < eps then
              (if org < lo orelse org > hi then NONE else SOME (tmin, tmax))
            else
              let
                val inv = 1.0 / d
                val t1 = (lo - org) * inv
                val t2 = (hi - org) * inv
                val tn = if t1 <= t2 then t1 else t2
                val tf = if t1 <= t2 then t2 else t1
                val tmin' = Real.max (tmin, tn)
                val tmax' = Real.min (tmax, tf)
              in if tmin' > tmax' then NONE else SOME (tmin', tmax') end
      val r0 = SOME (~1e308, 1e308)
      val r1 = slab (ox, dx, lox, hix, r0)
      val r2 = slab (oy, dy, loy, hiy, r1)
      val r3 = slab (oz, dz, loz, hiz, r2)
    in
      case r3 of
        NONE => NONE
      | SOME (tmin, tmax) =>
          if tmax < 0.0 then NONE              (* box entirely behind *)
          else if tmin >= 0.0 then SOME tmin   (* first hit ahead *)
          else SOME 0.0                        (* origin inside box *)
    end

  fun rayAabb ({ origin, dir } : ray, { min, max } : aabb) =
    slabHit (V3.x origin, V3.y origin, V3.z origin,
             V3.x dir, V3.y dir, V3.z dir,
             V3.x min, V3.y min, V3.z min,
             V3.x max, V3.y max, V3.z max)

  fun raySphere ({ origin, dir } : ray, { center, radius } : sphere) =
    let
      val oc = V3.sub (origin, center)
      val a = V3.dot (dir, dir)
      val b = 2.0 * V3.dot (oc, dir)
      val c = V3.dot (oc, oc) - radius * radius
      val disc = b * b - 4.0 * a * c
    in
      if disc < 0.0 orelse Real.abs a < eps then NONE
      else
        let
          val sq = Math.sqrt disc
          val t0 = (~b - sq) / (2.0 * a)
          val t1 = (~b + sq) / (2.0 * a)
          val (tn, tf) = if t0 <= t1 then (t0, t1) else (t1, t0)
        in
          if tf < 0.0 then NONE
          else if tn >= 0.0 then SOME tn
          else SOME 0.0   (* origin inside sphere *)
        end
    end

  (* Core Moller-Trumbore. All intermediates live in a mutable array rather
     than as simultaneously-live `val` bindings, so Poly/ML's native codegen
     never has to hold a dozen reals in FP registers at once ("asFPReg raised
     while compiling"). *)
  fun mtIntersect (ox,oy,oz, dx,dy,dz,
                   ax,ay,az, bx,by,bz, cx,cy,cz) =
    let
      val w = Array.array (12, 0.0)
      fun st (i, v) = Array.update (w, i, v)
      fun ld i = Array.sub (w, i)
      (* 0..2 = e1, 3..5 = e2 *)
      val () = st (0, bx-ax)  val () = st (1, by-ay)  val () = st (2, bz-az)
      val () = st (3, cx-ax)  val () = st (4, cy-ay)  val () = st (5, cz-az)
      (* 6..8 = p = dir x e2 *)
      val () = st (6, dy*ld 5 - dz*ld 4)
      val () = st (7, dz*ld 3 - dx*ld 5)
      val () = st (8, dx*ld 4 - dy*ld 3)
      val det = ld 0 * ld 6 + ld 1 * ld 7 + ld 2 * ld 8
    in
      if Real.abs det < eps then NONE
      else
        let
          val invDet = 1.0 / det
          (* 9..11 = tvec = origin - a *)
          val () = st (9, ox-ax)  val () = st (10, oy-ay)  val () = st (11, oz-az)
          val u = (ld 9 * ld 6 + ld 10 * ld 7 + ld 11 * ld 8) * invDet
        in
          if u < 0.0 orelse u > 1.0 then NONE
          else
            let
              (* reuse 6..8 for q = tvec x e1 *)
              val q0 = ld 10 * ld 2 - ld 11 * ld 1
              val q1 = ld 11 * ld 0 - ld 9 * ld 2
              val q2 = ld 9 * ld 1 - ld 10 * ld 0
              val () = st (6, q0)  val () = st (7, q1)  val () = st (8, q2)
              val vv = (dx*ld 6 + dy*ld 7 + dz*ld 8) * invDet
            in
              if vv < 0.0 orelse u + vv > 1.0 then NONE
              else
                let val tt = (ld 3 * ld 6 + ld 4 * ld 7 + ld 5 * ld 8) * invDet
                in if tt >= 0.0 then SOME tt else NONE end
            end
        end
    end

  (* Moller-Trumbore; culls degenerate (zero-area) triangles, hits either face. *)
  fun rayTriangle ({ origin, dir } : ray, (v0, v1, v2)) =
    mtIntersect (V3.x origin, V3.y origin, V3.z origin,
                 V3.x dir, V3.y dir, V3.z dir,
                 V3.x v0, V3.y v0, V3.z v0,
                 V3.x v1, V3.y v1, V3.z v1,
                 V3.x v2, V3.y v2, V3.z v2)

  fun aabbAabb (a : aabb, b : aabb) =
    V3.x (#min a) <= V3.x (#max b) andalso V3.x (#max a) >= V3.x (#min b)
    andalso V3.y (#min a) <= V3.y (#max b) andalso V3.y (#max a) >= V3.y (#min b)
    andalso V3.z (#min a) <= V3.z (#max b) andalso V3.z (#max a) >= V3.z (#min b)
end

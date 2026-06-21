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
     column-major; toList gives column-major order. We index by (row,col). *)
  fun frustumOf m =
    let
      val xs = Vector.fromList (M4.toList m)
      (* column-major: element (row r, col c) is at index c*4 + r *)
      fun e (r, c) = Vector.sub (xs, c * 4 + r)
      fun row r = (e (r,0), e (r,1), e (r,2), e (r,3))
      val (m00,m01,m02,m03) = row 0
      val (m10,m11,m12,m13) = row 1
      val (m20,m21,m22,m23) = row 2
      val (m30,m31,m32,m33) = row 3
      fun pl (a, b, c, d) = normalizePlane { normal = V3.v (a, b, c), d = d }
    in
      [ pl (m30+m00, m31+m01, m32+m02, m33+m03),   (* left   *)
        pl (m30-m00, m31-m01, m32-m02, m33-m03),   (* right  *)
        pl (m30+m10, m31+m11, m32+m12, m33+m13),   (* bottom *)
        pl (m30-m10, m31-m11, m32-m12, m33-m13),   (* top    *)
        pl (m30+m20, m31+m21, m32+m22, m33+m23),   (* near   *)
        pl (m30-m20, m31-m21, m32-m22, m33-m23) ]  (* far    *)
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

  fun rayAabb ({ origin, dir } : ray, { min, max } : aabb) =
    let
      (* slab method; handle dir component ~0 (parallel) without dividing. *)
      fun slab (org, d, lo, hi, (tmin, tmax)) =
        if Real.abs d < eps then
          (if org < lo orelse org > hi then NONE else SOME (tmin, tmax))
        else
          let
            val inv = 1.0 / d
            val t1 = (lo - org) * inv
            val t2 = (hi - org) * inv
            val (tn, tf) = if t1 <= t2 then (t1, t2) else (t2, t1)
            val tmin' = Real.max (tmin, tn)
            val tmax' = Real.min (tmax, tf)
          in if tmin' > tmax' then NONE else SOME (tmin', tmax') end
      val r0 = SOME (~1e308, 1e308)
      val r1 = case r0 of SOME acc => slab (V3.x origin, V3.x dir, V3.x min, V3.x max, acc) | NONE => NONE
      val r2 = case r1 of SOME acc => slab (V3.y origin, V3.y dir, V3.y min, V3.y max, acc) | NONE => NONE
      val r3 = case r2 of SOME acc => slab (V3.z origin, V3.z dir, V3.z min, V3.z max, acc) | NONE => NONE
    in
      case r3 of
        NONE => NONE
      | SOME (tmin, tmax) =>
          if tmax < 0.0 then NONE              (* box entirely behind *)
          else if tmin >= 0.0 then SOME tmin   (* first hit ahead *)
          else SOME 0.0                        (* origin inside box *)
    end

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

  (* Moller-Trumbore; culls degenerate (zero-area) triangles, hits either face. *)
  fun rayTriangle ({ origin, dir } : ray, (v0, v1, v2)) =
    let
      val e1 = V3.sub (v1, v0)
      val e2 = V3.sub (v2, v0)
      val p = V3.cross (dir, e2)
      val det = V3.dot (e1, p)
    in
      if Real.abs det < eps then NONE   (* parallel or degenerate *)
      else
        let
          val invDet = 1.0 / det
          val tvec = V3.sub (origin, v0)
          val u = V3.dot (tvec, p) * invDet
        in
          if u < 0.0 orelse u > 1.0 then NONE
          else
            let
              val q = V3.cross (tvec, e1)
              val vv = V3.dot (dir, q) * invDet
            in
              if vv < 0.0 orelse u + vv > 1.0 then NONE
              else
                let val t = V3.dot (e2, q) * invDet
                in if t >= 0.0 then SOME t else NONE end
            end
        end
    end

  fun aabbAabb (a : aabb, b : aabb) =
    V3.x (#min a) <= V3.x (#max b) andalso V3.x (#max a) >= V3.x (#min b)
    andalso V3.y (#min a) <= V3.y (#max b) andalso V3.y (#max a) >= V3.y (#min b)
    andalso V3.z (#min a) <= V3.z (#max b) andalso V3.z (#max a) >= V3.z (#min b)
end

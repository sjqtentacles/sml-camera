(* support.sml -- shared helpers for scene tests. *)

structure Support =
struct
  structure S = Scene
  structure V3 = Scene.Glm.Vec3
  structure M4 = Scene.Glm.Mat4

  val eps = 1e~5

  fun v3 (x, y, z) = V3.v (x, y, z)

  fun rApprox (a, b) = Real.abs (a - b) < eps

  fun checkMat name (expected, actual) =
    Harness.check name (M4.approx eps (expected, actual))

  fun checkV3 name (ex, ey, ez) v =
    Harness.check name
      (rApprox (V3.x v, ex) andalso rApprox (V3.y v, ey) andalso rApprox (V3.z v, ez))

  fun checkSome name f =
    case f of SOME _ => Harness.check name true | NONE => Harness.check name false
  fun checkNone name f =
    case f of NONE => Harness.check name true | SOME _ => Harness.check name false

  fun checkHit name expectedT f =
    case f of
      SOME t => Harness.check name (rApprox (t, expectedT))
    | NONE => Harness.check name false
end

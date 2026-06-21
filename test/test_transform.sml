(* test_transform.sml -- TRS transforms compose like the matrix product. *)

structure TransformTests =
struct
  structure S = Scene
  structure Q = Scene.Glm.Quat
  open Support

  fun run () =
    let
      val _ = Harness.section "transform -> matrix"
      val () = checkMat "identity transform is Mat4.id"
                 (M4.id, S.toMat4 S.identity)
      val () = checkMat "translate-only matches Mat4.translate"
                 (M4.translate (v3 (1.0, 2.0, 3.0)),
                  S.toMat4 (S.translation (v3 (1.0, 2.0, 3.0))))
      val () = checkMat "scale-only matches Mat4.scaleM"
                 (M4.scaleM (v3 (2.0, 3.0, 4.0)),
                  S.toMat4 (S.fromTRS (v3 (0.0,0.0,0.0), Q.id, v3 (2.0,3.0,4.0))))
      val rot = Q.fromAxisAngle (v3 (0.0, 0.0, 1.0), Scene.Glm.radians 90.0)
      val () = checkMat "rotate-only matches Quat.toMat4"
                 (Q.toMat4 rot,
                  S.toMat4 (S.fromTRS (v3 (0.0,0.0,0.0), rot, v3 (1.0,1.0,1.0))))

      val _ = Harness.section "composition & application"
      val parent = S.translation (v3 (10.0, 0.0, 0.0))
      val child  = S.translation (v3 (0.0, 5.0, 0.0))
      val () = checkMat "compose parent*child = matrix product"
                 (M4.mul (S.toMat4 parent, S.toMat4 child), S.compose (parent, child))
      val () = checkV3 "apply translate to a point"
                 (11.0, 2.0, 3.0)
                 (S.apply (S.translation (v3 (10.0, 0.0, 0.0)), v3 (1.0, 2.0, 3.0)))

      val _ = Harness.section "round-trip through transform & inverse"
      val tf = S.fromTRS (v3 (3.0, ~2.0, 1.0), rot, v3 (2.0, 2.0, 2.0))
      val m = S.toMat4 tf
      val p = v3 (1.0, 2.0, 3.0)
      val p' = M4.transformPoint (m, p)
      val () =
        case M4.inverse m of
          SOME minv => checkV3 "inverse(transform(p)) = p"
                         (1.0, 2.0, 3.0) (M4.transformPoint (minv, p'))
        | NONE => Harness.check "inverse(transform(p)) = p" false
    in () end
end

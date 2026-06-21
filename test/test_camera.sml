(* test_camera.sml -- view/projection match the sml-glm builders. *)

structure CameraTests =
struct
  structure S = Scene
  open Support

  val cam =
    { eye = v3 (0.0, 0.0, 0.0), center = v3 (0.0, 0.0, ~1.0), up = v3 (0.0, 1.0, 0.0),
      fovy = Scene.Glm.radians 60.0, aspect = 16.0 / 9.0, near = 0.1, far = 100.0 }

  fun run () =
    let
      val _ = Harness.section "camera matrices"
      val () = checkMat "view = lookAt"
                 (M4.lookAt { eye = #eye cam, center = #center cam, up = #up cam },
                  S.view cam)
      val () = checkMat "projection = perspective"
                 (M4.perspective { fovy = #fovy cam, aspect = #aspect cam,
                                   near = #near cam, far = #far cam },
                  S.projection cam)
      val () = checkMat "viewProjection = proj * view"
                 (M4.mul (S.projection cam, S.view cam), S.viewProjection cam)

      val _ = Harness.section "a point in front lands inside the frustum"
      val fr = S.frustumOf (S.viewProjection cam)
      val () = Harness.check "point straight ahead is visible"
                 (S.containsPoint (fr, v3 (0.0, 0.0, ~5.0)))
      val () = Harness.check "point behind the eye is not visible"
                 (not (S.containsPoint (fr, v3 (0.0, 0.0, 5.0))))
    in () end
end

(* test_frustum.sml -- inside is kept, clearly-outside is culled, straddling kept. *)

structure FrustumTests =
struct
  structure S = Scene
  open Support

  val cam =
    { eye = v3 (0.0, 0.0, 0.0), center = v3 (0.0, 0.0, ~1.0), up = v3 (0.0, 1.0, 0.0),
      fovy = Scene.Glm.radians 90.0, aspect = 1.0, near = 0.1, far = 100.0 }
  val fr = S.frustumOf (S.viewProjection cam)

  fun run () =
    let
      val _ = Harness.section "point culling"
      val () = Harness.check "inside" (S.containsPoint (fr, v3 (0.0, 0.0, ~10.0)))
      val () = Harness.check "behind near plane culled"
                 (not (S.containsPoint (fr, v3 (0.0, 0.0, 1.0))))
      val () = Harness.check "beyond far plane culled"
                 (not (S.containsPoint (fr, v3 (0.0, 0.0, ~200.0))))
      val () = Harness.check "far off to the side culled"
                 (not (S.containsPoint (fr, v3 (1000.0, 0.0, ~10.0))))

      val _ = Harness.section "sphere culling"
      val () = Harness.check "sphere inside kept"
                 (S.intersectsSphere (fr, { center = v3 (0.0,0.0,~10.0), radius = 1.0 }))
      val () = Harness.check "sphere far behind culled"
                 (not (S.intersectsSphere (fr, { center = v3 (0.0,0.0,50.0), radius = 1.0 })))
      val () = Harness.check "sphere straddling near plane kept (conservative)"
                 (S.intersectsSphere (fr, { center = v3 (0.0,0.0,0.0), radius = 1.0 }))

      val _ = Harness.section "AABB culling"
      val () = Harness.check "AABB inside kept"
                 (S.intersectsAabb (fr, { min = v3 (~1.0,~1.0,~11.0), max = v3 (1.0,1.0,~9.0) }))
      val () = Harness.check "AABB far to the side culled"
                 (not (S.intersectsAabb (fr, { min = v3 (1000.0,1000.0,~11.0),
                                               max = v3 (1001.0,1001.0,~9.0) })))
      val () = Harness.check "AABB straddling the frustum kept"
                 (S.intersectsAabb (fr, { min = v3 (~1.0,~1.0,~1.0), max = v3 (1.0,1.0,1.0) }))
    in () end
end

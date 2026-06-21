(* test_intersect.sml -- ray/AABB/sphere/triangle hits and AABB overlap. *)

structure IntersectTests =
struct
  structure S = Scene
  open Support

  val down = { origin = v3 (0.0, 0.0, 0.0), dir = v3 (0.0, 0.0, ~1.0) }
  val away = { origin = v3 (0.0, 0.0, 0.0), dir = v3 (0.0, 0.0, 1.0) }

  fun run () =
    let
      val _ = Harness.section "ray / AABB"
      val box = { min = v3 (~1.0,~1.0,~5.0), max = v3 (1.0,1.0,~3.0) }
      val () = checkHit "head-on hit at the near face" 3.0 (S.rayAabb (down, box))
      val () = checkNone "ray pointing away misses" (S.rayAabb (away, box))
      val () = checkHit "origin inside box -> t=0" 0.0
                 (S.rayAabb ({ origin = v3 (0.0,0.0,~4.0), dir = v3 (0.0,0.0,~1.0) }, box))
      (* ray parallel to a slab (x) but inside its extent still hits *)
      val () = checkSome "ray parallel to a slab does not divide-by-zero"
                 (S.rayAabb ({ origin = v3 (0.0, 0.0, 0.0), dir = v3 (0.0, 0.0, ~1.0) }, box))

      val _ = Harness.section "ray / sphere"
      val sph = { center = v3 (0.0,0.0,~5.0), radius = 1.0 }
      val () = checkHit "head-on hit at the near surface" 4.0 (S.raySphere (down, sph))
      val () = checkNone "ray pointing away misses" (S.raySphere (away, sph))
      val () = checkHit "origin inside sphere -> t=0" 0.0
                 (S.raySphere ({ origin = v3 (0.0,0.0,~5.0), dir = v3 (0.0,0.0,~1.0) }, sph))
      (* tangent ray grazes the sphere: just touches at x = radius *)
      val () = checkSome "tangent ray (grazing) reports a hit"
                 (S.raySphere ({ origin = v3 (1.0, 0.0, 0.0), dir = v3 (0.0, 0.0, ~1.0) }, sph))

      val _ = Harness.section "ray / triangle"
      val tri = (v3 (~1.0,~1.0,~5.0), v3 (1.0,~1.0,~5.0), v3 (0.0,1.0,~5.0))
      val () = checkHit "head-on hit through the centroid" 5.0 (S.rayTriangle (down, tri))
      val () = checkNone "ray pointing away misses" (S.rayTriangle (away, tri))
      val () = checkNone "ray outside the triangle misses"
                 (S.rayTriangle ({ origin = v3 (10.0,10.0,0.0), dir = v3 (0.0,0.0,~1.0) }, tri))
      val degenerate = (v3 (0.0,0.0,~5.0), v3 (0.0,0.0,~5.0), v3 (0.0,0.0,~5.0))
      val () = checkNone "degenerate (zero-area) triangle never hits"
                 (S.rayTriangle (down, degenerate))

      val _ = Harness.section "AABB / AABB"
      val a = { min = v3 (0.0,0.0,0.0), max = v3 (2.0,2.0,2.0) }
      val () = Harness.check "overlapping" (S.aabbAabb (a, { min = v3 (1.0,1.0,1.0), max = v3 (3.0,3.0,3.0) }))
      val () = Harness.check "touching counts as overlap"
                 (S.aabbAabb (a, { min = v3 (2.0,0.0,0.0), max = v3 (4.0,2.0,2.0) }))
      val () = Harness.check "disjoint"
                 (not (S.aabbAabb (a, { min = v3 (3.0,3.0,3.0), max = v3 (4.0,4.0,4.0) })))
    in () end
end

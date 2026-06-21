(* entry.sml -- runs every suite, prints the summary, exits with status. *)

fun runAllSuites () =
  ( Harness.reset ()
  ; TransformTests.run ()
  ; CameraTests.run ()
  ; FrustumTests.run ()
  ; IntersectTests.run ()
  ; Harness.run () )

fun main () =
  OS.Process.exit
    (if runAllSuites () then OS.Process.success else OS.Process.failure)

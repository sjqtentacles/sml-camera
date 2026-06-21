(* scene.sig

   Scene math built on sml-glm: transforms (TRS) with matrix composition, a
   camera (view + projection helpers), frustum extraction and culling, and ray
   / volume intersection primitives.

   Right-handed coordinates; GL clip space (z in [-1, 1]); angles in radians.
   All operations are pure and total. Deterministic and byte-identical across
   MLton and Poly/ML. *)

signature SCENE =
sig
  structure Glm : GLM

  (* ---------- transforms (translation / rotation / scale) ---------- *)

  type transform =
    { position : Glm.Vec3.t,
      rotation : Glm.Quat.t,
      scale    : Glm.Vec3.t }

  val identity  : transform
  val translation : Glm.Vec3.t -> transform
  val fromTRS   : Glm.Vec3.t * Glm.Quat.t * Glm.Vec3.t -> transform

  (* Local-to-world matrix (T * R * S). *)
  val toMat4    : transform -> Glm.Mat4.t
  (* Compose parent * child (apply child in parent's space). *)
  val compose   : transform * transform -> Glm.Mat4.t
  (* Transform a point through the transform's matrix. *)
  val apply     : transform * Glm.Vec3.t -> Glm.Vec3.t

  (* ---------- camera ---------- *)

  type camera =
    { eye : Glm.Vec3.t, center : Glm.Vec3.t, up : Glm.Vec3.t,
      fovy : real, aspect : real, near : real, far : real }

  val view        : camera -> Glm.Mat4.t
  val projection  : camera -> Glm.Mat4.t
  val viewProjection : camera -> Glm.Mat4.t

  (* ---------- bounding volumes & ray ---------- *)

  type aabb   = { min : Glm.Vec3.t, max : Glm.Vec3.t }
  type sphere = { center : Glm.Vec3.t, radius : real }
  type ray    = { origin : Glm.Vec3.t, dir : Glm.Vec3.t }   (* dir need not be unit *)

  (* ---------- frustum culling ---------- *)

  type plane   = { normal : Glm.Vec3.t, d : real }   (* normal.p + d = 0 *)
  type frustum = plane list                          (* 6 inward-facing planes *)

  (* Extract the 6 frustum planes from a view-projection matrix. *)
  val frustumOf  : Glm.Mat4.t -> frustum
  (* Conservative tests: a volume straddling a plane is kept (true). *)
  val containsPoint  : frustum * Glm.Vec3.t -> bool
  val intersectsSphere : frustum * sphere -> bool
  val intersectsAabb   : frustum * aabb -> bool

  (* ---------- intersection primitives ---------- *)

  (* Each returns SOME t (ray parameter of first hit, t >= 0) or NONE. *)
  val rayAabb     : ray * aabb -> real option
  val raySphere   : ray * sphere -> real option
  val rayTriangle : ray * (Glm.Vec3.t * Glm.Vec3.t * Glm.Vec3.t) -> real option

  (* AABB overlap: true if they intersect (touching counts as overlap). *)
  val aabbAabb    : aabb * aabb -> bool
end

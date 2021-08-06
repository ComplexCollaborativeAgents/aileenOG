(in-package :aileen)


;;; positive example
(defparameter *near-pos* 'd::( 

  (isa ObjNearPosA CVCube)
  (isa ObjNearPosA CVGreen)

  (isa ObjNearPosB CVCylinder)
  (isa ObjNearPosB CVGreen)

  (distanceBetween ObjNearPosA ObjNearPosB 4.0)

  (dc ObjNearPosA ObjNearPosB)
  (e ObjNearPosA ObjNearPosB)

  ))


;;; negative nearness test case
(defparameter *near-neg* 'd::( 

  (isa ObjNearNegA CVCube)
  (isa ObjNearNegA CVYellow)

  (isa ObjNearNegB CVCylinder)
  (isa ObjNearNegB CVGreen)

  (distanceBetween ObjNearNegA ObjNearNegB 99999.0)

  (dc ObjNearNegA ObjNearNegB)
  (s ObjNearNegA ObjNearNegB)

  ))


(defparameter *left-pos* 'd::( 

  (isa ObjLeftPosA CVCube)
  (isa ObjLeftPosA CVGreen)

  (isa ObjLeftPosB CVCylinder)
  (isa ObjLeftPosB CVGreen)

  (distanceBetween ObjLeftPosA ObjLeftPosB 9999.0)

  (dc ObjLeftPosA ObjLeftPosB)
  (w ObjLeftPosA ObjLeftPosB)

  ))


(defparameter *left-neg* 'd::( 

  (isa ObjLeftNegA CVCube)
  (isa ObjLeftNegA CVYellow)

  (isa ObjLeftNegB CVCylinder)
  (isa ObjLeftNegB CVGreen)

  (distanceBetween ObjLeftNegA ObjLeftNegB 7.0)

  (dc ObjLeftNegA ObjLeftNegB)
  (s ObjLeftNegA ObjLeftNegB)

  ))


(defparameter *mixed-pos* 'd::( 

  (isa ObjNearPosA CVCube)
  (isa ObjNearPosA CVGreen)
  (sizeOf ObjNearPosA 9999999999.0)

  (isa ObjNearPosB CVCylinder)
  (isa ObjNearPosB CVGreen)

  (distanceBetween ObjNearPosA ObjNearPosB 4.0)

  (dc ObjNearPosA ObjNearPosB)
  (e ObjNearPosA ObjNearPosB)

  ))


;;; negative nearness test case
(defparameter *mixed-neg* 'd::( 

  (isa ObjNearNegA CVCube)
  (isa ObjNearNegA CVYellow)
  (sizeOf ObjNearNegA 5.0)

  (isa ObjNearNegB CVCylinder)
  (isa ObjNearNegB CVGreen)

  (distanceBetween ObjNearNegA ObjNearNegB 99999.0)

  (dc ObjNearNegA ObjNearNegB)
  (s ObjNearNegA ObjNearNegB)

  ))


;;; positive smallness case
(defparameter *small-pos* 'd::( 

  (isa ObjSmallPosA CVCube)
  (isa ObjSmallPosA CVGreen)
  (sizeOf ObjSmallPosA 1.5)

  ))


;;; negative smallness test case
(defparameter *small-neg* 'd::( 

  (isa ObjSmallNegA CVCube)
  (isa ObjSmallNegA CVYellow)
  (sizeOf ObjSmallNegA 9999.0)

  ))
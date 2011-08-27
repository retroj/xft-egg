
(import chicken scheme extras)

(use test
     xft)

;;;
;;; XRender
;;;

(test "xrendercolor scalable color values 1"
      .9
      (let ((c (make-xrendercolor .9 .8 .7)))
        (xrendercolor-red c)))

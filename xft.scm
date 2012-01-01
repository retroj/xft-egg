;; Copyright 2011 John J Foerch. All rights reserved.
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;; 
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY JOHN J FOERCH ''AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL JOHN J FOERCH OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module xft
        *

;;; MISC
;;;
;; xft-get-version

;;; COLOR
;;;
;; make-xftcolor xftcolor-red xftcolor-green xftcolor-blue xftcolor-alpha

;;; DRAW ON X DRAWABLE OR X RENDERING EXTENSION DRAWABLE
;;;
;; xft-draw-rect
;; xft-draw-glyphs
;; xft-draw-string*
;; xft-draw-char-spec
;; xft-draw-glyph-spec

;;; CONSTRUCT/POPULATE XFTFONT
;;;
;;; XftFont structures are internally allocated and freed.
;;;
;; xft-font-open
;; xft-font-open/name (for loading Xft-style font names)
;; xft-font-open/xlfd (Xft equiv of Xlib's XLoadQueryFont)
;; xft-font-open/info (open font based on an XftFontInfo object)
;; xft-font-open/pattern

;;; MEASURE TEXT METRICS
;;;
;; xft-glyph-extents
;; xft-text-extents*

;;; CHECK FOR GLYPHS IN FONTS
;;;
;; xft-font-check-glyph
;; xft-char-exists
;; xft-char-index

;;; DRAW ON X RENDERING EXTENSIONS PICTURE STRUCTURES
;;;
;; xft-glyph-render
;; xft-glyph-spec-render
;; xft-char-spec-render
;; xft-text-render*

;;; XFTFONTINFO
;;;
;;; An opaque object that store information about a font.  Internally
;;; allocated and freed.
;;;
;; xft-fontinfo-create
;; xft-fontinfo-destroy (normally not needed?)
;; xft-fontinfo-equal
;; xft-fontinfo-hash

;;; XFTDRAW
;;;
;;; Opaque object, wrapper around X drawable.  Allocated and freed
;;; internally.
;;;
;; xftdraw-create (create xftdraw, given X drawable)
;; xftdraw-create-bitmap (create xftdraw, given pixmap)
;; xftdraw-create-alpha (create xftdraw, given pixmap)
;; xftdraw-destroy (not normally needed)
;; xftdraw-display
;; xftdraw-drawable
;; xftdraw-colormap
;; xftdraw-visual
;; xftdraw-picture



(import chicken scheme foreign foreigners)

(use xtypes)

(foreign-declare "#include <X11/Xft/Xft.h>")

;;;
;;; Utils
;;;

(define (inexact->int n)
  (inexact->exact (round n)))


;;;
;;; FontConfig
;;;

(define-foreign-type fcchar8 unsigned-char)
(define-foreign-type fcchar16 unsigned-short)
(define-foreign-type fcchar32 unsigned-int)
(define-foreign-type fcbool int)

(define-foreign-enum-type (fcendian int)
  (fcendian->int int->fcendian)
  ((big) FcEndianBig)
  ((little) FcEndianLittle))

(define-foreign-type fccharset (c-pointer (struct _FcCharSet)))
(define-foreign-type fcpattern (c-pointer (struct _FcPattern)))

(define-foreign-enum-type (fcresult (enum _FcResult))
  (fcresult->int int->fcresult)
  ((match) FcResultMatch)
  ((nomatch) FcResultNoMatch)
  ((typemismatch) FcResultTypeMismatch)
  ((noid) FcResultNoId)
  ((outofmemory) FcResultOutOfMemory))

(define-foreign-record-type (fcfontset FcFontSet)
  (constructor: make-fcfontset)
  (destructor: free-fcfontset)
  (int nfont fcfontset-nfont)
  (int sfont fcfontset-sfont)
  ((c-pointer fcpattern) fonts fcfontset-fonts))

(define fcfontset-print
  (foreign-lambda void FcFontSetPrint
                  (const fcfontset)))

;;;
;;; FreeType
;;;

(define-foreign-type ft_face c-pointer)
(define-foreign-type ft_uint unsigned-int)


;;;
;;; Xlib
;;;

(define-foreign-type drawable unsigned-long)
(define-foreign-type xdisplay (c-pointer (struct _XDisplay)))
(define-foreign-type visual c-pointer)
(define-foreign-type colormap unsigned-long)
(define-foreign-type pixmap unsigned-long)
(define-foreign-type picture unsigned-long)
(define-foreign-type xrectangle c-pointer)
(define-foreign-type xregion (c-pointer (struct _XRegion)))
(define-foreign-type xglyphinfo c-pointer)
(define-foreign-type xrendercolor c-pointer)


;;;
;;; XftColor
;;;

(define-foreign-record-type (xftcolor XftColor)
  (constructor: %make-xftcolor)
  (destructor: %free-xftcolor)
  (unsigned-long pixel xftcolor-pixel)
  ((struct XRenderColor) color xftcolor-color))

(define %xftcolor-alloc/name
  (foreign-lambda bool XftColorAllocName
                  xdisplay          ;; dpy
                  (const visual)    ;; visual
                  colormap          ;; cmap
                  (const c-string)  ;; name
                  xftcolor))        ;; result

(define %xftcolor-alloc/value
  (foreign-lambda bool XftColorAllocValue
                  xdisplay              ;; dpy
                  visual                ;; visual
                  colormap              ;; cmap
                  (const xrendercolor)  ;; color
                  xftcolor))            ;; result

(define %xftcolor-free
  (foreign-lambda void XftColorFree
                  xdisplay     ;; dpy
                  visual       ;; visual
                  colormap     ;; cmap
                  xftcolor))   ;; color

(define make-xftcolor
  (case-lambda
   ((display visual colormap name)
    (let ((xftc (%make-xftcolor)))
      (%xftcolor-alloc/name display
                            visual
                            colormap
                            name
                            xftc)
      (set-finalizer! xftc
                      (lambda (x)
                        (%xftcolor-free display visual colormap x)
                        (%free-xftcolor x)))
      xftc))
   ((display visual colormap r g b a)
    (let ((xftc (%make-xftcolor))
          (c (make-xrendercolor r g b a)))
      (%xftcolor-alloc/value display
                             visual
                             colormap
                             c
                             xftc)
      (set-finalizer! xftc
                      (lambda (x)
                        (%xftcolor-free display visual colormap x)
                        (%free-xftcolor x)))
      xftc))
   ((display visual colormap r g b)
    (make-xftcolor display visual colormap r g b 1.0))))

(define (xftcolor-red x)
  (xrendercolor-red (xftcolor-color x)))

(define (xftcolor-green x)
  (xrendercolor-green (xftcolor-color x)))

(define (xftcolor-blue x)
  (xrendercolor-blue (xftcolor-color x)))

(define (xftcolor-alpha x)
  (xrendercolor-alpha (xftcolor-color x)))


;;;
;;; XftDraw
;;;

(define-foreign-type xftdraw (c-pointer (struct _XftDraw)))

(define xftdraw-create
  (foreign-lambda xftdraw XftDrawCreate
                  xdisplay    ;; dpy
                  drawable    ;; drawable
                  visual      ;; visual
                  colormap))  ;; colormap

(define xftdraw-create-bitmap
  (foreign-lambda xftdraw XftDrawCreateBitmap
                  xdisplay    ;; dpy
                  pixmap))    ;; bitmap

(define xftdraw-create-alpha
  (foreign-lambda xftdraw XftDrawCreateAlpha
                  xdisplay    ;; dpy
                  pixmap      ;; pixmap
                  int))       ;; depth

(define xftdraw-change
  (foreign-lambda void XftDrawChange
                  xftdraw
                  drawable))

(define xftdraw-display
  (foreign-lambda xdisplay XftDrawDisplay
                  xftdraw))

(define xftdraw-drawable
  (foreign-lambda drawable XftDrawDrawable
                  xftdraw))

(define xftdraw-colormap
  (foreign-lambda colormap XftDrawColormap
                  xftdraw)) ;; draw

(define xftdraw-visual
  (foreign-lambda visual XftDrawVisual
                  xftdraw)) ;; draw

(define xftdraw-destroy
  (foreign-lambda void XftDrawDestroy
                  xftdraw))

;; returns 0 if the X server does not support X Rendering Extension
(define xftdraw-picture
  (foreign-lambda picture XftDrawPicture
                  xftdraw))

(define xftdraw-set-clip!
  (foreign-lambda bool XftDrawSetClip
                  xftdraw   ;; draw
                  xregion)) ;; r

(define xftdraw-set-clip-rectangles!
  (foreign-lambda bool XftDrawSetClipRectangles
                  xftdraw                    ;; draw
                  int                        ;; xOrigin
                  int                        ;; yOrigin
                  (const xrectangle)         ;; *rects
                  int))                      ;; n

(define xftdraw-set-subwindow-mode!
  (foreign-lambda void XftDrawSetSubwindowMode
                  xftdraw             ;; draw
                  int))               ;; mode


;;;
;;; XftFontInfo
;;;

(define-foreign-type xftfontinfo (c-pointer (struct _XftFontInfo)))

(define xft-fontinfo-create
  (foreign-lambda xftfontinfo XftFontInfoCreate
                  xdisplay            ;; dpy
                  (const fcpattern))) ;; pattern

(define xft-fontinfo-destroy
  (foreign-lambda void XftFontInfoDestroy
                  xdisplay      ;; dpy
                  xftfontinfo)) ;; fi

(define xft-fontinfo-hash
  (foreign-lambda fcchar32 XftFontInfoHash
                  (const xftfontinfo))) ;; fi

(define xft-fontinfo-equal
  (foreign-lambda fcbool XftFontInfoEqual
                  (const xftfontinfo)   ;; a
                  (const xftfontinfo))) ;; b



;;;
;;; XftFont
;;;

(define-foreign-record-type (xftfont XftFont)
  (constructor: make-xftfont)
  (destructor: free-xftfont)
  (int ascent xftfont-ascent)
  (int descent xftfont-descent)
  (int height xftfont-height)
  (int max_advance_width xftfont-max-advance-width)
  (fccharset charset xftfont-charset)
  (fcpattern pattern xftfont-pattern))

(define xftfont-match
  (foreign-lambda fcpattern XftFontMatch
                  xdisplay               ;; dpy
                  int                    ;; screen
                  (const fcpattern)      ;; *pattern
                  (c-pointer fcresult))) ;; *result

;; this can return NULL
;; example FIELD is XFT_FAMILY
;; example TYPE is XftTypeString, XftTypeDouble
;; example VALUE is "charter" or 12.0
(define xft-font-open
  (foreign-lambda xftfont XftFontOpen
                  xdisplay ;; dpy
                  int      ;; screen
                  ;; ... (list of pattern element triples FIELD, TYPE, VALUE, terminating with a NULL)
                  )) ;; _X_SENTINEL(0)

(define xft-font-open/name
  (foreign-lambda xftfont XftFontOpenName
                  xdisplay           ;; dpy
                  int                ;; screen
                  (const c-string))) ;; name

(define xft-font-open/xlfd
  (foreign-lambda xftfont XftFontOpenXlfd
                  xdisplay           ;; dpy
                  int                ;; screen
                  (const c-string))) ;; xlfd

(define xft-font-open/info
  (foreign-lambda xftfont XftFontOpenInfo
                  xdisplay       ;; dpy
                  fcpattern      ;; pattern
                  xftfontinfo))  ;; fi

(define xft-font-open/pattern
  (foreign-lambda xftfont XftFontOpenPattern
                  xdisplay    ;; dpy
                  fcpattern)) ;; pattern

(define xft-lock-face
  (foreign-lambda ft_face XftLockFace
                  xftfont)) ;; pub

(define xft-unlock-face
  (foreign-lambda void XftUnlockFace
                  xftfont)) ;; pub

(define xft-font-copy
  (foreign-lambda xftfont XftFontCopy
                  xdisplay   ;; dpy
                  xftfont))  ;; pub

(define xft-font-close
  (foreign-lambda void XftFontClose
                  xdisplay   ;; dpy
                  xftfont))  ;; pub

(define xft-font-load-glyphs
  (foreign-lambda void XftFontLoadGlyphs
                  xdisplay                    ;; dpy
                  xftfont                     ;; pub
                  fcbool                      ;; need_bitmaps
                  (const (c-pointer ft_uint)) ;; *glyphs
                  int))                       ;; nglyph

(define xft-font-unload-glyphs
  (foreign-lambda void XftFontUnloadGlyphs
                  xdisplay                    ;; dpy
                  xftfont                     ;; pub
                  (const (c-pointer ft_uint)) ;; *glyphs,
                  int))                       ;; nglyph

(define XFT_NMISSING 256)

(define xft-font-check-glyph
  (foreign-lambda fcbool XftFontCheckGlyph
                  xdisplay            ;; dpy
                  xftfont             ;; pub
                  fcbool              ;; need_bitmaps
                  ft_uint             ;; glyph
                  (c-pointer ft_uint) ;; *missing
                  (c-pointer int)))   ;; *nmissing

(define xft-char-exists
  (foreign-lambda fcbool XftCharExists
                  xdisplay   ;; dpy
                  xftfont    ;; pub
                  fcchar32)) ;; ucs4

(define xft-char-index
  (foreign-lambda ft_uint XftCharIndex
                  xdisplay   ;; dpy
                  xftfont    ;; pub
                  fcchar32)) ;; ucs4


;;;
;;; XftCharSpec
;;;

(define-foreign-record-type (xftcharspec XftCharSpec)
  (constructor: make-xftcharspec)
  (destructor: free-xftcharspec)
  (fcchar32 ucs4 xftcharspec-ucs4)
  (short x xftcharspec-x)
  (short y xftcharspec-y))

;;;
;;; XftCharFontSpec
;;;

(define-foreign-record-type (xftcharfontspec XftCharFontSpec)
  (constructor: make-xftcharfontspec)
  (destructor: free-xftcharfontspec)
  (xftfont font xftcharfontspec-font)
  (fcchar32 ucs4 xftcharfontspec-ucs4)
  (short x xftcharfontspec-x)
  (short y xftcharfontspec-y))

;;;
;;; XftGlyphSpec
;;;

(define-foreign-record-type (xftglyphspec XftGlyphSpec)
  (constructor: make-xftglyphspec)
  (destructor: free-xftglyphspec)
  (ft_uint glyph xftglyphspec-glyph)
  (short x xftglyphspec-x)
  (short y xftglyphspec-y))

;;;
;;; XftGlyphFontSpec
;;;

(define-foreign-record-type (xftglyphfontspec XftGlyphFontSpec)
  (constructor: make-xftglyphfontspec)
  (destructor: free-xftglyphfontspec)
  (xftfont font xftglyphfontspec-font)
  (ft_uint glyph xftglyphspec-glyph)
  (short x xftglyphfontspec-x)
  (short y xftglyphfontspec-y))


;;;
;;; Misc
;;;

;; not sure xft-default-has-render does.  perhaps it tells whether there
;; are X Rendering Extensions available for the given display?
(define xft-default-has-render
  (foreign-lambda bool XftDefaultHasRender
                  xdisplay))    ;; dpy
    
(define xft-default-set
  (foreign-lambda bool XftDefaultSet
                  xdisplay      ;; dpy
                  fcpattern))   ;; defaults

(define xft-default-substitute
  (foreign-lambda void XftDefaultSubstitute
                  xdisplay      ;; dpy
                  int           ;; screen
                  fcpattern))   ;; pattern
    
(define xft-name-parse
  (foreign-lambda fcpattern XftNameParse
                  (const c-string))) ;; name

(define xft-xlfd-parse
  (foreign-lambda fcpattern XftXlfdParse
                  (const c-string) ;; xlfd_orig
                  bool             ;; ignore_scalable
                  bool))           ;; complete

;; not sure what xft-init does.  should it even be exported?
(define xft-init
  (foreign-lambda bool XftInit
                  (const c-string))) ;; config

(define xft-get-version
  (foreign-lambda int XftGetVersion))


(define xft-list-fonts
  (foreign-lambda fcfontset XftListFonts
                  xdisplay ;; dpy
                  int      ;; screen
                  ;; ... (variable number of args)
                  c-pointer ;; null
                  c-pointer ;; null
                  )) ;;  _X_SENTINEL(0)

(define xft-init-ft-library
  (foreign-lambda fcbool XftInitFtLibrary))


;;;
;;; Draw on Drawables
;;;

;; not sure what xft-draw-src-picture does, or whether it should be
;; renamed to xftdraw-src-picture
(define xft-draw-src-picture
  (foreign-lambda picture XftDrawSrcPicture
                  xftdraw            ;; draw
                  (const xftcolor))) ;; color

(define xft-draw-glyphs
  (foreign-lambda void XftDrawGlyphs
                  xftdraw                      ;; draw
                  (const xftcolor)             ;; color
                  xftfont                      ;; XftFont* pub
                  int                          ;; x
                  int                          ;; y
                  (const (c-pointer ft_uint))  ;; glyphs
                  int))                        ;; nglyphs

(define (xft-draw-string draw font color x y string)
  ((foreign-lambda void XftDrawStringUtf8
                   xftdraw
                   (const xftcolor)
                   xftfont
                   int
                   int
                   (const c-string)
                   int)
   draw color font (inexact->int x) (inexact->int y)
   string (string-length string)))

(define xft-draw-char-spec
  (foreign-lambda void XftDrawCharSpec
                  xftdraw              ;; draw
                  (const xftcolor)     ;; color
                  xftfont              ;; pub
                  (const xftcharspec)  ;; chars
                  int))                ;; len

(define xft-draw-char-font-spec
  (foreign-lambda void XftDrawCharFontSpec
                  xftdraw                 ;; draw
                  (const xftcolor)        ;; color
                  (const xftcharfontspec) ;; chars
                  int))                   ;; len

(define xft-draw-glyph-spec
  (foreign-lambda void XftDrawGlyphSpec
                  xftdraw                 ;; draw
                  (const xftcolor)        ;; color
                  xftfont                 ;; pub
                  (const xftglyphspec)    ;; glyphs
                  int))                   ;; len

(define xft-draw-glyph-font-spec
  (foreign-lambda void XftDrawGlyphFontSpec
                  xftdraw                  ;; draw
                  (const xftcolor)         ;; color
                  (const xftglyphfontspec) ;; glyphs
                  int))                    ;; len

(define xft-draw-rect
  (foreign-lambda void XftDrawRect
                  xftdraw                      ;; draw
                  (const xftcolor)             ;; color
                  int                          ;; x
                  int                          ;; y
                  unsigned-int                 ;; width
                  unsigned-int))               ;; height


;;;
;;; Extents
;;;

(define (xft-text-extents display font string)
  (let ((glyphinfo (make-xglyphinfo)))
    ((foreign-lambda void XftTextExtentsUtf8
                     xdisplay
                     xftfont
                     (const c-string)
                     int
                     xglyphinfo)
     display font string (string-length string) glyphinfo)
    glyphinfo))



;;;
;;; Rendering
;;;

(define xft-glyph-render
  (foreign-lambda void XftGlyphRender
                  xdisplay                    ;; dpy
                  int                         ;; op
                  picture                     ;; src
                  xftfont                     ;; pub
                  picture                     ;; dst
                  int                         ;; srcx
                  int                         ;; srcy
                  int                         ;; x
                  int                         ;; y
                  (const (c-pointer ft_uint)) ;; *glyphs
                  int))                       ;; nglyphs

(define xft-glyph-spec-render
  (foreign-lambda void XftGlyphSpecRender
                  xdisplay             ;; dpy
                  int                  ;; op
                  picture              ;; src
                  xftfont              ;; *pub
                  picture              ;; dst
                  int                  ;; srcx
                  int                  ;; srcy
                  (const xftglyphspec) ;; *glyphs
                  int))                ;; nglyphs

(define xft-char-spec-render
  (foreign-lambda void XftCharSpecRender
                  xdisplay            ;; dpy
                  int                 ;; op
                  picture             ;; src
                  xftfont             ;; *pub
                  picture             ;; dst
                  int                 ;; srcx
                  int                 ;; srcy
                  (const xftcharspec) ;; *chars
                  int))               ;; len

(define xft-glyph-font-spec-render
  (foreign-lambda void XftGlyphFontSpecRender
                  xdisplay                 ;; dpy
                  int                      ;; op
                  picture                  ;; src
                  picture                  ;; dst
                  int                      ;; srcx
                  int                      ;; srcy
                  (const xftglyphfontspec) ;; *glyphs
                  int))                    ;; len

(define xft-char-font-spec-render
  (foreign-lambda void XftCharFontSpecRender
                  xdisplay                 ;; dpy
                  int                      ;; op
                  picture                  ;; src
                  picture                  ;; dst
                  int                      ;; srcx
                  int                      ;; srcy
                  (const xftcharfontspec)  ;; *chars
                  int))                    ;; len

(define xft-text-render-8
  (foreign-lambda void XftTextRender8
                  xdisplay                     ;; dpy
                  int                          ;; op
                  picture                      ;; src
                  xftfont                      ;; pub
                  picture                      ;; dst
                  int                          ;; srcx
                  int                          ;; srcy
                  int                          ;; x
                  int                          ;; y
                  (const (c-pointer fcchar8))  ;; string
                  int))                        ;; len

(define xft-text-render-16
  (foreign-lambda void XftTextRender16
                  xdisplay                     ;; dpy
                  int                          ;; op
                  picture                      ;; src
                  xftfont                      ;; pub
                  picture                      ;; dst
                  int                          ;; srcx
                  int                          ;; srcy
                  int                          ;; x
                  int                          ;; y
                  (const (c-pointer fcchar16)) ;; string
                  int))                        ;; len

(define xft-text-render-16be
  (foreign-lambda void XftTextRender16BE
                  xdisplay                    ;; dpy
                  int                         ;; op
                  picture                     ;; src
                  xftfont                     ;; pub
                  picture                     ;; dst
                  int                         ;; srcx
                  int                         ;; srcy
                  int                         ;; x
                  int                         ;; y
                  (const (c-pointer fcchar8)) ;; string
                  int))                       ;; len

(define xft-text-render-16le
  (foreign-lambda void XftTextRender16LE
                  xdisplay                    ;; dpy
                  int                         ;; op
                  picture                     ;; src
                  xftfont                     ;; pub
                  picture                     ;; dst
                  int                         ;; srcx
                  int                         ;; srcy
                  int                         ;; x
                  int                         ;; y
                  (const (c-pointer fcchar8)) ;; string
                  int))                       ;; len

(define xft-text-render-32
  (foreign-lambda void XftTextRender32
                  xdisplay                     ;; dpy
                  int                          ;; op
                  picture                      ;; src
                  xftfont                      ;; pub
                  picture                      ;; dst
                  int                          ;; srcx
                  int                          ;; srcy
                  int                          ;; x
                  int                          ;; y
                  (const (c-pointer fcchar32)) ;; string
                  int))                        ;; len

(define xft-text-render-32be
  (foreign-lambda void XftTextRender32BE
                  xdisplay                    ;; dpy
                  int                         ;; op
                  picture                     ;; src
                  xftfont                     ;; pub
                  picture                     ;; dst
                  int                         ;; srcx
                  int                         ;; srcy
                  int                         ;; x
                  int                         ;; y
                  (const (c-pointer fcchar8)) ;; string
                  int))                       ;; len

(define xft-text-render-32le
  (foreign-lambda void XftTextRender32LE
                  xdisplay                    ;; dpy
                  int                         ;; op
                  picture                     ;; src
                  xftfont                     ;; pub
                  picture                     ;; dst
                  int                         ;; srcx
                  int                         ;; srcy
                  int                         ;; x
                  int                         ;; y
                  (const (c-pointer fcchar8)) ;; string
                  int))                       ;; len

(define xft-text-render-utf8
  (foreign-lambda void XftTextRenderUtf8
                  xdisplay                     ;; dpy
                  int                          ;; op
                  picture                      ;; src
                  xftfont                      ;; pub
                  picture                      ;; dst
                  int                          ;; srcx
                  int                          ;; srcy
                  int                          ;; x
                  int                          ;; y
                  (const (c-pointer fcchar8))  ;; string
                  int))                        ;; len

(define xft-text-render-utf16
  (foreign-lambda void XftTextRenderUtf16
                  xdisplay                     ;; dpy
                  int                          ;; op
                  picture                      ;; src
                  xftfont                      ;; pub
                  picture                      ;; dst
                  int                          ;; srcx
                  int                          ;; srcy
                  int                          ;; x
                  int                          ;; y
                  (const (c-pointer fcchar8))  ;; string
                  fcendian                     ;; endian
                  int))                        ;; len

)

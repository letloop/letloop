;; Character sets for Unicode boundaries, TR29.
;; This code is written by Alex Shinn and placed in the
;; Public Domain.  All warranties are disclaimed.

;;> Char-sets used for
;;> \hyperlink["http://unicode.org/reports/tr29/"]{TR29} word
;;> boundaries.

(library (srfi srfi-115 boundary)
  (export char-set:regional-indicator
          char-set:extend-or-spacing-mark
          char-set:hangul-l
          char-set:hangul-v
          char-set:hangul-t
          char-set:hangul-lv
          char-set:hangul-lvt)
    (import (chezscheme)
          (srfi srfi-14))
  ;; generated with:
  ;; tools/extract-unicode-props.scm --derived GraphemeBreakProperty.txt
  ;;   Control extend-or-spacing-mark=Extend,SpacingMark Regional_Indicator
  ;;   hangul-l=:L hangul-v=:V hangul-t=:T hangul-lv=:LV hangul-lvt=:LVT
  (begin
    (define (immutable-char-set cs) cs)
    (include "srfi/srfi-115/boundary.body.scm")))

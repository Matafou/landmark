; AUTHOR:  Nikolaj Schumacher -- https://github.com/nschum/fringe-helper.el
(defun landmark--fringe-helper-convert (&rest strings)
"Convert STRINGS into a vector usable for `define-fringe-bitmap'.
Each string in STRINGS represents a line of the fringe bitmap.
Periods (.) are background-colored pixel; Xs are foreground-colored. The
fringe bitmap always is aligned to the right. If the fringe has half
width, only the left 4 pixels of an 8 pixel bitmap will be shown.
For example, the following code defines a diagonal line.
\(fringe-helper-convert
\"XX......\"
\"..XX....\"
\"....XX..\"
\"......XX\"\)"
  (unless (cdr strings)
  ;; only one string, probably with newlines
    (setq strings (split-string (car strings) "\n")))
  (apply 'vector
    (mapcar
      (lambda (str)
        (let ((num 0))
          (dolist (c (string-to-list str))
            (setq num (+ (* num 2) (if (eq c ?.) 0 1))))
          num))
      strings)))

(define-fringe-bitmap 'landmark-fringe-zero (landmark--fringe-helper-convert
  "......."
  "......."
  "......."
  "......."
  ".XXX..."
  "XXXXXX."
  "XX..XX."
  "XX..XX."
  "XX..XX."
  "XX..XX."
  "XX..XX."
  "XX..XX."
  "XX..XX."
  "XX..XX."
  "XXX.XX."
  ".XXXXX."
  "..XXX.."
  "......."
  "......."
  "......."))

(define-fringe-bitmap 'landmark-fringe-one (landmark--fringe-helper-convert
  "......."
  "......."
  "......."
  "......."
  "......."
  "....XX."
  "..XXXX."
  ".XXXXX."
  "XXXXXX."
  "XX.XXX."
  "...XXX."
  "...XXX."
  "...XXX."
  "...XXX."
  "...XXX."
  "...XXX."
  "...XXX."
  "......."
  "......."
  "......."))

(define-fringe-bitmap 'landmark-fringe-two (landmark--fringe-helper-convert
  "......."
  "......."
  "......."
  "......."
  "......."
  ".XXXXX."
  "XXXXXXX"
  "XX..XXX"
  "XX..XXX"
  "....XXX"
  "...XXX."
  "..XXX.."
  ".XXX..."
  "XXX...."
  "XXX...."
  "XXXXXXX"
  "XXXXXXX"
  "......."
  "......."
  "......."))

(define-fringe-bitmap 'landmark-fringe-three (landmark--fringe-helper-convert
  "......."
  "......."
  "......."
  "......."
  ".XXXXX."
  "XXXXXXX"
  "XX..XXX"
  "XX..XXX"
  "....XXX"
  "...XXXX"
  "..XXXX."
  "...XXXX"
  "....XXX"
  "XX..XXX"
  "XX..XXX"
  "XXXXXXX"
  ".XXXXX."
  "......."
  "......."
  "......."))

(define-fringe-bitmap 'landmark-fringe-four (landmark--fringe-helper-convert
  "......."
  "......."
  "......."
  "......."
  ".....X."
  "....XX.."
  "...XXX."
  "..XXXX."
  "..XXXX."
  ".XX.XX."
  "XX..XX."
  "XX..XX."
  "XXXXXXX"
  "XXXXXXX"
  "....XX."
  "....XX."
  "....XX."
  "......."
  "......."
  "......."))

(define-fringe-bitmap 'landmark-fringe-five (landmark--fringe-helper-convert
  "......."
  "......."
  "......."
  "......."
  ".XXXXX."
  "XXXXXXX"
  "XXX...."
  "XXX...."
  "XXX...."
  "XXXXX.."
  "XXXXXX."
  "...XXXX."
  "....XXX"
  "....XXX"
  "X..XXXX"
  "XXXXXX."
  "XXXXX.."
  "......."
  "......."
  "......."))

(define-fringe-bitmap 'landmark-fringe-six (landmark--fringe-helper-convert
  "......."
  "......."
  "......."
  "......."
  "......."
  ".XXXXX."
  "XXXXXXX"
  "XXX..XX"
  "XXX...."
  "XXX...."
  "XXXXXX."
  "XXXXXXX."
  "XX..XXX"
  "XX..XXX"
  "XX..XXX"
  "XXXXXXX"
  ".XXXXX."
  "......."
  "......."
  "......."))

(define-fringe-bitmap 'landmark-fringe-seven (landmark--fringe-helper-convert
  "......."
  "......."
  "......."
  "......."
  "......."
  "XXXXXX."
  "XXXXXX."
  "XXXXxX."
  "...XXX."
  "...XXX."
  "..XXX.."
  "..XXX.."
  ".XXX..."
  ".XXX..."
  "XXX...."
  "XXX...."
  "XXX...."
  "......."
  "......."
  "......."))

(define-fringe-bitmap 'landmark-fringe-eight (landmark--fringe-helper-convert
  "......."
  "......."
  "......."
  "......."
  "..XXXX."
  ".XXXXXX"
  "XXX.XXX"
  "XX...XX"
  "XX...XX"
  "XXX.XXX"
  ".XXXXX."
  "XXX.XXX"
  "XX...XX"
  "XX...XX"
  "XXX.XXX"
  "XXXXXX."
  ".XXXX.."
  "......."
  "......."
  "......."))

(define-fringe-bitmap 'landmark-fringe-nine (landmark--fringe-helper-convert
  "......."
  "......."
  "......."
  "......."
  ".XXXXX."
  "XXXXXXX"
  "XXX.XXX"
  "XX..XXX"
  "XXX.XXX"
  "XXXXXXX"
  "XXXXXXX"
  "....XXX"
  "....XXX"
  "....XXX"
  "XX..XXX"
  "XXXXXXX"
  ".XXXXX."
  "......."
  "......."
  "......."))


(provide 'landmark-bmp)


(progn
    ; Game of life sample app
    (setq _MATCH_PARENT "match_parent")
    (setq _WRAP_CONTENT "wrap_content")
    

    

    (setq alive-color
          "#02004C")

    (setq death-color
          "white")

    (setq board-color
          "white")

    (setq rows 25)
    (setq cols 25)
    (setq col-list (make-range 0 (- cols 1)))
    (setq row-list (make-range 0 (- rows 1)))

    (setq grid-width 300)
    (setq grid-height 300)

    (setq view-index 0)
    (setq current-state-index 1)
    (setq next-state-index 2)

    (setq num-cells (* rows cols))
    (setq cells
          (fill-list num-cells (list F F F)))

    (defun umod (value mod)
        (or (and (<= 0
                              (setq v
                                        (mod value mod)))
                        v)
            (+ v
                mod)))

    (defun get-cell-index (i j)
        (+ (* i cols) j))

    (defun get-index-coordinate (array-index)
        (list (integer (/ array-index cols))
              (mod array-index cols)))

    (setq surround-cell-offsets
          (list (-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))

    (defun create-cell-view (i j width height)
        (setq state-specification
              (nth cells
                   (setq array-index
                         (get-cell-index i j))))

        (set-nth state-specification
                 view-index
                 (setq cell-view
                       (solid :width width
                              :height height
                              :background-color board-color)))
        (set-nth cells
                 array-index
                 state-specification)
        cell-view)

    (comment

    (setq x (map-some (o i) cells (and (second o) i)))
      ; testing
     (setq s (map-some (s i) cells (progn (setq coord (get-index-coordinate i)) (< 0 (get-num-surrounding-alive-cells (first coord) (second coord))))))
     (setq coord (get-index-coordinate 82))
     (get-num-surrounding-alive-cells (first coord) (second coord))
      (setq cell-width
            (integer (/ grid-width cols)))
      (setq cell-height
            (integer (/ grid-height rows)))

      (multiple-bind (i j)
                     (list 0 0))
      (create-cell-view i j cell-width cell-height)

      )

    (defun create-game-board ()
        (setq cell-width
              (integer (/ grid-width cols)))
        (setq cell-height
              (integer (/ grid-height rows)))

        (vertical-layout :width _WRAP_CONTENT
                         :height _WRAP_CONTENT
                         :background-color board-color
                         (mapcar i
                                 row-list
                                 (horizontal-layout :width _WRAP_CONTENT
                                                    :height _WRAP_CONTENT
                                                    (mapcar j
                                                            col-list
                                                            (create-cell-view i j cell-width cell-height))))))


    (defun initialize-cell-state (initial-state)
        (setq initial-state 
              (or initial-state
                  (fill-list num-cells F)))
        (for i
             num-cells
             cells
             (progn
                (setq state-specification
                      (nth cells i))

                (set-nth state-specification
                         next-state-index
                         (nth initial-state i))
                (set-nth cells
                         i
                         state-specification))))

    (defun get-num-surrounding-alive-cells (i j)
       
       
       
        (length (find offset
                      surround-cell-offsets
                      (nth (nth cells
                                (get-cell-index (umod (+ i (first offset))
                                                      rows)
                                                (umod (+ j (second offset))
                                                      cols)))
                           current-state-index))))

   (defun get-default ()
      (lambda (r e) (if e (log-error "errors" e))))

    (defun do-display-pass ()
        (setq updated ())
        (for array-index
             num-cells
             (evaluate-foreground (get-default)
                                                   (for spec 
                                                           updated F 
                                                           (progn (multiple-bind (cell-view current-cell-alive-p) spec)
                                                                        (if current-cell-alive-p
                     (update-parameters cell-view
                                        :background-color alive-color)
                     (update-parameters cell-view
                                        :background-color death-color)))))
             (progn
                 (multiple-bind (cell-view current-cell-alive-p next-state-alive-p)
                                (setq state (nth cells array-index)))
                 (if (or (and current-cell-alive-p (not next-state-alive-p))
                            (and (not current-cell-alive-p) next-state-alive-p))
                      (set updated (append updated (list (list cell-view next-state-alive-p)))))
                 (set-nth state
                          current-state-index
                          (setq current-cell-alive-p
                                next-state-alive-p))
                 (set-nth state
                          next-state-index
                          F)

                 (set-nth cells
                          array-index
                          state)

                 )))

    (defun do-update-pass ()
        (setq updated-p F)
        (for array-index
             num-cells
             (not updated-p)
             (progn
                 (multiple-bind (i j)
                                (get-index-coordinate array-index))
                 (setq num-neighbors
                       (get-num-surrounding-alive-cells i j))
                 (setq state-specification
                       (nth cells
                            array-index))

                 (setq cell-is-alive-p
                       (nth state-specification current-state-index))

                 (set-nth state-specification
                          next-state-index
                          (if cell-is-alive-p
                              (and (>= num-neighbors 2)
                                   (< num-neighbors 4))
                              (= num-neighbors 3)))
                 (set-nth cells array-index state-specification)
                 (set updated-p
                      (or updated-p
                          (not (or (and (not (nth state-specification next-state-index))
                                         (not (nth state-specification current-state-index)))
                                    (and (nth state-specification next-state-index)
                                         (nth state-specification current-state-index)))))))))

(setq counts F)
(defun get-neighbor-counts ()
        
    (set counts (mapcar (state-specification array-index)
            cells
         (progn
             (multiple-bind (i j)
                            (get-index-coordinate array-index))
             (setq num-neighbors
                   (get-num-surrounding-alive-cells i j))
             

             (setq cell-is-alive-p
                   (nth state-specification current-state-index))

             (set-nth state-specification
                      next-state-index
                      (if cell-is-alive-p
                          (and (>= num-neighbors 2)
                               (< num-neighbors 4))
                          (= num-neighbors 3)))
             (set-nth cells array-index state-specification)
             (list num-neighbors cell-is-alive-p (list i j))))))

    (defun process-cells ()
        
        (do-update-pass)
        (do-display-pass))

    (defun on-randomize ()
        (initialize-cell-state (mapcar cell-state
                                       cells
                                       (and (> (random 0 1) 0.5)
                                            1)))
        (do-display-pass))

    (setq running-p F)

    (defun on-start ()
        (set running-p 1)
        (setq finished F)
        (evaluate-background "game" 
                                                (lambda (r e) (if e (log-error "errors" e)))
                                                (while (or (not finished)
                                                                   running-p)
                                                       (setq finished
                                                                (process-cells)))
                                                (set running-p F)
                                                (evaluate-foreground (get-default ) (show-short-toast "finished")))
        
        
       
        )

    (defun on-stop ()
        (set running-p F))

    (setq parent-view
          (vertical-layout (scrollview :width _MATCH_PARENT
                                       :height "100%"
                                       (horizontal-scrollview :width _MATCH_PARENT
                                                              :height _WRAP_CONTENT
                                                              (create-game-board)))
                           (horizontal-layout :width _MATCH_PARENT
                                              :height _WRAP_CONTENT
                                              (button "start"
                                                      :on-click (on-start))
                                              (button "step"
                                                      :on-click (progn (get-neighbor-counts)(do-display-pass)(show-short-toast "step")))
                                              (button "stop"
                                                      :on-click (on-stop))
                                              (button "randomize"
                                                      :on-click (on-randomize)))))

    (set-top-view parent-view)      

)
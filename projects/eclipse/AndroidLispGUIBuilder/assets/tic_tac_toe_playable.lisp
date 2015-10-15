(setq radio-padding 0)

(defun empty (string-list-or-map)
    (or (not string-list-or-map)
        (= 0 (length string-list-or-map))))

(defun incr-map (table key value)
    (setq value
          (or value 1))
    (setq prior
          (or (gethash table
                       key)
              0))
    (defhash table
             key
             (+ prior value)))


(defun incr-list-map (map key value)
    (if (setq prior 
              (gethash map key))
        (defhash map 
                 key
                 (append-item prior value))
        (defhash map
                 key
                 (list value)))
    map)

(defun coordinate-string (i j)
    (concat (string (integer i))
            ","
            (string (integer j))))

(defun incr-player-score (reset)
    (set player-score
         (or (and reset 0)
             (+ 1 player-score)))

    (if player-score-text
        (set-text player-score-text
              (string (integer player-score))))
    )

(defun incr-ai-score (reset)
    (set ai-score
         (or (and reset 0)
             (+ 1 ai-score)))
    (if ai-score-text
        (set-text ai-score-text
                  (string (integer ai-score))))
    )

(defun incr-draw-count (reset)
    (set draw-count
         (or (and reset 0)
             (+ 1 draw-count)))
    
    (if draw-score-text
        (set-text draw-score-text
              (string (integer draw-count))))
    )


(defun initialize ()
    (new-game)
    (incr-player-score 1)
    (incr-ai-score 1)
    (incr-draw-count 1))

(defun new-game ()
    (set position-mark-map
         (for i
             3
             position-mark-map
             (for j
                  3
                  F
                  (defhash position-mark-map
                           (coordinate-string i j)
                           ""))))
    (set finished F)
    (update-grid))

(defmacro build-score-columns (...)
    `(list ,@(mapcar column-spec
                    ...
                    `(vertical-layout :width "33%"
                                      :height "wrap_content"
                                      :child-align "center"
                                      (text :width "wrap_content"
                                            :height "wrap_content"
                                            ,(first column-spec))
                                      (set ,(second column-spec)
                                            (text :width "wrap_content"
                                                  :height "wrap_content"
                                                  :text-align "center"
                                                  "0"))))))

(defmacro udpate-solution-map (solution-map mark)
    `(or (and (empty type-map)
              (defhash* ,solution-map i 1))
         (and (= 1 (length type-map))
              (setq score (gethash type-map ,mark))
              (defhash* ,solution-map i (integer (+ 1 score))))
         ,solution-map))

(setq _AI_SCORE_WEIGHT 1)

(defun get-next-ai-move ()
    (multiple-bind (x-solution-path-map o-solution-path-map position-solution-map move-score-map)
                  (list (make-int-hashtable)
                        (make-int-hashtable)
                        (make-string-hashtable)
                        (make-string-hashtable)))

    (for (path i)
         solution-path-list
         (for pt
              (if (empty position-solution-map)
                  (return F)
                  (get-hash-keys position-solution-map))
              (maximal-value-map pt
                                 (if (empty move-score-map)
                                     (return F)
                                     (get-hash-keys move-score-map))
                                 (gethash move-score-map
                                          pt)
                                 pt)
              (for solution-path-index
                   (gethash position-solution-map pt)
                   F
                   (progn
                        (if (setq score (gethash x-solution-path-map
                                                 solution-path-index))
                            (incr-map move-score-map
                                      pt
                                      (+ (if (equals _X_MARK ai-mark) _AI_SCORE_WEIGHT 0) score)))
                        (if (setq score (gethash o-solution-path-map
                                                 solution-path-index))
                            (incr-map move-score-map
                                      pt
                                      (+ (if (equals _O_MARK ai-mark) _AI_SCORE_WEIGHT 0) score))))))
         (progn
            (setq type-map
                  (make-string-hashtable))
            (for pt
                 path
                 (multiple-set (x-solution-path-map o-solution-path-map)
                               (list (udpate-solution-map x-solution-path-map _X_MARK)
                                     (udpate-solution-map o-solution-path-map _O_MARK)))
                 (progn
                    (setq mark (gethash position-mark-map pt))
                    
                    (if (equals mark _X_MARK)
                        (incr-map type-map _X_MARK))
                    (if (equals mark _O_MARK)
                        (incr-map type-map _O_MARK))
                    (if (= 0 (length mark))
                        (incr-list-map position-solution-map pt i)))))))



(multiple-bind (player-score ai-score draw-count)
               (0 0 0))
(setq click-text "press")
(setq original-label "scores")
(setq check-boxes F)
(setq player-score-text F)
(setq ai-score-text F)
(setq draw-score-text F)
(setq board-container F)
(setq finished F)
(setq solution-path-list
      (list ("0,0" "0,1" "0,2")
            ("1,0" "1,1" "1,2")
            ("2,0" "2,1" "2,2")
            ("0,0" "1,0" "2,0")
            ("0,1" "1,1" "2,1")
            ("0,2" "1,2" "2,2")
            ("0,0" "1,1" "2,2")
            ("0,2" "1,1" "2,0")))

(setq position-mark-map
    (make-string-hashtable))

(setq _X_MARK "X")

(setq _O_MARK "O")
    
(setq ai-mark _X_MARK)

(setq player-mark _O_MARK)

(setq board-text-map
      (make-string-hashtable))

(setq mark-text-size 17)



(setq board-text-map
    (for i
         3
         board-text-map
         (for j
              3
              F
              (defhash board-text-map
                       (coordinate-string i j)
                       (text ""
                            :width "match_parent"
                            :height "wrap_content"
                            :background-color "white"
                            :text-align "center"
                            :parent-align "center"
                            :padding 20
                            :text-size mark-text-size
                            :on-click (set-user-move (coordinate-string i j))
                             )))))



(defun is-winning-mark-p (mark)
    (some path
          solution-path-list
          (all coordinate-string
               path
               (equals mark
                       (gethash position-mark-map
                                coordinate-string)))))

(defun game-can-be-won-p ()
    (some path
          solution-path-list
          (or (all coordinate-string
                   path
                   (or (equals _X_MARK
                               (gethash position-mark-map
                                        coordinate-string))
                       (empty (gethash position-mark-map
                                           coordinate-string))))
              (all coordinate-string
                   path
                   (or (equals _O_MARK
                               (gethash position-mark-map
                                        coordinate-string))
                       (empty (gethash position-mark-map
                                           coordinate-string)))))))




(defun do-ai-move ()
    (if (setq ai-move
              (get-next-ai-move))
        (progn
            (defhash position-mark-map
                     ai-move
                     ai-mark)
            (update-grid)
            (if (is-winning-mark-p ai-mark)
                (progn
                    (incr-ai-score)
                    (set finished 1))
                (if (not (game-can-be-won-p))
                    (progn
                        (incr-draw-count)
                        (set finished 1)))))
        (if (not (game-can-be-won-p))
            (progn
                (incr-draw-count)
                (set finished 1)))))

(defun set-user-move (coordinate-string)
    (if (or (< 0 (length (gethash position-mark-map coordinate-string)))
            finished)
        (return F))
    (defhash position-mark-map
             coordinate-string
             player-mark)
    (update-grid)
    (if (is-winning-mark-p player-mark)
        (progn
            (incr-player-score)
            (set finished 1))
        (if (game-can-be-won-p)
            (do-ai-move)
            (progn
                (incr-draw-count)
                (set finished 1)))))


(defun create-grid-text (i j)
    (setq border-width 1)
    (relative :width "33%"
              :height "wrap_content"
              :background-color "black"
              :padding-top (if (> i 0) border-width 0)
              :padding-left (if (> j 0) border-width 0)
              :padding-right (if (< j 2) border-width 0)
              :padding-bottom (if (< i 2) border-width 0)
              (gethash board-text-map
                       (coordinate-string i j))))


(defun create-grid ()
      (mapcar i
              (0 1 2)
              (horizontal-layout :width "match_parent"
                                 :height "wrap_content"
                                 (mapcar j
                                         (0 1 2)
                                         (create-grid-text i
                                                           j)))))


(defun update-grid ()
    (for i
         3
         F
         (for j
              3
              F
              (set-text (gethash board-text-map
                                     (coordinate-string i j))
                        (gethash position-mark-map
                                 (coordinate-string i j))))))


(initialize)

(setq ai-starts-p F)


(vertical-layout :height "match_parent"
                 :width "match_parent"
                 :background-color "#FFFFFF"
                 :padding 5
                 (text "Tic-Tac-Toe" 
                       :parent-align "center"
                       :padding-top 13
                       :text-size 26
                       :text-style "bold|italic")
                 (setq top-label
                       (text "Scores"
                             :padding-top 10
                             :padding-bottom 10
                             :parent-align "center"))
                 (horizontal-layout :width "match_parent"
                                    :height "wrap_content"
                                    (build-score-columns ("Player" player-score-text)
                                                         ("AI" ai-score-text)
                                                         ("Draws" draw-score-text)))
                 (shadow-button "Reset"
                         :parent-align "left"
                         :on-click "(initialize)")
                 (text "Select your mark:"
                       :parent-align "left")
                 (horizontal-radio-group :width "match_parent"
                                         :height "wrap_content"
                                         (radio-button "X" :padding-left radio-padding
                                                           :on-click "(setq player-mark _X_MARK) (setq ai-mark _O_MARK)")
                                         (radio-button "O"
                                                       :padding-left radio-padding
                                                       :on-click "(setq player-mark _O_MARK ) (setq ai-mark _X_MARK)"
                                                       :margin-left 13))
                 (text "Who starts:"
                       :parent-align "left"
                       :margin-top 5)
                 (horizontal-radio-group :width "match_parent"
                                         :height "wrap_content"
                                         (radio-button "Player" :on-click "(setq ai-starts-p F)" :padding-left radio-padding)
                                         (radio-button "AI"
                                                       :padding-left radio-padding
                                                       :on-click "(setq ai-starts-p 1)"
                                                       :margin-left 13))
                 (set board-container
                       (vertical-layout :width "match_parent"
                                        :height "wrap_content"
                                        (create-grid )))
                 
                 (shadow-button "New Game"
                         :parent-align "left"
                         :on-click "(new-game) (if ai-starts-p (do-ai-move))"))
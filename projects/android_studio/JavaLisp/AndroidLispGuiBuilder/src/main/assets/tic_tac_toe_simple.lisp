(progn


    (setq radio-padding 0)

    (setq position-mark-spec
          (list ("O"  "X" "X")
                ("X"  "O" "O")
                (""   "O" "X")))




    (defun create-grid-text (i j)
        (setq border-width 1)
        (relative :width "33%"
                  :height "wrap_content"
                  :background-color "black"
                  :padding-top (if (> i 0) border-width 0)
                  :padding-left (if (> j 0) border-width 0)
                  :padding-right (if (< j 2) border-width 0)
                  :padding-bottom (if (< i 2) border-width 0)
                  (text (nth (nth position-mark-spec i) j)
                        :width "match_parent"
                        :height "wrap_content"
                        :background-color "white"
                        :text-align "center"
                        :parent-align "center"
                        :padding 20)))


    (set-top-view (vertical-layout :height "match_parent"
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
                                        (vertical-layout :width "33%"
                                                         :height "wrap_content"
                                                         :child-align "center"
                                                          (text :width "wrap_content"
                                                                :height "wrap_content"
                                                                "Player")
                                                          (text :width "wrap_content"
                                                                      :height "wrap_content"
                                                                      :text-align "center"
                                                                      "0"))
                                        (vertical-layout :width "33%"
                                                         :height "wrap_content"
                                                         :child-align "center"
                                                          (text :width "wrap_content"
                                                                :height "wrap_content"
                                                                "AI")
                                                          (text :width "wrap_content"
                                                                      :height "wrap_content"
                                                                      :text-align "center"
                                                                      "0"))
                                        (vertical-layout :width "33%"
                                                         :height "wrap_content"
                                                         :child-align "center"
                                                          (text :width "wrap_content"
                                                                :height "wrap_content"
                                                                "Draw")
                                                          (text :width "wrap_content"
                                                                      :height "wrap_content"
                                                                      :text-align "center"
                                                                      "1")))
                     (shadow-button "Reset"
                                    :parent-align "left")
                     (text "Select your mark:"
                           :parent-align "left")
                     (horizontal-radio-group :width "match_parent"
                                             :height "wrap_content"
                                             (radio-button "X" :padding-left radio-padding)
                                             (radio-button "O" :padding-left radio-padding
                                                               :margin-left 13))
                     (text "Who starts:"
                           :parent-align "left"
                           :margin-top 5)
                     (horizontal-radio-group :width "match_parent"
                                             :height "wrap_content"
                                             (radio-button "Player" :padding-left radio-padding)
                                             (radio-button "AI" :padding-left radio-padding
                                                           :margin-left 13))

                     (vertical-layout :width "match_parent"
                                      :height "wrap_content"
                                      (mapcar i
                                              (0 1 2)
                                              (horizontal-layout :width "match_parent"
                                                                 :height "wrap_content"
                                                                 (mapcar j
                                                                         (0 1 2)
                                                                         (create-grid-text i
                                                                                           j)))))

                     (shadow-button "New Game"
                             :parent-align "left")))
)

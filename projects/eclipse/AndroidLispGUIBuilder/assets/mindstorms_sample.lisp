(defun add-right-shadow (proxy shadow-color width)
    (relative :width "wrap_content"
              :height "wrap_content"
              :background-color shadow-color
              (update-parameters proxy
                                 :margin-right width)))


(defun add-bottom-right-shadow (proxy shadow-color width)
    (relative :width "wrap_content"
              :height "wrap_content"
              :background-color shadow-color
              (update-parameters proxy
                                 :margin-right width
                                 :margin-bottom width)))

(setq shadow-width 5)

(vertical-layout :width "match_parent"
                 :height "match_parent"
                 :padding 10
                 (add-right-shadow (text "Bluetooth Status:"
                                         :background-color "#FCF1E0"
                                         :width "wrap_content"
                                         :height "wrap_content"
                                         :padding 10)
                                    "#D8D8D8"
                                    shadow-width)
                 (add-bottom-right-shadow (vertical-layout :width "match_parent"
                                                           :height "wrap_content"
                                                           :background-color "#FCF1E0"
                                                           :padding 10
                                                           (horizontal-layout :width "match_parent"
                                                                              :height "wrap_content"
                                                                              (text "Bluetooth adapter status:"
                                                                                    :width "50%"
                                                                                    :heigth "wrap_content"
                                                                                    :child-align "left")
                                                                              (set adapter-status
                                                                                    (text "Present"
                                                                                          :width "50%"
                                                                                          :heigth "wrap_content"
                                                                                          :child-align "right"
                                                                                          :text-color "green")))
                                                           (horizontal-layout :width "match_parent"
                                                                              :height "wrap_content"
                                                                              (text "Bluetooth connection status:"
                                                                                    :width "50%"
                                                                                    :heigth "wrap_content"
                                                                                    :child-align "left")
                                                                              (set connection-status
                                                                                   (text "not connected"
                                                                                         :width "50%"
                                                                                         :heigth "wrap_content"
                                                                                         :child-align "right"
                                                                                         :text-color "red"))))
                                            "#D8D8D8"
                                            shadow-width)
                (horizontal-layout :width "match_parent"
                                   :height "wrap_content"
                                   :child-align "center"
                                   (button "forward"
                                           :padding 10))
                (relative :width "match_parent"
                          :height "wrap_content"
                          (button "left"
                                  :padding 10
                                  :parent-align "left")
                          (button "stop"
                                  :padding 10
                                  :parent-align "center")
                          (button "right"
                                  :padding 10
                                  :parent-align "right"))
                (horizontal-layout :width "match_parent"
                                   :height "wrap_content"
                                   :child-align "center"
                                   (button "back"
                                           :padding 10))
                (button :width "match_parent"
                        :height "wrap_content"
                        :margin 10
                        "configure ports"
                        :text-size 20
                        :text-style "bold|italic")

                (relative :width "match_parent"
                          :height "wrap_content"
                          :margin-top 20
                          (button "get sonar"
                                  :parent-align "left"
                                  :padding 10)
                          (setq sonar-reading 
                                (text "0"
                                      :parent-align "right")))
                (relative :width "match_parent"
                          :height "wrap_content"
                          :margin-top 20
                          (button "get touch"
                                  :parent-align "left"
                                  :padding 10)
                          (setq sonar-reading 
                                (text "0"
                                      :parent-align "right"))))
                
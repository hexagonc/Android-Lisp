(setq VIEW_PROXY
      (vertical-layout :width "match_parent"
                     :height "match_parent"
                     :background-color "#FFFF00"
                     (mapcar label
                             (list "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
                             (text label
                                   :width "wrap_content"
                                   :height "wrap_content"
                                   :text-size 30
                                   :margin-bottom 15))))
(defun empty (string-list-or-map)
    (or (not string-list-or-map)
        (= 0 (length string-list-or-map))))

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

(setq configured-p F)


(setq _SONAR_PORT 0)
(setq _SWITCH_1_PORT 1)
(setq _SWITCH_2_PORT 2)

(setq _SONAR_SENSOR_TYPE 1)
(setq _SWITCH_SENSOR_TYPE 0)

(defun configure-ports ()
  (configure-sonar _SONAR_PORT)
  (configure-switch _SWITCH_1_PORT)
  (configure-switch _SWITCH_2_PORT))

(setq nxt F)

(setq _LOG_TAG "mindstorms-lisp")

(defun init ()
  (if (not (nxt-service-running-p))
      (start-service))

  (unless (nxt-service-running-p)
    (sleep-milli 20))
  (setup)
  (configure-ports))

(defun setup ()
  (set nxt-list
       (find dev 
             (get-paired-devices)
             (> (index-of (get-device-name dev)
                          "NXT")
                -1)))

  (log _LOG_TAG "paired devices: " 
                (mapcar dev 
                        nxt-list 
                        (list (get-device-name dev) 
                              (get-device-mac-address dev))))
  (if (not (empty nxt-list))
      (set nxt (first nxt-list))
      (progn
        (log _LOG_TAG "no viable NXTs found")
        (return)))

  (log _LOG_TAG (concat "connecting to: " (get-device-name nxt)))
  (if (not (connect-to-device nxt))
      (progn
        (log _LOG_TAG "failed to connect to "
                      (get-device-name nxt))
        (set nxt F)
        (return)
        ))

  (keep-nxt-alive nxt))


(defun set-power (port power)
   (if (not nxt)
      (progn 
        (log _LOG_TAG "Not connected to any nxt")
        (return)))

   (set-motor-power nxt port power)
   (log _LOG_TAG "set power of port: "
                 port 
                 " to: "
                 power))

(defun tear-down ()
  (disconnect-all-nxt))


(defun quit ()
  (disconnect-all-nxt)
  (stop-service)
  (set nxt F)
  )

(defun configure-switch (port)
  
  (if (not nxt)
      (progn 
        (log _LOG_TAG "Not connected to any nxt")
        (return)))
  
  
  (configure-sensor-port nxt port _SWITCH_SENSOR_TYPE))

(defun configure-sonar (port)
  (if (not nxt)
      (progn 
        (log _LOG_TAG "Not connected to any nxt")
        (return)))
  (configure-sensor-port nxt port _SONAR_SENSOR_TYPE)

  )


(defun get-sonar ()
  (if (not nxt)
      (progn 
        (log _LOG_TAG "Not connected to any nxt")
        (return)))
  (setq value (get-raw-sensor-value nxt _SONAR_PORT))
  (log _LOG_TAG  "sonar " value)
  value)

(defun get-touch ()
  (if (not nxt)
      (progn 
        (log _LOG_TAG "Not connected to any nxt")
        (return)))
  (setq value (get-boolean-sensor-value nxt _SWITCH_1_PORT))
  
  (log _LOG_TAG  "touch " value)
  value)



  (defun start-service ()
    (start-nxt-service)
    )

  (defun stop-service ()
    (stop-nxt-service)
    )



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
                                  :parent-align "center"
                                  :on-click (set-power 0 0))
                          (button "right"
                                  :padding 10
                                  :parent-align "right"))
                (horizontal-layout :width "match_parent"
                                   :height "wrap_content"
                                   :child-align "center"
                                   (button "back"
                                           :padding 10
                                           :on-click (set-power 0 20)))
                (button :width "match_parent"
                        :height "wrap_content"
                        :margin 10
                        "configure ports"
                        :text-size 20
                        :text-style "bold|italic"
                        :on-click (configure-ports))

                (relative :width "match_parent"
                          :height "wrap_content"
                          :margin-top 20
                          (button "get sonar"
                                  :parent-align "left"
                                  :padding 10
                                  :on-click (get-sonar))
                          (setq sonar-reading 
                                (text "0"
                                      :parent-align "right")))
                (relative :width "match_parent"
                          :height "wrap_content"
                          :margin-top 20
                          (button "get touch"
                                  :parent-align "left"
                                  :padding 10
                                  :on-click (get-touch))
                          (setq sonar-reading 
                                (text "0"
                                      :parent-align "right"))))
                
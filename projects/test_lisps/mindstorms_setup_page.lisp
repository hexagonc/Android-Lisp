(progn
  (global
    ; ******************************************
    ; Global Robot control functions
    ; ******************************************
    
        (setq current-nxt F)

        (if (not (var-exists-p "_MINDSTORMS_PRESENT_P"))
            (setq _MINDSTORMS_PRESENT_P F))

        (setq logical-motor-port-names ("left motor" "right motor" "extra"))

        (setq logical-sensor-port-names ("sonar sensor" "left touch" "right touch"))

        (setq key-motor-logical-to-physical-port-map "motor-logical-to-physical-port-map")

        (setq key-sensor-logical-to-physical-port-map "sensor-logical-to-physical-port-map")

        

        ; -=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<
        ;                       General Helper Functions
        ; -=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<

        (defun empty (string-list-or-map)
            (or (not string-list-or-map)
                (= 0 (length string-list-or-map))))


        ; =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ 
        ;                       Sensor Configuration
        ; 
        ; =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~ =(~   

        (setq sensor-type-configure-lambda-map
              (make-string-hashtable (list (list "sonar" 
                                                 (lambda (port device)
                                                    (configure-sonar device port)))
                                            (list "switch"
                                                  (lambda (port device)
                                                      (configure-switch device port))))))

        ; Maps device name to port to motor type
        (setq device-name-config-map
              (make-string-hashtable))

        (setq sensor-alias-map
              (make-string-hashtable (list (list "sonar" "sonar")
                                           (list "ultrasonic" "sonar")
                                           (list "touch" "touch")
                                           (list "switch" "touch"))))


        (defun convert-phrase-to-simplified-form (phrase word-map)
          (join (mapcar original
                        (split phrase " ")
                        (or (and word-map 
                                (gethash word-map
                                         original))
                            original))
                " "))


        (setq logical-sensor-port-type-map
              (make-string-hashtable (mapcar logical-sensor-port-name
                                             logical-sensor-port-names
                                             (cond 
                                                ((> (index-of (convert-phrase-to-simplified-form (lower logical-sensor-port-name)
                                                                                                 sensor-alias-map) 
                                                              "sonar")
                                                        -1) (list logical-sensor-port-name "sonar" ))
                                                ((> (index-of (convert-phrase-to-simplified-form (lower logical-sensor-port-name)
                                                                                                 sensor-alias-map) 
                                                              "touch")
                                                        -1) (list logical-sensor-port-name "switch"))))))


        (setq motor-port-map
              (make-string-hashtable))

        (setq sensor-port-map
              (make-string-hashtable))

        (setq current-configured-device F)

        (defun get-device-motor-logical-port-map (device)
            (gethash (gethash device-name-config-map
                              (get-device-name device))
                     key-motor-logical-to-physical-port-map))


        (defun get-on-motor-port-map-lambda (device logical-motor-port-name port-index)
          (lambda ()
              
              (defhash (get-device-motor-logical-port-map device)
                       logical-motor-port-name
                       port-index)

              ))


        (defun get-device-sensor-logical-port-map (device)
            (gethash (gethash device-name-config-map
                              (get-device-name device))
                     key-sensor-logical-to-physical-port-map))

        (defun get-on-sensor-port-map-lambda (device logical-sensor-port-name port-index)
            (lambda ()


                (defhash (get-device-sensor-logical-port-map device)
                         logical-sensor-port-name
                         port-index)
                (if (connected-to-device-p device)
                    (funcall (gethash sensor-type-configure-lambda-map
                                      (gethash logical-sensor-port-type-map
                                               logical-sensor-port-name))
                             port-index
                             device))))

        (setq configured-p F)


        (setq _SONAR_SENSOR_TYPE 1)
        (setq _SWITCH_SENSOR_TYPE 0)

        (setq _LOG_TAG "mindstorms-lisp")

        (defun tear-down ()
          (disconnect-all-nxt))


        (defun quit ()
          (disconnect-all-nxt)
          (stop-service)
          (set current-nxt F)
          (set _MINDSTORMS_PRESENT_P F)
          )

        (defun configure-switch (nxt port)
          
          (configure-sensor-port nxt port _SWITCH_SENSOR_TYPE))

        (defun configure-sonar (nxt port)
          (configure-sensor-port nxt port _SONAR_SENSOR_TYPE)

          )

          (defun start-service ()
            (start-nxt-service)
            )

          (defun stop-service ()
            (stop-nxt-service)
            )


        ; -=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<
        ;         Basic Setup
        ; -=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<



        (setq bluetooth-devices F)

        (setq bluetooth-device-info-map
              (make-string-hashtable))


        ; <~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~>
        ;                           NXT Motor and Sensor Interfaces
        ; <~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~><~>

        (defun set-power (nxt logical-port-name power)
          
           (set-motor-power nxt
                            (gethash (get-device-motor-logical-port-map nxt)
                                     logical-port-name) 
                            power)
           (log _LOG_TAG "set power of port: "
                         logical-port-name 
                         " to: "
                         power
                         " for "
                         (get-device-name nxt)))
           
        (defun get-sonar-value (nxt logical-port-name)
          
          (setq value (get-raw-sensor-value nxt 
                                            (gethash (get-device-sensor-logical-port-map nxt)
                                                     logical-port-name)))
          (log _LOG_TAG  logical-port-name 
                          " is " 
                          value 
                          " for device: " 
                          (get-device-name nxt))
          value)

        (defun get-touch (nxt logical-port-name)
          
          (setq value (get-boolean-sensor-value nxt (gethash (get-device-sensor-logical-port-map nxt)
                                                             logical-port-name)))
          
          (log _LOG_TAG  logical-port-name 
                         " is "
                         (if value "true" "false")
                         " for device: "
                         (get-device-name nxt))
          value)

        (defun move-forward (nxt power)
          (if (or (not nxt)
                  (not (connected-to-device-p nxt)))
              (progn 
                  (if (not nxt)
                      (log _LOG_TAG
                           "No NXT device defined in order to move forward")
                      (log _LOG_TAG
                           "can't move forward with device: "
                           (get-device-name nxt)
                           " until you connect to it"))
                   (return F)))
          (set-power nxt
                     "left motor"
                     (or power
                         20))
          (set-power nxt
                     "right motor"
                     (or power
                         20)))

        (defun rotate-left (nxt power)
          (if (or (not nxt)
                  (not (connected-to-device-p nxt)))
              (progn 
                  (if (not nxt)
                      (log _LOG_TAG
                           "No NXT device defined in order to rotate left")
                      (log _LOG_TAG
                           "can't move rotate left with device: "
                           (get-device-name nxt)
                           " until you connect to it"))
                   (return F)))
          (set-power nxt
                     "left motor"
                     (-  (or power
                             20)))
          (set-power nxt
                     "right motor"
                     (or power
                         20)))

        (defun stop-moving (nxt)
          (if (or (not nxt)
                  (not (connected-to-device-p nxt)))
              (progn 
                  (if (not nxt)
                      (log _LOG_TAG
                           "No NXT device defined in order to stop")
                      (log _LOG_TAG
                           "can't stop with device: "
                           (get-device-name nxt)
                           " until you connect to it"))
                   (return F)))
          (set-power nxt
                     "left motor"
                     0)
          (set-power nxt
                     "right motor"
                     0))

        (defun rotate-right (nxt power)
          (if (or (not nxt)
                  (not (connected-to-device-p nxt)))
              (progn 
                  (if (not nxt)
                      (log _LOG_TAG
                           "No NXT device defined in order to rotate right")
                      (log _LOG_TAG
                           "can't move rotate right with device: "
                           (get-device-name nxt)
                           " until you connect to it"))
                   (return F)))
          (set-power nxt
                     "left motor"
                     (or power
                         20))
          (set-power nxt
                     "right motor"
                     (- (or power
                            20))))

        (defun move-backward (nxt power)
          (if (or (not nxt)
                  (not (connected-to-device-p nxt)))
              (progn 
                  (if (not nxt)
                      (log _LOG_TAG
                           "No NXT device defined in order to move backwards")
                      (log _LOG_TAG
                           "can't move backwards with device: "
                           (get-device-name nxt)
                           " until you connect to it"))
                   (return F)))
          (set-power nxt
                     "left motor"
                     (- (or power
                            20)))
          (set-power nxt
                     "right motor"
                     (- (or power
                            20))))


        ; configure the 
        (defun basic-setup ()
          (keep-nxt-alive current-nxt)
          (set _MINDSTORMS_PRESENT_P 1))

        (defun setup-standard-rover ()
          (defhash (get-device-sensor-logical-port-map current-nxt)
                   "right touch"
                   2)

          (defhash (get-device-sensor-logical-port-map current-nxt)
                   "left touch"
                   1)

          (defhash (get-device-sensor-logical-port-map current-nxt)
                   "rear touch"
                   3)

          (defhash (get-device-sensor-logical-port-map current-nxt)
                   "sonar"
                   0)

          (configure-sonar current-nxt 0)

          (configure-switch current-nxt 3)
          (configure-switch current-nxt 2)
          (configure-switch current-nxt 1)

          (defhash (get-device-motor-logical-port-map current-nxt)
                     "left motor"
                     0)
          (defhash (get-device-motor-logical-port-map current-nxt)
                     "right motor"
                     2)
          (basic-setup)) 
    
        ; /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ 
        ;         Basic Speech Configuration
        ; /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ /^o_o^\ 

        (set _CONT F)

        (if SPEECH_AVAILABLE_P
          (progn
            (setq  tts_availability "TTS ready")
            (setq tts_color "green"))
          (progn
            (setq tts_availability "TTS not not available")
            (setq tts_color "red")))

        (if ASR_AVAILABLE_P
          (progn
            (setq asr_availability "Can do speech recognition")
            (setq asr_color "green")
            (register-asr-listener (lambda (speech status error-message)
                                     (if error-message
                                         (log-error "errors processing speech" 
                                                    (concat "Status: " status " Message: " error-message))
                                         (log-info "new speech result" 
                                               (concat "onNewSpeech called with " speech)))
                                     
                                     (if speech
                                         (onNewSpeech speech))
                                     (if (and SPEECH_AVAILABLE_P error-message (not (already-speaking-p)))
                                         (tts error-message)))))
          (progn
            (setq asr_availability "Speech recognition not available")
            (setq asr_color "red")))


        (defun already-listening-p ()
             (equals ASR_STATUS
                   "ASR_STARTED"))


        (defun already-speaking-p ()
          (equals TTS_STATUS
                "TTS_STARTED"))

        (setq _PREV_HANDLER F)

        (setq _EXECUTE_SPEECH_FROM_INPUT_P 1)

        (set _NEXT_SPEECH_COGJECT F)

        (setq _TOKENIZED_MISUNDERSTOOD_COMMAND F)

        (defun get-speech (prompt)
          (if prompt
              (tts prompt))
            (set _PREV_HANDLER
                 _SPEECH_HANDLER_LAMBDA)

            (setq speech-result F)

            (set _SPEECH_HANDLER_LAMBDA
                (lambda (speech)
                  (set speech-result speech)
                  (set _SPEECH_HANDLER_LAMBDA _PREV_HANDLER)
                  speech-result))
            (unless speech-result)
            speech-result)

        (defun onNewSpeech (speech)
            (funcall _SPEECH_HANDLER_LAMBDA speech))

        ; Override this
        (setq _SPEECH_HANDLER_LAMBDA 
            (lambda (speech)
                F
                ))


        )



    ; ******************************************
    ; Load the Mindstorms configuration UI
    ; ******************************************

    ; -=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<
    ;                         Visual Style constants
    ; -=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<

    (setq bluetooth-status-group-color "#EBF1F9")
    (setq bluetooth-status-group-border-color "#013470")
    (setq bluetooth-status-group-border-width 3)
    (setq bluetooth-status-group-title-size 17)
    (setq bluetooth-status-group-text-size 14)
    (setq bluetooth-status-group-text-color "#013470")

    (setq bluetooth-devices-height 100)
    (setq bluetooth-devices-divider-color "#398523")


    (setq motor-port-configuration-text "Motor Port Configuration:")

    (setq sensor-port-configuration-text "Sensor Port Configuration:")

    (setq key-sensor-logical-port-spinner-view-map "sensor port spinner")

    (setq key-motor-logical-port-spinner-view-map "motor port spinner")

    (defun get-device-motor-spinner (device logical-port-name)
      (gethash (gethash (gethash device-name-config-map
                                 (get-device-name device))
                        key-motor-logical-port-spinner-view-map)
               logical-port-name))

    (defun get-device-sensor-spinner (device logical-port-name)
      (gethash (gethash (gethash device-name-config-map
                                 (get-device-name device))
                        key-sensor-logical-port-spinner-view-map)
               logical-port-name))

    (setq bluetooth-device-connect-button-map
      (make-string-hashtable ))

    (defun create-device-connect-button (device)
      (log-info "create-device-connect-button" (get-device-name device))
      (defhash bluetooth-device-connect-button-map
               (get-device-name device)
               (button (if (connected-to-device-p device)
                            "disconnect"
                            "connect")
                       :on-click (if (not (empty bluetooth-devices))
                                     (do-on-click-for-connection-button device))
                       :width "wrap_content"
                       :height "wrap_content")))



    (defun configure-enabled-text-view (text state-title-list enabled-p)
      (if enabled-p
          (update-parameters (set-text text
                                       (first state-title-list))
                             :text-color "green"
                             1)
            
          
          (update-parameters (set-text text
                                       (second state-title-list))
                             :text-color "red"
                             1)))


    (defun create-motor-port-spinner-dropdown-view (physical-port-name)
      (text physical-port-name
            :width "match_parent"
            :height "wrap_content"))

    (defun create-motor-port-spinner-selected-view (physical-port-name)
      (text physical-port-name
            :width "match_parent"
            :height "wrap_content"))

    (defun create-sensor-port-spinner-dropdown-view (physical-port-name)
      (text physical-port-name
            :width "match_parent"
            :height "wrap_content"))

    (defun create-sensor-port-spinner-selected-view (physical-port-name)
      (text physical-port-name
            :width "match_parent"
            :height "wrap_content"))

    (defun on-paired-to-device (device)
      (log-info (concat "Paired to: " (get-device-name device)))
      ; Define the label that show when the device is connected
      (defhash bluetooth-device-info-map
                (get-device-name device)
                (text (get-device-name device)))

      ; Define the button for connecting and disconnecting from the a device
      (create-device-connect-button device)

      ; Map the device status changed listener
      (set-connection-status-listener device
                                      (create-connection-change-listener device)
                                      1)

      ; Device the device port configuration
      (defhash device-name-config-map
               (get-device-name device)
               (make-string-hashtable (list (list key-motor-logical-to-physical-port-map
                                                  (make-string-hashtable))
                                            (list key-sensor-logical-to-physical-port-map
                                                  (make-string-hashtable))
                                            (list key-motor-logical-port-spinner-view-map
                                                  (make-string-hashtable (mapcar logical-port-name
                                                                                 logical-motor-port-names
                                                                                 (list logical-port-name
                                                                                       (spinner :width "match_parent"
                                                                                                :height "wrap_content"
                                                                                                (mapcar physical-port-index
                                                                                                        (0 1 2)
                                                                                                        (list (create-motor-port-spinner-dropdown-view (string physical-port-index))
                                                                                                              (create-motor-port-spinner-selected-view (string physical-port-index))
                                                                                                              (get-on-motor-port-map-lambda device logical-port-name physical-port-index))))))))



                                            (list key-sensor-logical-port-spinner-view-map
                                                  (make-string-hashtable (mapcar logical-port-name
                                                                                 logical-sensor-port-names
                                                                                 (list logical-port-name
                                                                                       (spinner :width "match_parent"
                                                                                                :height "wrap_content"
                                                                                                (mapcar physical-port-index
                                                                                                        (0 1 2 3)
                                                                                                        (list (create-sensor-port-spinner-dropdown-view (string physical-port-index))
                                                                                                              (create-sensor-port-spinner-selected-view (string physical-port-index))
                                                                                                              (get-on-sensor-port-map-lambda device logical-port-name physical-port-index))))))))
                                            
                                            )))
      )

  ; This has to be a function because the lambda function is being called in a 
  ; for loop so the device-name would be varying
  (defun create-connection-change-listener (device)
     (lambda (connected-p)
              (configure-enabled-text-view (gethash bluetooth-device-info-map
                                                    (get-device-name device))
                                           (list (get-device-name device) (get-device-name device))
                                           connected-p)
              (if connected-p
                  (keep-nxt-alive device))

              (set-text (gethash bluetooth-device-connect-button-map
                                 (get-device-name device))
                        (if connected-p
                            "disconnect"
                            "connect"))))

  (defun init ()
      (if (not (nxt-service-running-p))
          (progn
              (if (start-nxt-service)
                  (unless (nxt-service-running-p)
                          (sleep-milli 10))
                  (progn
                    (evaluate-foreground F
                        (show-long-toast "Cannot start nxt service, probably because no bluetooth adapter available"))
                    
                    (return F)))))

      (set bluetooth-devices
           (and (bluetooth-adapter-on-p)
                (find dev 
                      (get-paired-devices)
                      (> (index-of (upper (get-device-name dev))
                                   "NXT")
                         -1))))

      (if bluetooth-devices
          (for dev 
               bluetooth-devices
               (log _LOG_TAG "paired devices: " 
                            (mapcar dev 
                                    bluetooth-devices
                                    (list (get-device-name dev) 
                                          (get-device-mac-address dev))))
               (on-paired-to-device dev))))

    (init)

    ; -*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-
    ;                   Configuration Tabs
    ; -*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-*)(*-


    (setq ports-configured-p F)

    (setq port-config-text-size 12)

    ; - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- 
    ;                 NXT Speech Page
    ; - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- 

    (setq asr-text-view
        (text :width "match_parent"
            :height "wrap_content"))

    (setq speech-interface-view 
          (scrollview 
                (vertical-layout :width "match_parent"
                           :height "match_parent"
                           (text tts_availability
                               :text-color tts_color
                               :text-style "bold")
                           (text asr_availability
                               :text-color asr_color
                               :text-style "bold")


                           (set-enabled (button "start listening"
                                                :on-click (if (and ASR_AVAILABLE_P
                                                                   (not (already-listening-p)))
                                                                (start-speech-recognition)))
                                        ASR_AVAILABLE_P)
                           (text "Transcription:"
                                 :text-style "bold")
                           asr-text-view
                           (setq speech-edit
                               (edit :width "match_parent"
                                     :height "wrap_content"))
                           (button "execute"
                                   :on-click (if (not (empty (setq command
                                                                   (trim (get-text asr-text-view)))))
                                                 (if (setq result
                                                           (test-speech-evaluation command))
                                                     (set _NEXT_SPEECH_COGJECT result))))
                           (set-enabled (button "speak message"
                                                :on-click (if (and SPEECH_AVAILABLE_P
                                                                    (not (already-speaking-p))
                                                                   (> (length (trim (setq s
                                                                                          (get-text speech-edit))))
                                                                    0))
                                                              (tts s)))
                                        SPEECH_AVAILABLE_P)
                           (button "start dialog engine"
                                   :on-click (progn 
                                                (cancel-background-actions)
                                                (evaluate-background (run-full-dialog))

                                                ))
                           (button "stop dialog engine"
                                   :on-click (progn 
                                                (set _STOP_DIALOG 1)
                                                (stop-all-cogjects _GLOBAL_AWARENESS_MAP)
                                                ))
                           (button "bluetooth debug"
                                   :on-click (log-info "bluetooth devices:" 
                                                       (if (not (empty bluetooth-devices))
                                                           (string (mapcar device bluetooth-devices (get-device-name device)))))))))



    ; - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- 
    ;                 NXT Simple Control page
    ; - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- - -- 

    (setq nxt-status-text
          (text))

    (defun update-direction-control-status-text ()
         (set-text nxt-status-text 
                   (if (and current-nxt
                            (connected-to-device-p current-nxt))
                       (concat "Controlling: "
                               (get-device-name current-nxt))
                       (if current-nxt
                           (concat "Not connected to: "
                                   (get-device-name current-nxt))
                           "No NXT selected"))))


    (setq directional-control-view
          (vertical-layout :width "match_parent"
                           :height "match_parent"
                           :background-color "white"
                           (update-direction-control-status-text)
                           (horizontal-layout :width "match_parent"
                                       :height "wrap_content"
                                       :child-align "center"
                                       (button "forward"
                                               :padding 10
                                               :on-click (move-forward current-nxt 20)))
                          (relative :width "match_parent"
                                    :height "wrap_content"
                                    (button "left"
                                            :padding 10
                                            :parent-align "left"
                                            :on-click (rotate-left current-nxt 20))
                                    (button "stop"
                                            :padding 10
                                            :parent-align "center"
                                            :on-click (stop-moving current-nxt))
                                    (button "right"
                                            :padding 10
                                            :parent-align "right"
                                            :on-click (rotate-right current-nxt 20)))
                          (horizontal-layout :width "match_parent"
                                             :height "wrap_content"
                                             :child-align "center"
                                             (button "back"
                                                     :padding 10
                                                     :on-click (move-backward current-nxt 20)))

                           ))

    (setq content-view
          (scrollview :width "match_parent"
                       :height "match_parent"
                       speech-interface-view))

    (setq tab-view-map
          (make-string-hashtable (list (list "speech" speech-interface-view)
                                       (list "direction-control" directional-control-view))))

    (defun configure-selected-tab (text selected)
      (update-parameters text
                         :background (if selected
                                        (create-background :foreground-color "white"
                                                           :border-color "green"
                                                           :border-width 2)
                                        (create-shadow-background :shadow-width 4
                                                           :shadow-color "#D8D8D8"
                                                           :foreground-color "white"
                                                           :shadow-angle -45)) 
                         1))

    (setq selected-tab-label "speech")

    (defun set-shadow-back (view)
          (update-parameters view
                             :text-color "#D8D8D8"
                             :text-style "normal"
                             :background (create-shadow-background :shadow-width 4
                                                           :shadow-color "#D8D8D8"
                                                           :foreground-color "white"
                                                           :shadow-angle -45)
                             1))

    (defun set-border-back (view)
          (update-parameters view
                             :text-color "black"
                             :text-style "bold"
                             :background (create-background :foreground-color "white"
                                                            :border-color "green"
                                                            :border-width 2)
                             1))



    (defun do-on-click-for-connection-button (device)
      (if (connected-to-device-p device)
          (progn
            (disconnect-from-device device)
            (set current-nxt F))
          (progn
            (set current-nxt device)
            (connect-to-device device)
            (basic-setup))))


    ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;     Tab Configuration
    ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    (setq tab-label-map
          (make-string-hashtable (list (list "speech" (text "Speech"
                                                               :padding 15
                                                               :text-color "black"
                                                               :text-style "bold"
                                                               :background (create-background :foreground-color "white"
                                                                                          :border-color "green"
                                                                                          :border-width 2)
                                                               :on-click (progn
                                                                            (set-border-back (gethash tab-label-map "speech"))
                                                                            (set-shadow-back (gethash tab-label-map "direction-control"))
                                                                            (remove-all-views content-view)
                                                                            (add-view content-view
                                                                                      (gethash tab-view-map
                                                                                               "speech")))
                                                               ))
                                       (list "direction-control" (text "direction-control"
                                                                       :padding 15
                                                                       :text-color "#D8D8D8"
                                                                       :text-style "normal"
                                                                       :on-click (progn
                                                                                    (update-direction-control-status-text)
                                                                                    (set-border-back (gethash tab-label-map "direction-control"))
                                                                                    (set-shadow-back (gethash tab-label-map "speech"))
                                                                                    (remove-all-views content-view)
                                                                                    (add-view content-view
                                                                                              (gethash tab-view-map
                                                                                                       "direction-control")))
                                                                       :background (create-shadow-background :shadow-width 4
                                                                                                             :shadow-color "#D8D8D8"
                                                                                                             :foreground-color "white"
                                                                                                             :shadow-angle -45))))))




    (setq help-text
    "First make sure you enable your bluetooth adapter.  Then you need to make sure 
    that you have paired NXTs.  In order to connect to an NXT, its bluetooth device 
    name needs to have 'NXT' in it somewhere.  Once you connect to an NXT, you can 
    configure the sensor and motor ports.  Then you can use the directional buttons
    to control a robot with differential drive.")

    (defun show-help-dialog ()
      (setq help-dialog
            (dialog :dialog-title "Help"
                    :cancel-text "Ok"
                    (scrollview :width "match_parent"
                                :padding 20
                                :height 200
                                 (text help-text
                                       :width "match_parent"
                                       :height "match_parent")))))

    ; >~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~-
    ;                 Main GUI Code
    ; >~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~->~-
    (init)

    (defun create-bluetooth-device-item (device i)
      (log-info "Creating bluetooth device item" (get-device-name device))
      (vertical-layout :width "match_parent"
                       :height "wrap_content"
                       :background (create-background :foreground-color "#EBF1F9"
                                                      :border-color bluetooth-devices-divider-color
                                                      :border-top-width (if (> i 0) 1 0)
                                                      :border-bottom-width 0
                                                      :border-left-width 0
                                                      :border-right-width 0)
                       (gethash bluetooth-device-info-map
                                (get-device-name device))
                       (create-device-connect-button device)))

    (setq VIEW_PROXY
          (vertical-layout :width "match_parent"
                     :height "match_parent"
                     :padding 10
                     (text "?"
                           :padding 14
                           :text-size 20
                           :text-sytle "bold"
                           :text-color "#398523" 
                           :parent-align "right"
                           :on-click (show-help-dialog))
                     (vertical-layout :width "match_parent"
                                      :height "wrap_content"
                                      :padding 10
                                      :background (create-background :foreground-color bluetooth-status-group-color
                                                                     :border-color bluetooth-status-group-border-color
                                                                     :border-width bluetooth-status-group-border-width)
                                      (text "Bluetooth Status:"
                                            :parent-align "center"
                                            :text-size bluetooth-status-group-title-size
                                            :text-style "bold"
                                            :text-color bluetooth-status-group-text-color)
                                      (horizontal-layout :width "match_parent"
                                                          :height "wrap_content"
                                                          (text "Bluetooth adapter:"
                                                                :width "50%"
                                                                :heigth "wrap_content"
                                                                :child-align "left"
                                                                :text-size bluetooth-status-group-text-size
                                                                :text-color bluetooth-status-group-text-color)
                                                          (configure-enabled-text-view (text "Present"
                                                                                             :width "50%"
                                                                                             :heigth "wrap_content"
                                                                                             :child-align "right")
                                                                                       ("Enabled" "Disabled")
                                                                                       (bluetooth-adapter-on-p)))                                  
                                      (horizontal-layout :width "match_parent"
                                                         :height "wrap_content"
                                                         (text "Paired NXTs:"
                                                                :width "50%"
                                                                :heigth "wrap_content"
                                                                :child-align "left"
                                                                :parent-align "top"
                                                                :text-size bluetooth-status-group-text-size
                                                                :text-color bluetooth-status-group-text-color)
                                                         (scrollview :width "match_parent"
                                                                      :height bluetooth-devices-height
                                                                 (vertical-layout :width "match_parent"
                                                                          :height "wrap_content"
                                                                          (if (not (empty bluetooth-devices))
                                                                              (mapcar (device i)
                                                                                      bluetooth-devices
                                                                                      (create-bluetooth-device-item device i)))))))
                    (horizontal-layout :width "match_parent"
                                       :height "wrap_content"
                                       (mapcar (tab-label i)
                                               ("speech" "direction-control")
                                               (gethash tab-label-map
                                                        tab-label)))
                    content-view))
  (set-top-view VIEW_PROXY)

  

  )

(comment
  ; bluetooth debuggign

  (if bluetooth-devices
          (for dev 
               bluetooth-devices
               (log _LOG_TAG "paired devices: " 
                            (mapcar dev 
                                    bluetooth-devices
                                    (list (get-device-name dev) 
                                          (get-device-mac-address dev))))
               (on-paired-to-device dev)))

  (setq dev
        (first bluetooth-devices))





  )


                
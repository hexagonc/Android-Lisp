; /__//__/__//__/__//__/__//__/__//__/__//__/__//__/__//__/__//__
;           Mindstorms Robot
;
; Examples of simple control procedures for differential drive rover robots


; Default rover port config is:
; left moter -> 0
; right motor -> 2
; sonar -> 0
; left sensor -> 2
; right sensor -> 3

; /__//__/__//__/__//__/__//__/__//__/__//__/__//__/__//__/__//__

; Check if mindstorms_samples was executed.  If mindstorms_samples was not called then bluetooth-devices will be undefined
; and throw an exception

(defmacro catch (exp on-exception)
    (setq signal (unique-id))

    `(signal-block (,signal)
        ((,signal) (try ,exp))
        ,on-exception))

(setq mindstorms-sampled-ran 
      (catch (or bluetooth-devices 1)
             ((F) F)))

(setq nxts-to-use
      (and mindstorms-sampled-ran
           (connected-to-device-p current-nxt)))

; global variable indicating that a robot procedure should continue executing
(setq continue F)

(if nxts-to-use
    (stop-moving current-nxt))

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


; -=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<
;                       Helper Functions and Macros
; -=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<-=<

(defun empty (string-list-or-map)
    (or (not string-list-or-map)
        (= 0 (length string-list-or-map))))

(defun non-empty (string-list-or-map)
    (and string-list-or-map
         (< 0 (length string-list-or-map))
         string-list-or-map))




(defun average (x y)
    (/ (+ x y) 2))


(defun get-logical-motor-tach (logical-motor-port-name)
    (get-motor-tach current-nxt
                    (gethash (get-device-motor-logical-port-map current-nxt)
                             logical-motor-port-name)))





(setq error-message "You must select and configure your NXT before running this robot control program.  Run \"lego mindstorms sample\" sample program.")

(setq message-text 
      (set-enabled (edit :width "match_parent"
                         :height "20%"
                         :text-color "green"
                         :background-color "black"
                         (if mindstorms-sampled-ran
                             (if (not nxts-to-use)
                                "You must connect to an NXT in order to run the sample robot code here.  Run the sample program \"lego mindstorms sample\" in order to configure an nxt robot."
                                "NXT robot control tests")
                            "You must run the sample program \"lego mindstorms sample\" in order to configure an nxt robot."))
                    F))

(setq check-nxt-configured 
      '(if (not nxts-to-use)
            (progn
                (dialog :dialog-title "No NXT Selected!"
                        :cancel-text "Ok"
                        :dialog-message error-message)
                (return))))

(defun signal-if (condition signal-key signal-data)
    (if condition
        (signal signal-key signal-data)))


(defun create-black-fancy-button (button)
    (update-parameters button
                       :text-color "#F8E71C"
                       :text-style "bold"
                       :padding 4
                       :background (create-background :foreground-color "black"
                                                        :border-color "#F8E71C"
                                                        :corner-radius 3
                                                        :on-pressed-drawable (create-background :foreground-color "#8D8D8D"
                                                                                                :border-color "#F8E71C"
                                                                                                :corner-radius 3)))



    )

(defun make-forward-robot-runner-view ()

    (setq movement-speed 20)
    
    (setq previous-speed movement-speed)

    (setq default-collision-distance "20")

    (defun get-collision-distance ()
        (integer (or (non-empty (trim (get-text stop-distance-view)))
                     default-collision-distance)))

    (setq check-collision-code
          '(or (signal-if (get-touch current-nxt
                                     "left touch")
                           "LEFT-COLLISION")
                (signal-if (get-touch current-nxt
                                          "right touch")
                           "RIGHT-COLLISION")
                (signal-if (< (setq current-distance
                                    (get-sonar-value current-nxt
                                                         "sonar sensor"))
                              (get-collision-distance))
                           "FORWARD-OBJECT-TOO-CLOSE")))

    (defun start-forward-test ()
        (eval check-nxt-configured)
        
        (setq finished F)

        (evaluate-background (progn
                                (setq initial-tach
                                      (average (get-logical-motor-tach "left motor")
                                               (get-logical-motor-tach "right motor")))
                                (setq state "INITIAL-CHECK-DISTANCE")
                                (setq stop-distance
                                      (integer (get-text stop-distance-view)))
                                (unless (not continue)
                                    (signal-block state
                                        (("CHECK-DISTANCE") (setq stop-distance
                                                                  (integer (get-text stop-distance-view)))
                                                            (eval check-collision-code)
                                                            (evaluate-foreground (set-text travel-distance-view
                                                                                           (string (abs (- (average (get-logical-motor-tach "left motor")
                                                                                                                    (gget-logical-motor-tach "right motor"))
                                                                                                           initial-tach))))))
                                        (("INITIAL-CHECK-DISTANCE") (eval check-collision-code)
                                                                    (evaluate-foreground (set-text travel-distance-view
                                                                                                   (string (abs (- (average (get-logical-motor-tach "left motor")
                                                                                                                            (get-logical-motor-tach "right motor"))
                                                                                                                   initial-tach)))))
                                                                    (move-forward current-nxt
                                                                                  movement-speed)

                                                                    (setq state "CHECK-DISTANCE"))
                                        (("LEFT-COLLISION") (if SPEECH_AVAILABLE_P
                                                                (tts "collision left"))
                                                            (signal "EXIT"))
                                        (("RIGHT-COLLISION") (if SPEECH_AVAILABLE_P
                                                                (tts "collision right"))
                                                             (signal "EXIT"))
                                        (("FORWARD-OBJECT-TOO-CLOSE") (if SPEECH_AVAILABLE_P
                                                                          (tts "I've detected an object"))
                                                                      (signal "EXIT"))

                                        (("EXIT") (stop-moving current-nxt)
                                                  (set continue F))))
                                (stop-moving current-nxt)
                                (set finished
                                     (abs (- (average (get-logical-motor-tach "left motor")
                                                      (get-logical-motor-tach "right motor"))
                                             initial-tach)))))
        
        finished)

    (defun stop-forward-test ()
        (set continue F))

    (vertical-layout :width "match_parent"
                     :height "wrap_content"
                     :padding 3
                     :background (create-background :foreground-color "black"
                                                    :border-color "#F8E71C"
                                                    :border-width 2
                                                    :corner-radius 3)
                     (horizontal-layout :width "match_parent"
                                        :height "wrap_content"
                                        (create-black-fancy-button (button "start"
                                                                           :width "wrap_content"
                                                                           :height "wrap_content"
                                                                           :on-click (start-forward-test)))
                                      (create-black-fancy-button (button "stop"
                                                                         :width "wrap_content"
                                                                         :height "wrap_content"
                                                                         :on-click (stop-forward-test)))
                                      (vertical-layout :width "wrap_content"
                                                       :height "wrap_content"
                                                       (text "Stop Distance:"
                                                             :text-color "#F8E71C"
                                                             :text-style "bold")
                                                       (setq stop-distance-view
                                                             (edit :width "wrap_content"
                                                                   :height "wrap_content"
                                                                   "25")))
                                      (vertical-layout :width "wrap_content"
                                                       :height "wrap_content"
                                                       (text "Current Distance:"
                                                             :text-color "#F8E71C"
                                                             :text-style "bold")
                                                       (setq current-distance-view
                                                             (text :width "wrap_content"
                                                                   :height "wrap_content"
                                                                   :text-color "#F8E71C"))))
                    (vertical-layout :width "wrap_content"
                                     :height "wrap_content"
                                     (text "Distance Traveled in Rotations:"
                                           :text-color "#F8E71C"
                                           :text-style "bold")
                                     (setq travel-distance-view
                                           (text "0"
                                                 :text-color "#F8E71C"
                                                 :width "wrap_content"
                                                 :height "wrap_content")))))


(vertical-layout :width "match_parent"
                 :height "match_parent"
                 message-text
                 (scrollview :width "match_parent"
                              :height "80%"
                              :background-color "#398523"
                              :padding 7
                              (make-forward-robot-runner-view)))




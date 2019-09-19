(progn
  (setq _INSTRUMENTATION_USE_NEW_MAP 1)

  (setq _STOP_ON_MEMORY_INCONSISTENCY F)

  (setq _ENABLE_INTERNAL_REPORTING_P 1)

  (setq _INSTRUMENTATION_PLAN_WITH_MODEL 20)

  (setq _EXTRAP_DEBUG F)

  (setq _SPEECH_DEBUG_ENABLE 1)

  (setq DECAY-ALL-FEATURES-P 1)

  (setq UTILITY-DECAY-FRACTION 0.99)

  (setq MINIMUM-UTILITY 0.1)


  (setq UNNECESSARY-FEAR-VALUE-REDUNCTION-FRACTION 0.75)

  (setq UNNECESSARY-FEAR-UTILITY-REDUNCTION-FRACTION 0.75)

  (setq FEAR-ACCURACY-VALUE-BONUS 0.25)

  (setq MISPREDICTING-REWARD-MODEL-VALUE-REDUNCTION-FRACTION 0.8)

  (setq MISPREDICTING-REWARD-MODEL-UTILITY-REDUNCTION-FRACTION 0.8)

  (setq REWARD-ACCURACY-VALUE-BONUS 0.2)


  (setq _DEBUG F)

  (setq _DEBUG_2 F)

  (setq _DEBUG_3 1)

  (setq _DEBUG_4 F)

  (setq _DEBUG_5 F)

  (setq _DEBUG_TIME 1)

  (setq _USE_MATCH_COUNT_P 1)
  
  (setq _DEBUG_HALT_1 F)

  (setq _GROUP_TEST_PATTERN_MAP
        (make-string-hashtable))

  (setq _RECOGNITION_NAME_MAP
        (make-string-hashtable))

  (setq _NUM_ACTION_TYPE_KEY "NUM-ACTION-TYPES")
  (setq _ULTRASONIC_WIDTH_KEY "ULTRASONIC-VECTOR-WIDTH")
  (setq _ACTION_REP_COUNT_WIDTH_KEY "ACTION-REP-VECTOR-WIDTH")
  (setq _UNSIGNED_REWARD_WIDTH_KEY "REWARD-VECTOR-WIDTH")
  (setq _ACTION_NAMES_KEY "ACTION-NAMES")
  (setq _DATA_VIEWER_LAMBDA_KEY "VIEWER")

  (setq last-speak-time (time))

  (setq speech-delay 5000)

  (setq _COMPLETE_FEATURE_STATE_MAP
        (make-string-hashtable (list (list "MATCHING" 1)
                                     (list "NON_MATCHING" 1))))

  (defun is-feature-building (model)
      (gethash _COMPLETE_FEATURE_STATE_MAP
               (feature-get-state model)))


  (defun speak (phrase)
    (if (and _SPEECH_DEBUG_ENABLE (> (time) (+ speech-delay last-speak-time)))
        (progn
            (set last-speak-time (time))
            (tts phrase))
        (log-ui "speech" "Skipped tts: " phrase (if _SPEECH_DEBUG_ENABLE " due to already speaking" "because speech debugging is disabled"))))


  ; <> 
  ; Returns true if the feature-model's utility is less than delete-threshold, indicating it
  ; can be deleted
  (defun decay-feature-model-utility (feature-model delete-threshold)
    
    (set-feature-base-utility feature-model
                              (setq utility
                                   (* (get-feature-utility feature-model)
                                      (setq decay (get-effective-decay-fraction feature-model)))))

    (log-ui "decay" "Reducing " feature-model "'s utility to " (integer (* 100 decay)) "% minimum utility: " delete-threshold ". New value: " utility)
    ;(println (concat "Reducing " (string feature-model) "'s utility to " (integer (* 100 decay-fraction)) "% minimum utility: " delete-threshold ". New value: " new-v))
    (and delete-threshold
         (< utility
            delete-threshold)))


  (setq _FEATURE_META_KEY_UTILITY "UTILITY")
  (setq _FEATURE_META_KEY_DEFAULT_DECAY "DEFAULT-DECAY-FRACTION")
  (setq _FEATURE_META_KEY_FAILURE_COUNT "FAILURE-COUNT")
  (setq _MAX_DECAY 32)

  ; ><
  (defun get-default-feature-meta-data (group-name)
      (make-string-hashtable (list (list _FEATURE_META_KEY_UTILITY 0)
                                   (list _FEATURE_META_KEY_DEFAULT_DECAY UTILITY-DECAY-FRACTION)
                                   (list _FEATURE_META_KEY_FAILURE_COUNT 0))))

  ; ><
  (defun set-feature-failure-count (feature-model new-count)
      (defhash (get-feature-meta-map feature-model)
               _FEATURE_META_KEY_FAILURE_COUNT
               new-count))

  ; ><
  (defun get-feature-failure-count (feature-model)
      (gethash (get-feature-meta-map feature-model) _FEATURE_META_KEY_FAILURE_COUNT))

  ; ><
  (defun increment-feature-failure-count (feature-model)
      (set-feature-failure-count feature-model (+ 1 (get-feature-failure-count feature-model))))

  ; ><
  (defun reset-feature-failure-count (feature-model)
    (set-feature-failure-count feature-model 0))

  ; ><
  (defun get-feature-meta-map (feature-model)
      (setq meta (feature-get-custom-meta-data feature-model))
      (if (not meta)
          (progn
            (feature-set-custom-meta-data feature-model (setq map (get-default-feature-meta-data)))
            map)
          (if (not (is-string-hashtable meta))
              (progn 
                  (feature-set-custom-meta-data feature-model (setq map (defhash* (get-default-feature-meta-data) _FEATURE_META_KEY_UTILITY meta)))
                  map)
              meta)))


  ; ><
  ; effective decay fracton is d^(2^failure-count)
  (defun get-effective-decay-fraction (feature-model)
    (setq decay-fraction
          (gethash (get-feature-meta-map feature-model) _FEATURE_META_KEY_DEFAULT_DECAY))
    (setq failure-count
          (gethash (get-feature-meta-map feature-model) _FEATURE_META_KEY_FAILURE_COUNT))

    (setq power
          (pow 2 (min _MAX_DECAY
                      failure-count)))
    (pow decay-fraction power))


  ; ><
  (defun get-feature-utility (feature-model)
      (gethash (get-feature-meta-map feature-model) _FEATURE_META_KEY_UTILITY))



  ; ><
  (defun set-feature-base-utility (feature-model utility)
      (defhash (get-feature-meta-map feature-model)
               _FEATURE_META_KEY_UTILITY
               utility))

  ; Helper functions
  (defun remove-string-duplicates (list)
    (setq map (make-string-hashtable))
    (setq nlist ())
    (for x
         list
         nlist
         (if (not (gethash map x))
             (progn
                (defhash map x 1)
                (set nlist (append-item nlist x))))))


  (defun sample-n-strings (list n require-n-distinct-p)
    (setq map (make-string-hashtable))
    (setq out ())
    (for (x i)
         (random-perm list)
         out
         (if (or (< i n)
                 (and require-n-distinct-p
                      (< (length map) n)))
             (progn
                (defhash map x 1)
                (set out (append-item out x)))
             (return out))))


  (defun remove-key-from-list (list key leave-at-least-1)
    (setq found F)
    (setq out ())
    (for item
         list
         out
         (if (or (not (equals key item))
                 (and leave-at-least-1
                      (not found)
                      (set found 1)))
              (set out (append-item out item)))))

  (defun decrement-key-from-list (list key leave-at-least-1)
    (setq found F)
    (setq dec F)
    (setq out ())
    (for item
         list
         out
         (if (or dec
                 (not (equals key item))
                 (and leave-at-least-1
                      (not found)
                      (set found 1)))
              (set out (append-item out item))
              (set dec 1))))

  (defun get-vmap-key-list (vmap)
    (if _INSTRUMENTATION_USE_NEW_MAP
      (mapcar x (get-vector-map-keys vmap) (first x))
      (mapcar x (get-all-vector-entry-list vmap) (first x))))

  (defun get-vmap-entry-list (vmap)
    (if _INSTRUMENTATION_USE_NEW_MAP
        (get-vector-map-keys vmap)
        (get-all-vector-entry-list vmap)))

  ; * 
  (defmacro make-vmap ()
    (if _INSTRUMENTATION_USE_NEW_MAP
        (make-vector-map)
        (make-int-hashtable)))

  ; * 
  (defmacro vmap-set (map key value)
    `(if _INSTRUMENTATION_USE_NEW_MAP
        (vector-map-set-value ,map ,key ,value)
        (map-vector-value ,map ,key ,value)))

  ; * 
  (defmacro vmap-get (map key)
    `(if _INSTRUMENTATION_USE_NEW_MAP
        (vector-map-get-value ,map ,key)
        (get-vector-value ,map ,key)))

  (defun get-extended-robot-state-width (state-config-map)
        (multiple-bind (num-ultra-discretization-stages num-action-types reward-value-width max-action-rep-count)
                       (list (gethash state-config-map _ULTRASONIC_WIDTH_KEY)
                             (gethash state-config-map _NUM_ACTION_TYPE_KEY)
                             (gethash state-config-map _UNSIGNED_REWARD_WIDTH_KEY)
                             (gethash state-config-map _ACTION_REP_COUNT_WIDTH_KEY)))
        (+ num-ultra-discretization-stages
           max-action-rep-count
           (+ reward-value-width 1)
           (+ reward-value-width 1)
           1
           1
           num-action-types))


  ; End generic helper functions

  ; Robot functions
      ; This function constructs a state-action vector in the following manner:
    ; ( {current-ultra-vec} 
    ;   {num-times-last-action-repeated} 
    ;   {current-reward} 
    ;   {current-previous-reward-delta} 
    ;   {current-left-collison-state} 
    ;   {current-right-collision-state}
    ;   {previous-action-id} )
  (defun create-extended-robot-state-vec (current-ultrasonic action-rep-count current-reward previous-reward current-left-collision current-right-collision previous-action-id state-config-map)
      (multiple-bind (num-ultra-discretization-stages num-action-types reward-value-width action-count-width)
                     (list (gethash state-config-map _ULTRASONIC_WIDTH_KEY)
                           (gethash state-config-map _NUM_ACTION_TYPE_KEY)
                           (gethash state-config-map _UNSIGNED_REWARD_WIDTH_KEY)
                           (gethash state-config-map _ACTION_REP_COUNT_WIDTH_KEY)))

              
      (append (setq ultra-vec
                    (unsigned-vectorize current-ultrasonic
                                        num-ultra-discretization-stages
                                        0
                                        255))
              (if (> action-count-width 0)
                  (fast-num-to-tally-vector action-rep-count
                                            action-count-width)
                  ())
              (fast-num-to-tally-vector current-reward reward-value-width 1)
              (signed-vectorize (- current-reward
                                   (or previous-reward current-reward))
                                reward-value-width
                                (* 2 reward-value-width))
              current-left-collision 
              current-right-collision
              (fast-num-to-enum-vector previous-action-id num-action-types)))

    (setq _TEST_P 1)
    (setq _TEST_P F)
    (setq _DEBUG_P 1)
    (setq _MATCH_PARENT "match_parent")
    (setq _WRAP_CONTENT "wrap_content")
    (setq _DEFAULT_LARGE_ICON_SIZE_DP 75)
    (setq _DEFAULT_ICON_SIZE_DP 50)
    (setq _DEFAULT_SMALL_ICON_SIZE_DP 35)

    (setq ROBOT_CONTEXT_KEY
        "robot-test-context")

    (setq _GLOBAL_DROPBOX_BASE_PATH "/speechbot")

    (setq RELOAD_AI_TOOLS_P F)

    (if (not _TEST_P)
        (if (or (not (var-exists-p "_LOADED_AI_TOOLS_P"))
                RELOAD_AI_TOOLS_P)
            (progn
                (load-page-by-title "global-ai")
                (log-info "loaded global ai data")
                (set RELOAD_AI_TOOLS_P F))))

    (defun upload-data-to-dropbox (data-key dropbox-file)
        (upload-dropbox-file (or dropbox-file "/speechbot/robot_data.lisp")
                             (serialize (get-data-value data-key ROBOT_CONTEXT_KEY))
                             1
                             (lambda (target-file error)
                                    (if error
                                        (progn
                                          (show-short-toast "Error uploading file")
                                          (log-error "Failure uploading file"
                                                     error))
                                        (log-info "Successfully saved robot data" (concat "data saved to: " target-file))))))


    (defun display-boolean (value)
      (if value "true" "false"))

    (defun get-date-string (date)
      (setq date (or date (time)))

      (setq date-spec
           (get-datetime-parts date))

      (setq temporal-suffix
            (concat (gethash date-spec "YEAR")
                    (gethash date-spec "MONTH")
                    (gethash date-spec "DAY_OF_MONTH"))))

    (defun get-datetime-data-key (base-key date-suffix)
      (setq key 
            (concat base-key "-" date-suffix)))

    (defun save-training-data (data data-key)
      (set-data-value data-key data ROBOT_CONTEXT_KEY))

    (defun get-training-data (data-key)
      (get-data-value data-key ROBOT_CONTEXT_KEY))

    (setq prev-message F)
    (defun show-toast-off-thread (message)
          (if (not (equals prev-message message))
              (progn
                  (evaluate-foreground F
                                       (show-short-toast message))
                  (set prev-message message))))


    (setq _DEFAULT_LARGE_ICON_SIZE_DP 75)
    (setq _DEFAULT_ICON_SIZE_DP 50)
    (setq _DEFAULT_SMALL_ICON_SIZE_DP 35)
    (setq _DEFAULT_SMALL_CARD_ICON_SIZE_DP 25)


    (setq _PATTERN_MAP
          (make-string-hashtable))

    (setq _INITIAL_MOTOR_PATTERNS
          (make-string-hashtable))

    (defun set-pattern (pattern-name pattern-list)
      (defhash _PATTERN_MAP
               pattern-name
               pattern-list))

    (defun get-pattern (pattern-name)
      (gethash _PATTERN_MAP pattern-name))

    (setq nxt F)

    (setq _WHEEL_RADIUS_CENTI 4.5)

    (setq _STANDARD_POWER 20)

    (setq _STANDARD_ROTATIONAL_POWER 15)

    (setq _CURRENT_MOTOR_POWER
          15)

    (setq _PI 3.14159265)

    
    (setq _ACTION_KEY_LEFT "left")
    (setq _ACTION_KEY_RIGHT "right")
    (setq _ACTION_KEY_FORWARD "forward")
    (setq _ACTION_KEY_BACKWARD "back")
    (setq _ACTION_KEY_STOP "stop")

    (setq _BASE_ACTION_TYPE_NAMES 
          (list _ACTION_KEY_LEFT _ACTION_KEY_RIGHT _ACTION_KEY_FORWARD _ACTION_KEY_BACKWARD _ACTION_KEY_STOP))

    (setq _STANDARD_ROBOT_CONFIG
          (make-string-hashtable (list (list _NUM_ACTION_TYPE_KEY (length _BASE_ACTION_TYPE_NAMES))
                                       (list _ULTRASONIC_WIDTH_KEY 20)
                                       (list _UNSIGNED_REWARD_WIDTH_KEY 10)
                                       (list _ACTION_REP_COUNT_WIDTH_KEY 0)
                                       (list _ACTION_NAMES_KEY _BASE_ACTION_TYPE_NAMES))))

    (setq _ACTION_NAME_TO_ID_MAP
          (make-string-hashtable (mapcar (name i)
                                         _BASE_ACTION_TYPE_NAMES
                                         (list name (integer i)))))

    (defun get-action-id (name)
        (gethash _ACTION_NAME_TO_ID_MAP
                 name))

    (setq _ULTRA_SONIC_DATA_KEY "ultrasonic")
    (setq _LEFT_TACH_DATA_KEY "left-tach")
    (setq _RIGHT_TACH_DATA_KEY "right-tach")
    (setq _LEFT_BUMPER_DATA_KEY "left-collision")
    (setq _RIGHT_BUMPER_DATA_KEY "right-collision")
    (setq _REWARD_SIGNAL_DATA_KEY "reward-signal")
    (setq _PREV_REWARD_SIGNAL_DATA_KEY "previous-reward-signal")
    (setq _PREV_ACTION_DATA_KEY "prev-action")
    (setq _ACTION_COUNT_DATA_KEY "action-count")

    (setq _MAX_ACTION_REP_COUNT 10)

    (setq _SENSOR_DATA_CACHE_MAP
          (make-string-hashtable))

    (setq _KEYWORD_SONAR ':SONAR)
    (setq _KEYWORD_LEFT_TACH ':LEFT-TACH)
    (setq _KEYWORD_RIGHT_TACH ':RIGHT-TACH)
    (setq _KEYWORD_LEFT_COLLISON ':COLLISION-LEFT)
    (setq _KEYWORD_RIGHT_COLLISON ':COLLISION-RIGHT)
    (setq _ENABLE_SENSOR_CACHE F)

    (setq _ACTION_SEQUENCE_MAP
          (make-string-hashtable))


    (defun update-sensor-cache-values (...)
      (if (not _ENABLE_SENSOR_CACHE)
          (return F))
      (if (gethash key-map _KEYWORD_SONAR)
          (defhash _SENSOR_DATA_CACHE_MAP
                   _ULTRA_SONIC_DATA_KEY
                   (gethash key-map _KEYWORD_SONAR)))

      (if (gethash key-map _KEYWORD_LEFT_TACH)
          (defhash _SENSOR_DATA_CACHE_MAP
                   _LEFT_TACH_DATA_KEY
                   (gethash key-map _KEYWORD_LEFT_TACH)))

      (if (gethash key-map _KEYWORD_RIGHT_TACH)
          (defhash _SENSOR_DATA_CACHE_MAP
                   _RIGHT_TACH_DATA_KEY
                   (gethash key-map _KEYWORD_RIGHT_TACH)))

      (if (gethash key-map _KEYWORD_LEFT_COLLISON)
          (defhash _SENSOR_DATA_CACHE_MAP
                   _LEFT_BUMPER_DATA_KEY
                   (gethash key-map _KEYWORD_LEFT_COLLISON)))

      (if (gethash key-map _KEYWORD_RIGHT_COLLISON)
          (defhash _SENSOR_DATA_CACHE_MAP
                   _RIGHT_BUMPER_DATA_KEY
                   (gethash key-map _KEYWORD_RIGHT_COLLISON))))

    ; This is not currently used 
    (defun create-simple-robot-state (current-ultrasonic current-reward current-left-collision current-right-collision previous-action-id state-config-map)
      (append (setq ultra-vec
                      (unsigned-vectorize current-ultrasonic
                                          num-ultra-discretization-stages
                                          0
                                          255))
                (fast-num-to-tally-vector current-reward reward-value-width 1)
                current-left-collision 
                current-right-collision
                (fast-num-to-enum-vector previous-action-id num-action-types)))


    (defun view-extended-robot-state (state-vec state-config-map)
        (multiple-bind (num-ultra-discretization-stages num-action-types reward-value-width action-name-list max-action-rep-count)
                       (list (gethash state-config-map _ULTRASONIC_WIDTH_KEY)
                             (gethash state-config-map _NUM_ACTION_TYPE_KEY)
                             (gethash state-config-map _UNSIGNED_REWARD_WIDTH_KEY)
                              (gethash state-config-map _ACTION_NAMES_KEY)
                              (gethash state-config-map _ACTION_REP_COUNT_WIDTH_KEY)))

        (multiple-bind (current-ultrasonic-vec action-rep-count-tally-vec current-reward-vec previous-reward-delta current-left-collision current-right-collision previous-action-vec)
                       (list (subseq state-vec 
                                     0 
                                     (setq total num-ultra-discretization-stages))
                             (subseq state-vec
                                     total 
                                     (setq total (+ total max-action-rep-count)))
                             (subseq state-vec
                                     total
                                     (setq total (+ total 1 reward-value-width)))
                             (subseq state-vec
                                     total
                                     (setq total (+ total reward-value-width 1)))
                             (subseq state-vec
                                     total
                                     (setq total (+ total 1)))
                             (subseq state-vec
                                     total
                                     (setq total (+ total 1)))
                             (subseq state-vec
                                     total
                                     (setq total (+ total num-action-types)))))
      (list (fast-tally-vector-to-num current-ultrasonic-vec)
            (fast-tally-vector-to-num action-rep-count-tally-vec)
            (fast-tally-vector-to-num current-reward-vec 1)
            (fast-tally-vector-to-num previous-reward-delta 1)
            (append current-left-collision current-right-collision)
            (nth action-name-list
                 (fast-enum-vector-to-num previous-action-vec))))

    (defun get-robot-logical-state (state-vec config)
        (multiple-bind (ultra rep-count reward prev-reward collision-pair action)
                       (view-extended-robot-state state-vec config))
        (make-string-hashtable (list (list _ULTRA_SONIC_DATA_KEY (integer (* ultra (/ 255 (gethash config _ULTRASONIC_WIDTH_KEY)))))
                                     (list _REWARD_SIGNAL_DATA_KEY reward)
                                     (list _LEFT_BUMPER_DATA_KEY (first collision-pair))
                                     (list _RIGHT_BUMPER_DATA_KEY (second collision-pair))
                                     (list _PREV_ACTION_DATA_KEY action)
                                     )))


    (defun throw-error (message)
      (speak message)
      (signal _ERROR_GENERAL))


    (defun centi-to-degrees (centi)
      (/ (* centi 180 _DISTANCE_MULTIPLIER)
         (* _WHEEL_RADIUS_CENTI _PI)))

    (defun get-left-tach ()
      (setq motor-port
          (gethash (get-device-motor-logical-port-map nxt)
                    "left motor"))
      (if motor-port
          (or (gethash _SENSOR_DATA_CACHE_MAP _LEFT_TACH_DATA_KEY)
              (get-motor-tach nxt
                              motor-port))
          (throw-error "the left motor port is undefined")))

    (defun get-right-tach ()
      (setq motor-port
          (gethash (get-device-motor-logical-port-map nxt)
                   "right motor"))
      (if motor-port
          (or (gethash _SENSOR_DATA_CACHE_MAP _RIGHT_TACH_DATA_KEY)
              (get-motor-tach nxt
                              motor-port))
        (throw-error "the right motor port is undefined")))

    (defun get-sonar ()
        (setq logical-port-name "sonar")
        (get-sonar-value nxt logical-port-name))

    (defun get-left-bumper-state ()
        (if (get-touch nxt "left touch") 1 0))

    (defun get-rear-bumper-state ()
        (if (get-touch nxt "rear touch") 1 0))

    (defun get-right-bumper-state ()
      (if (get-touch nxt "right touch") 1 0))

    (defun action-forward ()
      (run-with-priority MAX_THREAD_PRIORITY (move-forward nxt
                              _CURRENT_MOTOR_POWER)))

    (defun action-backward ()
      (run-with-priority MAX_THREAD_PRIORITY (move-backward nxt
                    _CURRENT_MOTOR_POWER)))


    (defun action-rotate-left ()
      (run-with-priority MAX_THREAD_PRIORITY (rotate-left nxt
                    _CURRENT_MOTOR_POWER)))

    (defun action-rotate-right ()
      (run-with-priority MAX_THREAD_PRIORITY (rotate-right nxt
                   _CURRENT_MOTOR_POWER)))

    (defun action-stop ()
      (run-with-priority MAX_THREAD_PRIORITY (stop-moving nxt)))
          
    ; Not using this yet
    (defun add-sensory-data (awareness-map reward)
        (defhash awareness-map
                 _LEFT_TACH_DATA_KEY
                 (get-left-tach))

        (defhash awareness-map
                 _RIGHT_TACH_DATA_KEY
                 (get-right-tach))

        (defhash awareness-map
                 _ULTRA_SONIC_DATA_KEY
                 (get-sonar))

        (defhash awareness-map
                 _LEFT_BUMPER_DATA_KEY
                 (get-left-bumper-state))

        (defhash awareness-map
                 _RIGHT_BUMPER_DATA_KEY
                 (get-right-bumper-state))
        (defhash awareness-map
                 _PREV_REWARD_SIGNAL_DATA_KEY
                 (or (gethash awareness-map _REWARD_SIGNAL_DATA_KEY) 0))

        (defhash awareness-map
                 _REWARD_SIGNAL_DATA_KEY
                 (or reward 0))
        awareness-map)


    (defun get-next-robot-state (action-name awareness-map ...)
      (setq action-name
            (or action-name "stop"))

      (setq awareness-map
            (or awareness-map (make-string-hashtable)))


      (setq action-count
            (or (and (or (not (setq action-count (gethash awareness-map _ACTION_COUNT_DATA_KEY)))
                         (not (equals action-name 
                                      (gethash awareness-map _PREV_ACTION_DATA_KEY))))
                     1)
                (inc action-count)))

      (defhash awareness-map _PREV_ACTION_DATA_KEY action-name)
      (defhash awareness-map _ACTION_COUNT_DATA_KEY action-count)

      (create-extended-robot-state-vec (gethash awareness-map _ULTRA_SONIC_DATA_KEY)
                                       (gethash awareness-map _ACTION_COUNT_DATA_KEY)
                                       (gethash awareness-map _REWARD_SIGNAL_DATA_KEY)
                                       (gethash awareness-map _PREV_REWARD_SIGNAL_DATA_KEY)
                                       (gethash awareness-map _LEFT_BUMPER_DATA_KEY)
                                       (gethash awareness-map _RIGHT_BUMPER_DATA_KEY)
                                       (get-action-id action-name)
                                       _STANDARD_ROBOT_CONFIG))

    (setq _BASE_ACTION_MAP
          (make-string-hashtable (list (list _ACTION_KEY_LEFT (lambda () (action-rotate-left)))
                                       (list _ACTION_KEY_RIGHT (lambda () (action-rotate-right)))
                                       (list _ACTION_KEY_FORWARD (lambda () (action-forward)))
                                       (list _ACTION_KEY_BACKWARD (lambda () (action-backward)))
                                       (list _ACTION_KEY_STOP (lambda () (action-stop))))))

    (defun execute-base-action (action-name)
        
        (setq action-lambda
              (gethash _BASE_ACTION_MAP action-name))
        (if action-lambda
            (funcall action-lambda)
            (throw-error (concat "The action action: " action-name " doesn't exist"))))

    ; o)o o)o o)o o)o o)o o)o o)o o)o 
    ; Model -> UI keys


    (setq _BEING_RECOGNIZED_KEY "BEING_RECOGNIZED")

    ; o)o o)o o)o o)o o)o o)o o)o o)o 
    
      (defun create-collapsible-container (label initial-open-state view)
          (setq state-map
                (make-string-hashtable (list (list "opened" "true")
                                             (list "closed" "false"))))
          
          (vertical-layout :width _MATCH_PARENT
                           :height _WRAP_CONTENT
                           :background (create-background :border-color "#E5B1F2" 
                                                          :border-width 2
                                                          :foreground-color "#FFFFFF"
                                                          :background-color "#FFFFFF"
                                                          :corner-radius 5)
                           (checkbox label
                                    :width _MATCH_PARENT
                                    :height _WRAP_CONTENT
                                    :checked (or (gethash state-map initial-open-state) "false")
                                    :on-check-changed (if is-checked-p
                                                          (show-view view)
                                                          (hide-view view 1)))
                           (if (equals initial-open-state "opened")
                               (show-view view)
                               (hide-view view 1))))

    (defun do-action-step-timing (action-type-key data-map delay)
        (setq delay
              (or delay 2000))

        (multiple-bind (start-left-tach start-right-tach)
                       (list (get-left-tach)
                             (get-right-tach)))
        (setq stop-time (+ (time) delay))
        (execute-base-action action-type-key)
         (unless (> (time) stop-time))
        (action-stop)
        (multiple-bind (end-left-tach end-right-tach)
                       (list (get-left-tach)
                             (get-right-tach)))
        (incr-list-map data-map action-type-key (setq delta (list (- end-left-tach start-left-tach) (- end-right-tach start-right-tach))))
        delta)

  (setq view-state-list ())

  (setq view-state-segment-list ())

  (setq _GRAPH_MAP_KEY "GRAPH_MAP")

  (setq _GRAPH_CONTEXT_KEY "GRAPH_MAP_CONTEXT")

  (setq _MOTOR_PATTERN_KEY "MOTOR_PATTERN_KEY")

  (defun load-graph (name)
    (setq map
          (or (get-data-value _GRAPH_MAP_KEY _GRAPH_CONTEXT_KEY)
              (make-string-hashtable)))
    (if name
        (gethash map name)
        map))


  (defun persist-graph (name value)
    (setq prior
          (load-graph))

    (defhash prior
             name
             value)
    (set-data-value _GRAPH_MAP_KEY
                    prior
                    _GRAPH_CONTEXT_KEY)
    prior)

  (defun persist-motor-pattern (to-current-world)

    (if to-current-world
        (set-custom-motor-patterns _PATTERN_MAP)
        (set-data-value _MOTOR_PATTERN_KEY
                _PATTERN_MAP
                _GRAPH_CONTEXT_KEY))

    _PATTERN_MAP)

  (defun load-motor-pattern ()
      (set _PATTERN_MAP
           (or (get-data-value _MOTOR_PATTERN_KEY _GRAPH_CONTEXT_KEY)
               (make-string-hashtable))))

  (defun remove-graph (name)
    (setq prior
          (load-graph))

    (remhash prior name)
    (set-data-value _GRAPH_MAP_KEY
                    prior
                    _GRAPH_CONTEXT_KEY)
    prior)

  (defun load-graph-names (sort-asending-p sub-sort-asending-p)
    (if sort-asending-p
        (sort-strings-ascending (get-hash-keys (load-graph))
                               sub-sort-asending-p)
        (sort-strings-descending (get-hash-keys (load-graph))
                                 sub-sort-asending-p)))

  (setq _LAST_GRAPH_NAME "LAST-GRAPH")


  ; Real robot stuff
  ; Database Serialization keys
  (setq _ROBOT_METADATA_KEY "robot-meta-data")
  (setq _ROBOT_CORE_DATA_KEY "robot-data")



  (setq _WORLD_NAME_DATA_KEY "WORLD-DATA-NAME")
  (setq _STANDARD_ROBOT_WORLD_TYPE "STANDARD-4-GROUP-ROBOT")


  (setq _DEFAULT_NUM_FEATURE_MEMORY_CELL_STATES 20)
  (setq _DEFAULT_NUM_FEATURE_NUM_INPUT_OUTPUT_NODES (get-extended-robot-state-width _STANDARD_ROBOT_CONFIG))
  (setq _DEFAULT_FEATURE_BUFFER_LENGTH 4)
  (setq _MINIMUM_FOCUS_BUFFER_OVERLAP_LENGTH 3)
  
  (setq _BEHAVIOR_GROUP_WEIGHT 1)
  (setq _MEMORY_CACHE_GROUP_WEIGHT 1)

  (setq STANDARD_ROBOT_TYPE_NAME "STANDARD-ROBOT")
  (setq BEHAVIOR_TYPE_NAME "BEHAVIOR-GROUP-TYPE")
  ; max time spent learning
  (setq STANDARD_ROBOT_PROCESSING_MS 2000)
  ; max time spent learning
  (setq BEHAVIOR_TYPE_PROCESSING_MS 4000)

  (setq FEAR_LEVEL_ROBOT_GROUP_NAME "FEAR-LEVEL")
  (setq REWARD_SEEKING_GROUP_NAME "REWARD-SEEKING")
  (setq MEMCACHE_GROUP_NAME "SHORT-TERM-MEMORY")
  (setq MIMIC_GROUP_NAME "MIMICRY")

  ; robot meta-data keys

  (setq current-world-custom-meta-data F)
  (setq current-world-name F)
  (setq current-world-desc F)
  (setq current-world-allocation F)


; .i-i...i-i...i-i...i-i...i-i...i-i...i-i...i-i...i-i...i-i...i-i..
; Journal related functions
; ..................................................................

(defun get-current-journal ()
    (mapcar entry
            (get-current-meta-data _META_DATA_JOURNAL_KEY)
            (append (list (get-datetime-string (first entry)))
                    (rest entry))))

; Add a new entry
(defun append-current-journal (tag entry color)
    (setq color
          (or color _JOURNAL_ENTRY_RED))
    (setq prior
          (get-current-meta-data _META_DATA_JOURNAL_KEY))
    (set-current-meta-data _META_DATA_JOURNAL_KEY (append-item prior (list (time) tag entry color))))

; Clear journal
(defun clear-current-journal ()
    (set-current-meta-data _META_DATA_JOURNAL_KEY ()))

(setq _JOURNAL_ENTRY_RED "#730000")
(setq _JOURNAL_ENTRY_BLUE "#000373")
(setq _JOURNAL_ENTRY_GREEN "#007302")
(setq _JOURNAL_ENTRY_VIOLET "#730061")

(setq _JOURNAL_DEFAULT_TAG_COLOR _JOURNAL_ENTRY_VIOLET)



; ---(o-- o=)(=====> ---(o-- o=)(=====> ---(o-- o=)(=====> 
; Robot Detailed Metadata functions and variables
; ________________________________________________________
  (setq _TRANSITION_MODEL_HISTORY_LENGTH 8)

  (setq _STATE_TRANSITIONS_KEY "TRANSITION-MODEL")
  (setq _META_DATA_STATE_META_KEY "STATE-META")
  (setq _META_DATA_JOURNAL_KEY "JOURNAL")
  (setq _META_DATA_MOTOR_PATTERNS_KEY "MOTOR_PATTERNS")
  (setq _META_DATA_PRIOR_STATE_KEY "PRIOR-STATE")

  (setq _META_DATA_DEFAULT_PATTERNS_KEY "DEFAULT-PATTERNS")
  (setq _META_DATA_EXPLORATORY_PATTERNS_KEY "EXPORATORY-PATTERNS")
  (setq _META_DATA_RECOVERY_PATTERNS_KEY "RECOVERY-PATTERNS")
  (setq _META_DATA_PATTERN_PREFS_KEY "PATTERN-PREF-LIST")
  (setq _META_DATA_PATTERN_PREFS_MAX_SIZE_KEY "PATTERN-PREF-LIST-MAX-SIZE")

  (setq _STATE_METADATA_LABELS_KEY "LABELS")

; State meta-data 

  ; * 
  ; Returns a hashtable
  (defun get-current-world-vector-meta-data (state-vector)
      (setq meta-map (get-current-meta-data _META_DATA_STATE_META_KEY))

      (or (vmap-get meta-map state-vector)
          (vmap-set meta-map state-vector (make-string-hashtable))))

  (defun get-state-labels-meta-data (state-vector)
    (or (gethash (get-current-world-vector-meta-data state-vector) _STATE_METADATA_LABELS_KEY)
        (defhash (get-current-world-vector-meta-data state-vector) _STATE_METADATA_LABELS_KEY (make-string-hashtable))))


  (defun add-label-to-state-meta-data (state-vector label)
      (setq label-map
            (get-state-labels-meta-data state-vector))

      (defhash label-map 
               label 
               1))

  (defun remove-label-from-state-meta-data (state-vector label)
      (setq label-map
            (get-state-labels-meta-data state-vector))
      (remhash label-map label))

  
  ; * 
  (defun get-initial-meta-data ()
    (make-string-hashtable (list (list _META_DATA_JOURNAL_KEY ())
                                 (list _META_DATA_STATE_META_KEY (make-vmap))
                                 (list _META_DATA_MOTOR_PATTERNS_KEY _INITIAL_MOTOR_PATTERNS)
                                 (list _META_DATA_PRIOR_STATE_KEY F)
                                 (list _META_DATA_PATTERN_PREFS_KEY (get-hash-keys _PATTERN_MAP))
                                 (list _META_DATA_PATTERN_PREFS_MAX_SIZE_KEY 30)
                                 )))


  ; ..................................................
  ; State Vector transitions
  ; ..................................................

  ; * 
  ; Check that testing for existence of data creates data structures for storage
  (defun get-state-transitions (state-vec unique-p)

    (setq trans-list
          (or (gethash (get-current-world-vector-meta-data state-vec) _STATE_TRANSITIONS_KEY) ()))
    (if (not unique-p)
        (return trans-list))

    (setq map
          (make-vmap))

    (for vector
         trans-list
         (get-vmap-key-list map)
         (vmap-set map vector 1)))

  (defun set-state-vector-transitions (state-vec next-vects)
    (defhash (get-current-world-vector-meta-data state-vec)
             _STATE_TRANSITIONS_KEY
             next-vects))

  (defun add-state-vector-transition (initial-state next-state limit)
      (set-state-vector-transitions initial-state
                             (or (and (setq prior (non-empty-list (get-state-transitions initial-state)))
                                      (subseq (append (list next-state) prior) 0 limit))
                                 (list next-state))))


  (defun set-current-meta-data (key value)
      (set current-world-custom-meta-data
           (or current-world-custom-meta-data (get-initial-meta-data)))
      (defhash current-world-custom-meta-data key value))


  (defun get-current-meta-data (key)
      (setq init-meta (get-initial-meta-data))
      (set current-world-custom-meta-data
           (or current-world-custom-meta-data init-meta))
      (if (not (gethash current-world-custom-meta-data key))
          (defhash current-world-custom-meta-data key (gethash init-meta key)))
      
      (gethash current-world-custom-meta-data key))

  (defun get-current-pattern-prefs-max-length ()
    (get-current-meta-data _META_DATA_PATTERN_PREFS_MAX_SIZE_KEY))

  (defun set-current-pattern-prefs-max-length (n)
    (set-current-meta-data _META_DATA_PATTERN_PREFS_MAX_SIZE_KEY n)
    )

  ; *
  (defun get-current-pattern-preferences ()
    (get-current-meta-data  _META_DATA_PATTERN_PREFS_KEY))

  ; *
  (defun set-current-pattern-preferences (pref-list)
    (set-current-meta-data _META_DATA_PATTERN_PREFS_KEY pref-list))

  ; *
  (defun update-pattern-preferences (pattern-name)
    (if (< (length (setq prev (get-current-pattern-preferences)))
           (setq max (get-current-pattern-prefs-max-length)))
        (set-current-pattern-preferences (append-item prev pattern-name))
        (set-current-pattern-preferences (subseq (append-item prev pattern-name)
                                                 (- (length prev) -1 max)))))


  (defun get-world-state-vectors-meta-data ()
      (get-current-meta-data _META_DATA_STATE_META_KEY))


  (defun get-state-vector-transitions (state-data-map)
      (gethash state-data-map _STATE_TRANSITIONS_KEY)
    )

  (defun save-current-prior-state ()
      (set-current-meta-data _META_DATA_PRIOR_STATE_KEY _PRIOR_STATE))

  (defun restore-world-prior-state ()
      (set _PRIOR_STATE
           (get-current-meta-data _META_DATA_PRIOR_STATE_KEY)))


  (defun switch-vmap-type (new-type)
    (if (or (not robot)
            (not current-world-custom-meta-data)
            (and (not _INSTRUMENTATION_USE_NEW_MAP) (not new-type))
            (and _INSTRUMENTATION_USE_NEW_MAP new-type))
        (return ))

    (if (not new-type)
        (progn
          (set _INSTRUMENTATION_USE_NEW_MAP F)
          (defhash current-world-custom-meta-data
                   _META_DATA_STATE_META_KEY
                   (progn
                      (setq map (make-int-hashtable ))
                      (for spec
                           (get-vector-map-keys (gethash current-world-custom-meta-data _META_DATA_STATE_META_KEY))
                           map
                           (map-vector-value map (first spec) (second spec))))))
        (progn
          (set _INSTRUMENTATION_USE_NEW_MAP 1)
          (defhash current-world-custom-meta-data
                   _META_DATA_STATE_META_KEY
                   (make-vector-map (get-all-vector-entry-list (gethash current-world-custom-meta-data _META_DATA_STATE_META_KEY)))))))

  ; _<.>_ _<.>_ _<.>_ _<.>_ _<.>_ _<.>_ _<.>_ _<.>_  
  ; custom motor pattern functions
  ; _<.>_ _<.>_ _<.>_ _<.>_ _<.>_ _<.>_ _<.>_ _<.>_ 

  (defun get-custom-motor-patterns ()
      (get-current-meta-data _META_DATA_MOTOR_PATTERNS_KEY)
    )

  (defun set-custom-motor-patterns (pattern-map)
      (set-current-meta-data _META_DATA_MOTOR_PATTERNS_KEY (copy-map pattern-map)))


  (setq MIN_SHORT_TERM_MEMORY_FEATURE 4)

  (setq robot
        F)

  (setq robot-data-interface
        F)

  (setq fear-level-group F)

  (setq reward-seeking-group F)

  (setq memory-cache-group F)

  (setq mimic-group F)

  (setq behavior-group-type F)

  (setq memcache-group-type F)


  (defun get-default-memory-listener (group-name)

      (lambda (is-starting-p result)
                (if is-starting-p
                    (progn
                      (log-ui "sleep" "dreaming starting for " group-name " group")
                      )
                    (progn
                       (log-ui "sleep" "Finished dreaming for " group-name " group.  Released: " result)
                      )
                    )))


  (defun get-standard-feature-view (feature-model)
      (setq label
            (concat (string (feature-get-custom-meta-data feature-model)) ":"
                    (string feature-model)))
      (text label))

  (defun standard-robot-world-state-view (robot-world height) 
      
      (relative :width _MATCH_PARENT
                  :height height
                  (vertical-layout :width _MATCH_PARENT
                                   :height _WRAP_CONTENT
                                   (create-collapsible-container "Short-term memory" "closed"
                                                                  (vertical-layout :width _MATCH_PARENT
                                                                                   :height _WRAP_CONTENT
                                                                                   (mapcar feature
                                                                                           (group-get-ordered-processed-features (world-get-group robot-world STANDARD_ROBOT_TYPE_NAME MEMCACHE_GROUP_NAME))
                                                                                           (get-standard-feature-view feature))))
                                   (if mimic-group

                                      (create-collapsible-container "Mimic model" "closed"
                                                                    (vertical-layout :width _MATCH_PARENT
                                                                                     :height _WRAP_CONTENT
                                                                                     (mapcar feature
                                                                                             (group-get-ordered-processed-features (world-get-group robot-world BEHAVIOR_TYPE_NAME MIMIC_GROUP_NAME))
                                                                                             (get-standard-feature-view feature)))))
                                   (create-collapsible-container "Avoidance model" "closed"
                                                                  (vertical-layout :width _MATCH_PARENT
                                                                                   :height _WRAP_CONTENT
                                                                                   (mapcar feature
                                                                                           (group-get-ordered-processed-features (world-get-group robot-world BEHAVIOR_TYPE_NAME FEAR_LEVEL_ROBOT_GROUP_NAME))
                                                                                           (get-standard-feature-view feature))))
                                   (create-collapsible-container "Reward model" "closed"
                                                                  (vertical-layout :width _MATCH_PARENT
                                                                                   :height _WRAP_CONTENT
                                                                                   (mapcar feature
                                                                                           (group-get-ordered-processed-features (world-get-group robot-world BEHAVIOR_TYPE_NAME REWARD_SEEKING_GROUP_NAME))
                                                                                           (get-standard-feature-view feature)))))))

  (defun configure-new-robot (default-allocation)

    (model-set-thread-count 4)
    (set robot
         (model-create-world default-allocation
                             (model-get-default-config)))

    (set behavior-group-type
         (world-create-group-type robot 
                                  BEHAVIOR_TYPE_NAME
                                  _DEFAULT_NUM_FEATURE_NUM_INPUT_OUTPUT_NODES
                                  _DEFAULT_NUM_FEATURE_MEMORY_CELL_STATES
                                  _BEHAVIOR_GROUP_WEIGHT
                                  _DEFAULT_FEATURE_BUFFER_LENGTH
                                  _MINIMUM_FOCUS_BUFFER_OVERLAP_LENGTH
                                  (model-get-default-config)))

    (set memcache-group-type
         (world-create-group-type robot 
                                  STANDARD_ROBOT_TYPE_NAME
                                  _DEFAULT_NUM_FEATURE_NUM_INPUT_OUTPUT_NODES
                                  _DEFAULT_NUM_FEATURE_MEMORY_CELL_STATES
                                  _MEMORY_CACHE_GROUP_WEIGHT
                                  _DEFAULT_FEATURE_BUFFER_LENGTH
                                  _MINIMUM_FOCUS_BUFFER_OVERLAP_LENGTH
                                  (model-get-default-config)))

    (group-type-set-default-string-serializer behavior-group-type)
    (group-type-set-default-string-serializer memcache-group-type)


    (set fear-level-group
         (world-add-group robot 
                          FEAR_LEVEL_ROBOT_GROUP_NAME
                          behavior-group-type))

    (set reward-seeking-group
         (world-add-group robot 
                          REWARD_SEEKING_GROUP_NAME
                          behavior-group-type))

    (set memory-cache-group
         (world-add-group robot 
                          MEMCACHE_GROUP_NAME
                          memcache-group-type))

    (group-set-max-duration-milli fear-level-group
                                  BEHAVIOR_TYPE_PROCESSING_MS)

    (group-set-max-duration-milli reward-seeking-group
                                  BEHAVIOR_TYPE_PROCESSING_MS)

    (group-set-max-duration-milli memory-cache-group
                                  STANDARD_ROBOT_PROCESSING_MS)


    (group-set-sleep-listener fear-level-group (get-default-memory-listener FEAR_LEVEL_ROBOT_GROUP_NAME))    
    (group-set-sleep-listener reward-seeking-group (get-default-memory-listener REWARD_SEEKING_GROUP_NAME))

    (group-set-mode reward-seeking-group "EXTRAPOLATION")
    (group-set-mode fear-level-group "EXTRAPOLATION")
    
    (group-toggle-memory-management fear-level-group 1)
    (group-toggle-memory-management reward-seeking-group 1)

    (group-set-mode memory-cache-group "LEARNING")
    (group-toggle-memory-management memory-cache-group 1)
    (group-toggle-simple-memory-management memory-cache-group 1)
    (group-set-feature-recycle-fraction memory-cache-group 0.20)
    (group-set-sleep-listener memory-cache-group (get-default-memory-listener MEMCACHE_GROUP_NAME))

    (group-toggle-internal-reporting memory-cache-group _ENABLE_INTERNAL_REPORTING_P)

    )


(defun save-world (name meta-only-p)
  (setq name (or name "avoidance-test-robot"))
  (setq data-key name)
  (setq context-key _ROBOT_CORE_DATA_KEY)
  (log-ui "save-world" "trying to serialie robot")
  (setq start (time))
  (setq data-b64
        (world-serialized-group-data robot))
          
  (log-ui "save-world" "trying to save data" (if (non-empty data-b64) (string (length data-b64)) "0"))
  (set-data-value data-key data-b64 context-key)
  (/ (- (time) start) 1000))


(defun restore-world (default-allocation name)
    (setq name (or name "avoidance-test-robot"))
    (setq data-key name)
    (setq context-key _ROBOT_CORE_DATA_KEY)
    
    (model-set-thread-count 4)

    (set robot
         (model-create-world default-allocation (model-get-default-config)))
    
    (set behavior-group-type
         (world-create-group-type robot 
                                  BEHAVIOR_TYPE_NAME
                                  _DEFAULT_NUM_FEATURE_NUM_INPUT_OUTPUT_NODES
                                  _DEFAULT_NUM_FEATURE_MEMORY_CELL_STATES
                                  _BEHAVIOR_GROUP_WEIGHT
                                  _DEFAULT_FEATURE_BUFFER_LENGTH
                                  _MINIMUM_FOCUS_BUFFER_OVERLAP_LENGTH
                                  (model-get-default-config)))

    (set memcache-group-type
         (world-create-group-type robot 
                                  STANDARD_ROBOT_TYPE_NAME
                                  _DEFAULT_NUM_FEATURE_NUM_INPUT_OUTPUT_NODES
                                  _DEFAULT_NUM_FEATURE_MEMORY_CELL_STATES
                                  _MEMORY_CACHE_GROUP_WEIGHT
                                  _DEFAULT_FEATURE_BUFFER_LENGTH
                                  _MINIMUM_FOCUS_BUFFER_OVERLAP_LENGTH
                                  (model-get-default-config)))

    (group-type-set-default-string-serializer behavior-group-type)
    (group-type-set-default-string-serializer memcache-group-type)

    (setq start (time))

    (setq data-b64
          (get-data-value data-key context-key))
          
    (world-load-serialized-group-data robot data-b64)

    (setq load-time
          (integer (/ (- (time) start) 1000)))

    (set fear-level-group
         (world-get-group robot 
                          BEHAVIOR_TYPE_NAME
                          FEAR_LEVEL_ROBOT_GROUP_NAME))

    (set reward-seeking-group
         (world-get-group robot
                          BEHAVIOR_TYPE_NAME 
                          REWARD_SEEKING_GROUP_NAME
                          ))

    (set memory-cache-group
         (world-get-group robot
                          STANDARD_ROBOT_TYPE_NAME 
                          MEMCACHE_GROUP_NAME
                          ))

    (group-set-max-duration-milli fear-level-group
                                  BEHAVIOR_TYPE_PROCESSING_MS)

    (group-set-max-duration-milli reward-seeking-group
                                  BEHAVIOR_TYPE_PROCESSING_MS)

    (group-set-max-duration-milli memory-cache-group
                                  STANDARD_ROBOT_PROCESSING_MS)


    (group-set-sleep-listener fear-level-group (get-default-memory-listener FEAR_LEVEL_ROBOT_GROUP_NAME))
    (group-set-sleep-listener reward-seeking-group (get-default-memory-listener REWARD_SEEKING_GROUP_NAME))


    (group-set-mode reward-seeking-group "EXTRAPOLATION")
    (group-set-mode fear-level-group "EXTRAPOLATION")
    
    (group-toggle-memory-management fear-level-group 1)
    (group-toggle-memory-management reward-seeking-group 1)

    (group-set-mode memory-cache-group "LEARNING")
    (group-toggle-memory-management memory-cache-group 1)
    (group-toggle-simple-memory-management memory-cache-group 1)
    (group-set-feature-recycle-fraction memory-cache-group 0.20)
    (group-set-sleep-listener memory-cache-group (get-default-memory-listener MEMCACHE_GROUP_NAME))

    (group-toggle-internal-reporting memory-cache-group _ENABLE_INTERNAL_REPORTING_P)

    )

  (setq WORLD-TYPE-SAVER-MAP
      (make-string-hashtable (list (list _STANDARD_ROBOT_WORLD_TYPE 
                                         (list (lambda (name)
                                                  (save-world name))
                                               (lambda (alloc)
                                                  (configure-new-robot alloc))
                                               (lambda (alloc name)
                                                  (restore-world alloc name)))))))


  (defun get-consistent-patterns (remaining-map pattern-map forcing-action)
    (for name
         (get-hash-keys pattern-map)
         remaining-map
         (if (and (empty-list (setq remaining-pattern (gethash remaining-map name)))
                  (equals (first (setq pattern (gethash pattern-map name)))
                          forcing-action))
             (defhash remaining-map
                      name
                      (rest remaining-pattern))
             (if (equals (first remaining-pattern)
                         forcing-action)
                 (defhash remaining-map name (rest remaining-pattern))))))

  (defun sample-index-probabilistically (probs)
      (maximal-value-map (value i)
                         (mapcar prob
                                 probs
                                 (* (random 0 1) (* 100 prob)))
                         value
                         i))

  (defun show-group-data (title group)
    (evaluate-foreground (lambda (result error) 
                                    (if error 
                                        (log-error "show model dialog" error)))
        (dialog :dialog-title title
          (scrollview :width _MATCH_PARENT
                      :height _MATCH_PARENT
                      (vertical-layout :width _MATCH_PARENT
                                       :height _WRAP_CONTENT
                                       (mapcar feature
                                               (group-get-ordered-processed-features group)
                                               (text (string feature))))))))


  (setq HARM-RECOVERY-STATE "HARM-RECOVERY")
  (setq HARM-AVOIDANCE-STATE "HARM-AVOIDANCE")
  (setq REWARD-SEEKING-STATE "REWAR-SEEKING")
  (setq EXECUTING-SIMPLE-PATTERN "SIMPLE-MOTOR-BEHAVIOR")
  (setq INATE-BEHAVIOR-PATTERN-STATE "INATE-BEHAVIOR")

  (setq _PRIOR_STATE F)

  (defun clear-prior-state ()
    (set _PRIOR_STATE F))

  ; interrupt-lambda runs at the end of the action step and takes the updated awareness-map as an argument
  ; if it returns true then tbe currently executed action should be interrupted.
  (defun do-action-step (action-name action-step-milli awareness-map interrupt-lambda stop-on-complete-p immediate-interrupt-lambda)
    (execute-base-action action-name)
    (setq stop-time
          (if action-step-milli
              (+ (time) action-step-milli)))
  
    
    (unless (or (and immediate-interrupt-lambda
                     (funcall immediate-interrupt-lambda))
                (and stop-time
                     (> (time) stop-time))))

    (if stop-on-complete-p
        (action-stop))
    (add-sensory-data awareness-map)
    (setq state-vector
          (get-next-robot-state action-name awareness-map))
    (if _PRIOR_STATE
        (add-state-vector-transition _PRIOR_STATE state-vector _TRANSITION_MODEL_HISTORY_LENGTH))

    (set _PRIOR_STATE state-vector)

    (list state-vector awareness-map (and interrupt-lambda (funcall interrupt-lambda action-name awareness-map))))


  (defun match-action (state-vector action-name)
      (equals action-name
              (gethash (get-robot-logical-state state-vector _STANDARD_ROBOT_CONFIG)
                       _PREV_ACTION_DATA_KEY)))


  (defun sort-state-vectors (vectors)
      (sort vectors
            (lambda (lvalue rvalue)
                (if (< (nth lvalue 2) (nth rvalue 2))
                    -1
                    (if (= (nth lvalue 2) (nth rvalue 2))
                        0
                        1)))))

  ; ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ 
  ; Planning
  ; ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ ~oOo~ 

  (defun uniquify (vect-list)
    (get-vmap-key-list (make-vmap (mapcar v vect-list (list v 1)))))

    (defun uniquify (vect-list)
       (mapcar x (get-vector-map-keys (make-vector-map (mapcar v vect-list (list v 1)))) (first x))
       )


  (defun copy-group (group)
      (group-deserialize (group-get-type group) (group-serialize group)))


  (setq _DEBUG_PLANNING 0)

  (defun extrapolate-motor-pattern (start-vector motor-pattern-iter extrap-depth max-paths planning-model-group max-group-copies retain-group-state-p)
    (defun get-next-group-state (group next-state skip-copy-p)
        (if planning-model-group
            (lambda ()
                (if (and (not skip-copy-p) (< 0 max-group-copies))
                    (progn
                        (set max-group-copies (- max-group-copies 1))
                        (setq new-group (copy-group group))
                        (group-process-input new-group next-state)
                        new-group)
                    (progn
                        (group-process-input group next-state)
                        group)))))

    (if planning-model-group
        (setq planning-model-group
              (group-set-mode (copy-group planning-model-group) "EXTRAPOLATION")))


    (setq paths (list (list () start-vector 1 (if planning-model-group (lambda () planning-model-group)))))

    (setq new-paths paths)

    (while (and (non-empty-list new-paths)
                (multiple-bind (next-action index) (funcall motor-pattern-iter))
                next-action
                (or (not extrap-depth) (< index extrap-depth)))

        (setq paths (or (and max-paths 
                             (subseq (sort-state-vectors new-paths) 
                                     0
                                     max-paths)) 
                        new-paths))

        (setq new-paths ())

        (for path-spec
             paths
             F
             (progn
                (multiple-bind (state-path current-vec parent-score next-group-lambda)
                               path-spec)
                (setq model-group F)

                (for next-state
                     (setq next-states
                            (or (and next-group-lambda
                                     (setq model-group (funcall next-group-lambda))
                                     (non-empty (uniquify (map-filter (feature-model i)
                                                                     (group-get-ordered-processed-features model-group)
                                                                     (and (equals "MATCHING" (feature-get-state feature-model))
                                                                          (setq prediction-vec (feature-get-next-predicted-value feature-model))
                                                                          (match-action prediction-vec next-action)
                                                                          (or (not _EXTRAP_DEBUG) (log-ui "extrapolations" "found next state: " (get-robot-logical-state prediction-vec _STANDARD_ROBOT_CONFIG) " from " (get-robot-logical-state current-vec _STANDARD_ROBOT_CONFIG)))
                                                                          prediction-vec)))))

                                (non-empty (find next-vector 
                                                 (get-state-transitions current-vec 1)
                                                 (match-action next-vector next-action)))
                                ()))
                      F
                      (progn
                          (set _DEBUG_PLANNING (inc _DEBUG_PLANNING))

                          (set new-paths
                               (append-item new-paths
                                            (list (append-item (mapcar x state-path x) next-state)
                                                  next-state
                                                  (* parent-score (length next-states))
                                                  (get-next-group-state model-group next-state (= 1 (length next-states)))))))))))
    (setq paths
          (mapcar path-spec
                  paths
                  (first path-spec)))

    (log-ui "extrapolate-motor-pattern" "Paths: " )
    paths)



  ; Takes a state-vector list and applies positive and negative groups against it
  (defun extrap-state-path-value (state-vector-path)
      (group-reset fear-level-group)
      (group-reset reward-seeking-group)
      (mapcar (state-vec i)
              state-vector-path
              (progn
                  (group-process-input fear-level-group state-vec)
                  (group-process-input reward-seeking-group state-vec)

                  (multiple-bind (reward-feature avoidance-feature)
                                 (list (group-get-focus-feature reward-seeking-group)
                                       (group-get-focus-feature fear-level-group)))

                  (list (or (and reward-feature
                                 (get-feature-utility reward-feature))
                            0)
                        (or (and avoidance-feature
                                       (get-feature-utility avoidance-feature))
                                  0)))))

(defun sign (num)
  (if (> num 0)
      1
      (if (= num 0)
          0
          -1)))

(defun anti-sign (num)
  (if (= num 0)
      1
      0))

(defun format-percent (frac)
  (concat (integer (* 100 frac)) "%"))

(setq _ACTION_VALUE_DESC_HARM "HARM")
(setq _ACTION_VALUE_DESC_REWARD "REWARD")
(setq _ACTION_VALUE_DESC_NEUTRAL "NEUTRAL")

(defun describe-action-value-spec (value-spec harm-cutoff-fraction reward-cutoff-fraction)
  (multiple-bind (PATTERN-COUNT NEUTRAL-COUNT REWARD-COUNT HARM-COUNT REWARD-ANTIC HARM-ANTIC)
                                value-spec)
  (multiple-bind (harm-prob harm-degree reward-prob reward-degree)
                 (list (/ HARM-COUNT
                          PATTERN-COUNT)
                       HARM-ANTIC
                       (/ REWARD-COUNT
                          PATTERN-COUNT)
                       REWARD-ANTIC))

  (multiple-bind (reward-value harm)
                 (list (* reward-prob reward-degree)
                       (* harm-prob harm-degree)))

  ; More sensitive to harm than 
  (multiple-bind (harm-cutoff-fraction reward-cutoff-fraction)
                 (list (or harm-cutoff-fraction 0.2) (or reward-cutoff-fraction 0.4)))

  (setq description
        (if (< reward-value harm)
            (if (and (> harm 0) (> (/ (- harm reward-value) harm) harm-cutoff-fraction))
                _ACTION_VALUE_DESC_HARM
                _ACTION_VALUE_DESC_NEUTRAL)
            (if (and (> reward-value 0) (> (/ (- reward-value harm ) reward-value) reward-cutoff-fraction))
                _ACTION_VALUE_DESC_REWARD
                _ACTION_VALUE_DESC_NEUTRAL))))

(defun extrap-motor-pattern-value (current-state motor-pattern-iter search-depth max-paths retain-group-state-p)
  (setq value-list (1 0 0 0 0 0))
  (multiple-bind  (PATTERN-COUNT NEUTRAL-COUNT REWARD-COUNT HARM-COUNT REWARD-ANTIC HARM-ANTIC)
                  (list 0 1 2 3 4 5))

  (multiple-bind (search-depth max-paths)
                 (list (or search-depth 7)
                       (or max-paths 20)))

  (setq tag "extrap-motor-pattern-value")

  (log-ui "planning timing" "Extrapolating motor pattern")
  (setq total-start-time (time))
  (setq extrap-start-time F)
  (for path
       (or (extrapolate-motor-pattern current-state motor-pattern-iter search-depth max-paths (if _INSTRUMENTATION_PLAN_WITH_MODEL memory-cache-group) _INSTRUMENTATION_PLAN_WITH_MODEL retain-group-state-p) ())
       (progn
          (setq value-milli (integer (- (time) total-start-time)))
          (setq planning-ms
                (- (or extrap-start-time total-start-time) total-start-time))
          (setq model-ms
                (- (time) (or extrap-start-time total-start-time)))

          (multiple-bind (harm-prob harm-degree reward-prob reward-degree)
                         (list (/ (nth value-list HARM-COUNT)
                                  (nth value-list PATTERN-COUNT))
                               (nth value-list HARM-ANTIC)
                               (/ (nth value-list REWARD-COUNT)
                                  (nth value-list PATTERN-COUNT))
                               (nth value-list REWARD-ANTIC)))
          (setq report
                (concat "Overal stats: " 
                        value-list 
                        " harm anticipation: " 
                        harm-degree " at " (format-percent harm-prob))
                        "and reward anticipation: "
                        reward-degree " at " (format-percent reward-prob))
          (log-ui "planning timing" report)
          (log-ui "planning timing" "Time spent in constructing test paths: " 
                                    (integer (/ planning-ms 1000)) 
                                    "(" 
                                    (format-percent (/ planning-ms value-milli))
                                     ") secs.  Time spent reliving paths: "  
                                    (integer (/ model-ms 1000))
                                    "("
                                    (format-percent (/ model-ms value-milli))
                                    ") secs")

          value-list)
       (progn
          (if (not extrap-start-time)
              (set extrap-start-time (time)))
          (setq start-time (time))
          (setq local-value-list (0 0 0 0 0 0))
          (for value-spec
               (extrap-state-path-value path)
               (progn
                    (setq value-milli (integer (- (time) start-time)))
                    (multiple-bind (harm-prob harm-degree reward-prob reward-degree)
                                   (list (/ (nth local-value-list HARM-COUNT)
                                            (nth local-value-list PATTERN-COUNT))
                                         (nth local-value-list HARM-ANTIC)
                                         (/ (nth local-value-list REWARD-COUNT)
                                            (nth local-value-list PATTERN-COUNT))
                                         (nth local-value-list REWARD-ANTIC)))
                    (setq report
                          (concat "Overal stats: " 
                                  local-value-list 
                                  " harm anticipation: " 
                                  harm-degree " at " (format-percent harm-prob))
                                  "and reward anticipation: "
                                  reward-degree " at " (format-percent reward-prob))

                    (log-ui "planning timing" (string value-milli) " ms to assess value of: " (string path) ".  Value of path: " report))
               (progn
                  (multiple-bind (reward-fraction avoid-fraction)
                                 value-spec)

                  (incr-list-index local-value-list PATTERN-COUNT 1)
                  (incr-list-index local-value-list REWARD-COUNT (setq rc (sign reward-fraction)))
                  (incr-list-index local-value-list REWARD-ANTIC reward-fraction)
                  (incr-list-index local-value-list HARM-COUNT (setq hc (sign avoid-fraction)))
                  (incr-list-index local-value-list HARM-ANTIC avoid-fraction)
                  (incr-list-index local-value-list NEUTRAL-COUNT (anti-sign (+ rc hc)))

                  (incr-list-index value-list PATTERN-COUNT 1)
                  (incr-list-index value-list REWARD-COUNT (setq rc (sign reward-fraction)))
                  (incr-list-index value-list REWARD-ANTIC reward-fraction)
                  (incr-list-index value-list HARM-COUNT (setq hc (sign avoid-fraction)))
                  (incr-list-index value-list HARM-ANTIC avoid-fraction)
                  (incr-list-index value-list NEUTRAL-COUNT (anti-sign (+ rc hc)))

                  )))))

  (setq STEP_KEY_GET_REWARD_ANTICIPATORS "REWARD HOPING MODELS")
  (setq STEP_KEY_GET_HARM_ANTICIPATORS "HARM EXPECTING MODELS")
  (setq STEP_KEY_GET_WAS_REWARDS "WAS REWARDED")

  (setq STEP_KEY_LAST_FEATURE_WAS_REWARDED "LAST_FEATURE WAS REWARDED")

  (setq STEP_KEY_LAST_FEATURE_PROCESSED "LAST_FEATURE")
  

  (defun get-standard-learning-step-uppdate-lambda (learning-p reset-first-p allow-fear-interrupt-p decay-all-features-p executing-pattern-name on-process-step-lambda)
    (setq first 1)
    (setq standard-pattern-decay 0.99)
    (setq FEAR-CONFIRMATION-ENHANCEMENT-FRACTION 0.2)
    (setq AVOIDANCE-BEHAVIOR 1)
    (setq SEEKING-BEHAVIOR 0)
    (setq SEVERE-USER-FEAR-THRESHOLD 0.9)
    (setq update-activation-probability-of-confirmed-fear-p 1)
    (setq tag
          "action update lambda")

    (setq harm-predicting-model-ids
          (make-int-hashtable))

    (setq reward-predicting-model-ids
          (make-int-hashtable))

    (setq step-response
          (make-string-hashtable (list (list STEP_KEY_GET_REWARD_ANTICIPATORS reward-predicting-model-ids)
                                       (list STEP_KEY_GET_HARM_ANTICIPATORS harm-predicting-model-ids)
                                       (list STEP_KEY_GET_WAS_REWARDS F)
                                       )))

    (defun set-last-feature (f)
        (defhash step-response STEP_KEY_LAST_FEATURE_PROCESSED f))

    (defun declare-reward (utility)
        (set was-rewarded-p (max utility (or was-rewarded-p 0)))
        (defhash step-response STEP_KEY_GET_WAS_REWARDS 1)
        (defhash step-response STEP_KEY_LAST_FEATURE_WAS_REWARDED (max utility was-rewarded-p)))

    (defun declare-harm ()
        (set felt-pain-p 1)
        (remhash step-response STEP_KEY_LAST_FEATURE_WAS_REWARDED))

    (defun clear-total-reward-data ()
        (set felt-pain-p F)
        (set was-rewarded-p F)
        (remhash step-response STEP_KEY_LAST_FEATURE_WAS_REWARDED))

    (defun add-harm-expecting-model (model)
      (defhash harm-predicting-model-ids
               (feature-get-group-allocation-index model)
               model))

    (defun add-reward-expecting-model (model)
      (defhash reward-predicting-model-ids
               (feature-get-group-allocation-index model)
               model))
          
    (defun remove-harm-expecting-model (model)
       (remhash harm-predicting-model-ids (feature-get-group-allocation-index model)))

    (setq prev-focus F)

    (setq was-rewarded-p F)
    (setq felt-pain-p F)

    (lambda (state-vec awareness-map)

        (setq direct-harm-detection-p
              (or (< (gethash awareness-map _ULTRA_SONIC_DATA_KEY) 15)
                  (= 1 (gethash awareness-map _LEFT_BUMPER_DATA_KEY))
                  (= 1 (gethash awareness-map _RIGHT_BUMPER_DATA_KEY))))

        (setq direct-harm-degree 0.9)

        (log-ui tag "Input: (awareness-map learning-p reset-first-p allow-fear-interrupt-p decay-all-features-p executing-pattern-name" 
                    (list awareness-map learning-p reset-first-p allow-fear-interrupt-p decay-all-features-p executing-pattern-name))
        ; update groups
        (if (and reset-first-p first)
            (progn
              (log-ui tag "resetting groups")
              (group-reset fear-level-group)
              (group-reset reward-seeking-group)
              (group-reset memory-cache-group 1)
              (set first F)))

        (setq short-term-feature F)

        (if learning-p
            (catch
                (progn
                      (log-ui tag "Learning enabled so updating model group")
                      (setq was-originally-stable (empty-list (report-get-warnings (group-get-state-report memory-cache-group))))
                      (setq mode (group-process-input memory-cache-group state-vec))
                      (setq short-term-feature
                            (group-get-focus-feature memory-cache-group))
                      (if (equals "EXTRAPOLATION" mode)
                          (progn
                              (log-ui tag "could not learn input.  " mode)
                              (log-error tag "Failed to learn input.  Short term memory exhausted"))
                          (log-ui tag "Learned input current mode: " mode))
                      (setq state-report
                            (group-get-state-report memory-cache-group))

                      (if (non-empty-list (setq warnings (report-get-warnings state-report)))
                          (progn
                              (log-ui tag "WARNING: inconsistent memory state detected: " (if was-originally-stable "error introduced by processing input " "") state-report " attempting repair " )
                              (log-error "WARNING:" (concat "inconsistent memory state detected: " (if was-originally-stable "error introduced by processing input " "") (string warnings)))
                              (if _STOP_ON_MEMORY_INCONSISTENCY
                                  (progn
                                    (set stop-process 1)
                                    (set finished-p 1)
                                    (log-ui tag "Exiting on inconsistencies")
                                    (return (list F F)))
                                  (group-fix-memory-inconsistencies memory-cache-group))))

                      (set-last-feature short-term-feature)
                      (if (and prev-focus
                               (not (equals prev-focus short-term-feature)))
                          (progn
                              (log-ui tag "Change of focus from: " prev-focus " to " short-term-feature)
                              (if (and was-rewarded-p (not felt-pain-p))
                                  (progn
                                      
                                      (setq copied (feature-copy prev-focus))

                                      (set-feature-base-utility copied
                                                                was-rewarded-p)
                                      (log-ui "Copied reward feature " prev-focus " to reward group forming: " copied " with utility " (get-feature-utility copied)  " and meta " (feature-get-custom-meta-data copied))
                                      (log-ui tag "Created new model " copied " a copy of " prev-focus)
                                      (group-import-feature reward-seeking-group copied 1)))
                              (clear-total-reward-data)))
                      (set prev-focus short-term-feature))
                (progn
                  (log-error "Errors learning model" (first e))
                  (log-ui tag "Errors: " e)
                  (set stop-process 1)
                  (set finished-p 1))))

        (catch
          (progn
              (log-ui tag "Processing fears")
              (group-process-input fear-level-group state-vec)
              (log-ui tag "Processing hope")
              (group-process-input reward-seeking-group state-vec))
          (progn
            (log-error "Errors procssing" (first e))
            (log-ui tag "Errors: " e)
            (set stop-process 1)
            (set finished-p 1)))


        (multiple-bind (reward-feature avoidance-feature)
                       (list (group-get-focus-feature reward-seeking-group)
                             (group-get-focus-feature fear-level-group)))
        (if decay-all-features-p
            (progn
                (log-ui tag "Decaying reward seeking group")
                (for f 
                     (group-get-ordered-processed-features reward-seeking-group)
                     F
                     (progn
                        (group-decrease-feature-value-fraction reward-seeking-group f standard-pattern-decay)
                        (if (setq remove-p
                                  (decay-feature-model-utility f MINIMUM-UTILITY))
                            (progn
                                (log-ui tag "Removed reward seeking feature " f " due to obselescence")
                                (group-remove-feature reward-seeking-group f)))))
                (log-ui tag "Decaying harm avoiding group")
                (for f 
                     (group-get-ordered-processed-features fear-level-group)
                     F
                     (progn
                        (group-decrease-feature-value-fraction fear-level-group f standard-pattern-decay)
                        (if (setq remove-p
                                  (decay-feature-model-utility f MINIMUM-UTILITY))
                            (progn
                                (log-ui tag "Removed harm avoiding feature " f " due to obselescence")
                                (group-remove-feature fear-level-group f)))
                        ))))

        ; Assess user's utility assignment
        (set current-utility (get-seek-value utility-bar))

        (setq value current-utility)
        (setq fear-level
             (max 0 (min 5 (- value 5))))

        (setq happiness-level
              (max 0 (- 5 value)))

        (if (> fear-level 0)
            (show-toast-off-thread (concat "Fear Level: " fear-level))
            (if (> happiness-level 0)
                (show-toast-off-thread (concat "happiness Level: " happiness-level))
                (show-toast-off-thread "Neutral")))

        (setq fear-level-fraction
              (/ fear-level 5))

        (setq happiness-level-fraction
              (/ happiness-level 5))

        (multiple-bind (user-positive-utility user-negative-utility)
                       (list happiness-level-fraction fear-level-fraction))

        (log-ui tag "User utility: happiness - " user-positive-utility " saddness - " user-negative-utility)

        (if (and interrupt-enabled-p 
                  (<= SEVERE-USER-FEAR-THRESHOLD user-negative-utility))
            (progn
                (log-ui tag "User Interrupt!!")
                (declare-harm)
                (if short-term-feature
                    (progn
                        (setq copied (feature-copy short-term-feature))

                        (feature-force-complete copied)
                        (set-feature-base-utility copied
                                                  user-negative-utility)
                        
                        (log-ui tag "Copied focus into new model due to user unhappiness: " copied " with utility " (get-feature-utility copied) " and meta " (feature-get-custom-meta-data copied))
                        (group-import-feature fear-level-group copied 1)))

                ; enhance the features that predicted this
                ; TODO: use Memory Report data to get the matching features
                (if avoidance-feature
                    (for other-model
                         (find x (group-get-ordered-processed-features fear-level-group) (equals (feature-get-state x) "MATCHING"))
                         F
                         (progn
                            (group-increase-feature-value-fraction fear-level-group 
                                                                   other-model 
                                                                   FEAR-CONFIRMATION-ENHANCEMENT-FRACTION)
                            (log-ui tag (concat "fear group confirmed by negative event: " other-model))
                            (if update-activation-probability-of-confirmed-fear-p
                                (set-feature-base-utility other-model user-negative-utility)))))
                
                (if reset-user-utility-on-interrupt-p
                    (reset-user-interrupt))
                (if on-process-step-lambda (funcall on-process-step-lambda step-response))
                (return (list 1 (list user-positive-utility user-negative-utility))))
            (progn
                ; sort short term memory based on user utility
                ; The robot has done something that is either "paintful" to the robot or has
                ; elicited either a positive or negative response from the user
                (if (and short-term-feature
                         (or (> user-positive-utility 0)
                             direct-harm-detection-p
                             (> user-negative-utility 0)))
                    (progn

                        (if (and (> user-positive-utility 0) 
                                 (or (not direct-harm-detection-p)
                                     (< direct-harm-degree user-positive-utility)))
                            (progn
                               (declare-reward user-positive-utility)
                               (log-ui tag "moving short term memory to reward group " (if direct-harm-detection-p
                                                                                           (concat " but with some pain: " (string direct-harm-degree))
                                                                                           ""))
                               (if reset-user-utility-on-interrupt-p
                                   (reset-user-interrupt))

                               (if executing-pattern-name
                                   (progn
                                        (update-pattern-preferences executing-pattern-name)
                                        ; twice to speed up learning
                                        (update-pattern-preferences executing-pattern-name)
                                        (log-ui tag "Reinforcing " executing-pattern-name " total prefs" (get-current-pattern-preferences))
                                        (log-info "tag" (concat "Reinforcing " executing-pattern-name)))))
                            (progn
                               
                               (setq copied
                                      (feature-copy short-term-feature))
                               
                               (declare-harm)
                               (if (> user-negative-utility 0)
                                   (if reset-user-utility-on-interrupt-p
                                       (reset-user-interrupt)))

                               (set-feature-base-utility copied 
                                                         (or direct-harm-degree
                                                             user-negative-utility))
                               (log-ui tag "Copied " short-term-feature " as avoidance snapshot to get: " copied " with utility " (get-feature-utility copied) " custom " (feature-get-custom-meta-data copied))
                               (group-import-feature fear-level-group copied 1)))
                        ; copy feature instead of moving it?
                        ))
                (if direct-harm-detection-p
                    (progn
                      (if avoidance-feature
                          (for other-model
                               (find x (group-get-ordered-processed-features fear-level-group) (equals (feature-get-state x) "MATCHING"))
                               F
                               (progn
                                  (group-increase-feature-value-fraction fear-level-group 
                                                                         other-model 
                                                                         FEAR-CONFIRMATION-ENHANCEMENT-FRACTION)
                                  (log-ui tag (concat "fear group confirmed by negative event: " other-model))
                                  (if update-activation-probability-of-confirmed-fear-p
                                      (set-feature-base-utility other-model direct-harm-degree)))))
                      ))
                
                ; Handling expectations of reward or punishment
                ; behavior-activation-probs is a pair which basically reflects the robot's
                ; anticipation of reward (first behavior-activation-probs) versus
                ; harm (second behavior-activation-probs)
                ; (0 0) means the robot has no expectation of either reward or punishment
                (setq behavior-activation-probs
                      (list (or (and reward-feature
                                     (or (> user-positive-utility 0)
                                         (get-feature-utility reward-feature)))
                                0)
                            (setq fear-activation-prob
                                  (or (and avoidance-feature
                                           (get-feature-utility avoidance-feature))
                                      (> user-negative-utility 0)
                                      0))))

                (setq has-expecations
                      (or reward-feature avoidance-feature))

                ; returns either AVOIDANCE-BEHAVIOR = 1 or SEEKING-BEHAVIOR = 0
                ; This indicates whether robot expects something good (SEEKING-BEHAVIOR) or bad (AVOIDANCE-BEHAVIOR) to 
                ; occur if this sensorimotor pattern is continued
                (setq preferred-behavior
                      (sample-index-probabilistically behavior-activation-probs))

                (if has-expecations
                    (progn
                        (log-ui tag "Competing beliefs ({reward} {harm}): " (string behavior-activation-probs))
                        (log-ui tag "Expectations this time: " (if (= AVOIDANCE-BEHAVIOR preferred-behavior) "harm" "reward") " " (list reward-feature avoidance-feature)))
                    (log-ui tag "no reward or harm expectations"))

                (if (and (not direct-harm-detection-p) has-expecations)
                    (if (= AVOIDANCE-BEHAVIOR preferred-behavior)
                        (progn
                            ; predicting harm but whether we act upon this prediction depends on the strength
                            ; of fear times the probability of facing the danger concerned
                            (if (< (random 0 1)
                                   (second behavior-activation-probs)) 
                                (progn
                                    (if allow-fear-interrupt-p
                                        (log-ui tag "Acting upon fear by interrupting current motor pattern")
                                        (log-ui tag "Should act upon fear but fear is not allowed to interrupt motor pattern: " (or executing-pattern-name "unknown")))
                                    
                                    (if allow-fear-interrupt-p
                                        (remove-harm-expecting-model avoidance-feature))
                                    (if on-process-step-lambda (funcall on-process-step-lambda step-response))
                                    (and allow-fear-interrupt-p
                                         (list F)))
                                (progn
                                    (log-ui tag "Expect harm but not acting upon fear")
                                    (speak "ignoring fears")
                                    (add-harm-expecting-model avoidance-feature)
                                    (if on-process-step-lambda (funcall on-process-step-lambda step-response))
                                    F)))
                        (progn
                          (add-reward-expecting-model reward-feature)
                          (if on-process-step-lambda (funcall on-process-step-lambda step-response))
                          F))
                    (progn
                        (if direct-harm-detection-p
                            (log-ui tag "Direct pain interrupt: " direct-harm-degree))
                        (and direct-harm-detection-p
                             (list 1))))))))

  ; [action-step-lambda] - lambda that takes an action step name and an interrupt-lambda.  Should return an updated state-vector, updated awareness-map
  ;                        and a boolean parameter indicating if the behavior should be interrupted.  This interrupt is interpretted as a pain signal
  ; [utility-provider-lambda] - the utility lambda is a representation of the user's utility.  This lambda function takes no arguments and returns a list
  ;                             (reward-probability fear-probability)
  ; [action-step-interrupt-lambda] - lambda function that takes the last action step and an  awareness-map as input and which returns true if a pain event (collision) occurs
  ; [default-behavior-pattern-lambda] - lambda function that takes the previous action and most recent awareness-map and returns a new action step name
  ; [seed-pattern] - can be a sequence of action-names or scalar action name.  When it is a scalar, it is considered to be the predicated next action
  ;                  from reward seeking behavior
  (defun run-autonomously (pattern-map action-step-lambda seed-pattern-iterator utility-provider-lambda default-behavior-pattern-lambda action-step-interrupt-lambda max-steps enable-learning-p can-act-upon-learning-p on-complete-lambda)

    (setq reward-input F)
    (if (and seed-pattern-iterator (setq reward-input (not (lambda-p seed-pattern-iterator))))
        (setq seed-pattern-iterator
              (get-list-iterator-with-index (list seed-pattern-iterator))))

    (setq awareness-map (make-string-hashtable))

    (if (not default-behavior-pattern-lambda)
        (setq default-behavior-pattern-lambda
              (lambda (previous-action awareness-map)
                  _ACTION_KEY_FORWARD)))

    (setq collision-distance-threshold 15)

    (if (not action-step-interrupt-lambda)
        (setq action-step-interrupt-lambda
              (lambda (last-action awareness-map)
                  (or (= 1 (gethash awareness-map _LEFT_BUMPER_DATA_KEY))
                      (<= (gethash awareness-map _ULTRA_SONIC_DATA_KEY) collision-distance-threshold)
                      (= 1 (gethash awareness-map _RIGHT_BUMPER_DATA_KEY))))))

    (if (not action-step-lambda)
        (setq action-step-lambda
              (lambda (action-name interrupt-lambda)
                  (log-ui (eval tag) "Executing" action-name)
                  (do-action-step action-name action-step-milli awareness-map action-step-interrupt-lambda 1)
                )))

    (setq INITIAL-PAIN-INTERRUPT-PROB 0.5)
    (setq FEAR-CONFIRMATION-ENHANCEMENT-FRACTION 0.2)
    (setq interrupt-p F)
    (setq finished-p F)
    (setq process-key (concat "running action " (unique-id)))
    (setq AVOIDANCE-BEHAVIOR 1)
    (setq SEEKING-BEHAVIOR 0)
    (setq SEVERE-USER-FEAR-THRESHOLD 0.9)
    (setq update-activation-probability-of-confirmed-fear-p F)
    (setq standard-pattern-decay 0.1)
    

    (setq state EXECUTING-SIMPLE-PATTERN)

    (if reward-input
        (setq state REWARD-SEEKING-STATE))

    (setq tag
          '(concat state " robot-step: " (integer i)))

    (defmacro updated-changed-state-p (new-state)
        `(and (not (equals ,new-state state))
              (set state ,new-state)))

    (defun assert-boundary (all)
        (if (not enable-learning-p)
            (return F))
        (group-reset memory-cache-group)
        (if all
            (progn
                (group-reset fear-level-group)
                (group-reset reward-seeking-group))))

    (defun select-another-motor-pattern (current)
        (setq step-list
              (gethash pattern-map
                       (random-select (or (non-empty-list (find key 
                                                                (get-hash-keys pattern-map) 
                                                                (not (equals current (first (gethash pattern-map key))))))
                                         (get-hash-keys pattern-map)))))
        (get-pattern-iterator step-list))

    (evaluate-background process-key
                          (lambda (next-action e) 
                                  (if e 
                                      (log-error "errors" e)
                                      
                                      )
                                  (if on-complete-lambda
                                      (funcall on-complete-lambda next-action e)))

                          (setq current-action _ACTION_KEY_STOP)

                          (if (and seed-pattern-iterator (not reward-input))
                              (progn
                                  (assert-boundary 1)
                                  (log-ui "run-autonomously" "Asserting boundary for all due to seed-pattern input")
                                  ))

                          (setq recognition-result F)
                          (if (and seed-pattern-iterator
                                   (setq seed-pattern
                                         (funcall seed-pattern-iterator)))
                              (multiple-bind (current-action seed-pattern)
                                             (list (first seed-pattern)
                                                   (rest seed-pattern))))

                          (setq fear-ignore-count 0)

                          (setq i 0)

                          (multiple-bind (short-term-feature avoidance-feature reward-feature)
                                         (list F F F))
                          (setq prior-state F)

                          (unless (or interrupt-p
                                      (and max-steps
                                           (< max-steps i)))
                              (setq i (+ 1 i))
                              (setq update-boundary-p F)
                              ; execute current action
                              (multiple-bind (state-vec sensory-map pain-interrupt-p) 
                                             (funcall action-step-lambda current-action action-step-interrupt-lambda))
                              
                              (log-ui (eval tag) "Next input: " (get-robot-logical-state state-vec _STANDARD_ROBOT_CONFIG))
                              (set prior-state state-vec)
                              (if enable-learning-p
                                  (progn
                                      (log-ui (eval tag) "learning enabled." memory-cache-group state-vec)
                                      ; update LSTMs
                                      (setq mode (group-process-input memory-cache-group state-vec))
                                      (log-ui (eval tag) "Getting focus")
                                      (if (setq short-term-feature
                                                (group-get-focus-feature memory-cache-group))
                                           (log-ui (eval tag) (concat "short term memory: " (string short-term-feature))))

                                      (log-ui (eval tag) "learning enabled: fear " fear-level-group)
                                      (log-ui (eval tag) "learning enabled: reward " reward-seeking-group)
                                      (group-process-input fear-level-group state-vec)
                                      (group-process-input reward-seeking-group state-vec)

                                      (multiple-bind (reward-feature avoidance-feature)
                                                     (list (group-get-focus-feature reward-seeking-group)
                                                           (group-get-focus-feature fear-level-group)))

                                      (if DECAY-ALL-FEATURES-P
                                          (progn
                                              (log-ui (eval tag) "decay-all-features-p")
                                              (for f 
                                                   (group-get-ordered-processed-features reward-seeking-group)
                                                   F
                                                   (group-decrease-feature-value-fraction reward-seeking-group f standard-pattern-decay))
                                              (for f 
                                                   (group-get-ordered-processed-features fear-level-group)
                                                   F
                                                   (group-decrease-feature-value-fraction fear-level-group f standard-pattern-decay))))))

                              
                              (multiple-bind (user-positive-utility user-negative-utility)
                                             (or (and utility-provider-lambda
                                                      (funcall utility-provider-lambda))
                                                 (list F F)))

                              (if (or pain-interrupt-p 
                                      (and user-negative-utility 
                                           (<= SEVERE-USER-FEAR-THRESHOLD user-negative-utility)))
                                  (progn
                                      (log-ui (eval tag) "pain-interrupt-p or user punishment")
                                      (set state
                                           HARM-RECOVERY-STATE)

                                      (log-ui (eval tag) "pain event occurred")
                                      (if short-term-feature
                                          (progn
                                              (log-ui (eval tag) "short-term-feature")
                                              (feature-force-complete short-term-feature)
                                              (group-remove-feature memory-cache-group short-term-feature)
                                              (set-feature-base-utility short-term-feature 
                                                                   (or user-negative-utility INITIAL-PAIN-INTERRUPT-PROB))
                                              (group-import-feature fear-level-group short-term-feature 1)))
                                      ; enhance value of fears confirmed
                                      (if avoidance-feature
                                          (progn
                                              (log-ui (eval tag) "avoidance feature")
                                              (group-increase-feature-value-fraction fear-level-group 
                                                                                     avoidance-feature 
                                                                                     FEAR-CONFIRMATION-ENHANCEMENT-FRACTION)
                                              (log-ui (eval tag) (concat "fear group confirmed by negative event: " avoidance-feature))
                                              (if update-activation-probability-of-confirmed-fear-p
                                                  (set-feature-base-utility avoidance-feature (or user-negative-utility INITIAL-PAIN-INTERRUPT-PROB)))))
                                      ; Select a different action pattern that starts differently from last action
                                      (log-ui (eval tag) "selecting another action")
                                      (set seed-pattern-iterator
                                            (select-another-motor-pattern current-action))
                                     
                                      (multiple-bind (current-action index)
                                                     (funcall seed-pattern-iterator)))
                                  (progn
                                      (if (and short-term-feature
                                               can-act-upon-learning-p
                                               (or (and user-positive-utility (> user-positive-utility 0))
                                                   (and user-negative-utility (> user-negative-utility 0))))
                                          (progn
                                              (log-ui (eval tag) "user is rewarding or punishing me")

                                              (feature-force-complete short-term-feature)
                                              (set-feature-base-utility short-term-feature 
                                                                    (or user-positive-utility
                                                                        user-negative-utility))
                                              (if user-positive-utility
                                                  (progn
                                                     (log-ui (eval tag) "moving short term memory to reward group")
                                                     (group-import-feature reward-seeking-group short-term-feature 1))
                                                  (progn
                                                     (log-ui (eval tag) "moving short term memory to avoidance group")
                                                     (group-import-feature fear-level-group short-term-feature 1)))
                                              (group-remove-feature memory-cache-group short-term-feature)))

                                      (setq behavior-activation-probs
                                            (list (or (and reward-feature
                                                           (or user-positive-utility
                                                               (get-feature-utility reward-feature)))
                                                      0)
                                                  (setq fear-activation-prob
                                                        (or (and avoidance-feature
                                                                 (get-feature-utility avoidance-feature))
                                                            user-negative-utility
                                                            0))))

                                      (log-ui (eval tag) "Overall pleasure/pain: " behavior-activation-probs)

                                      (if (or avoidance-feature reward-feature)
                                          (if (= SEEKING-BEHAVIOR (sample-index-probabilistically behavior-activation-probs))
                                              (progn
                                                  (log-ui (eval tag) "Hope of reward wins")
                                                  (updated-changed-state-p REWARD-SEEKING-STATE)
                                                  
                                                  (log-ui (eval tag) (concat "Acting upon reward seeking impulse: " current-action))
                                                  (setq predicted-logical-state
                                                        (get-robot-logical-state (feature-get-next-predicted-value reward-feature) _STANDARD_ROBOT_CONFIG))
                                                  (setq current-action
                                                        (gethash predicted-logical-state _PREV_ACTION_DATA_KEY))
                                                  (log-ui (eval tag) (concat "Acting upon reward seeking impulse: " current-action)))
                                              (progn
                                                  (log-ui (eval tag) "Fear of harm wins")
                                                  (if (setq action-interrupt-p
                                                            (<= (random 0 1) (nth behavior-activation-probs AVOIDANCE-BEHAVIOR)))
                                                      (progn
                                                          (updated-changed-state-p HARM-AVOIDANCE-STATE)
                                                          (log-ui (eval tag) (concat "Acting upon " (string (integer (* 100 fear-activation-prob))) "% fear avoidance "))
                                                          (set seed-pattern-iterator
                                                               (select-another-motor-pattern current-action))
                                                          (multiple-bind (current-action seed-pattern)
                                                                         (funcall seed-pattern-iterator)))
                                                      (log-ui (eval tag) (concat "Ignoring " (string (integer (* 100 fear-activation-prob))) "% fear avoidance ")))))
                                          (if (and seed-pattern-iterator
                                                   (setq seed-pattern
                                                         (funcall seed-pattern-iterator)))
                                              (progn
                                                  
                                                  (log-ui (eval tag) (concat "continuing motor pattern: " seed-pattern))
                                                  (multiple-bind (current-action seed-index)
                                                                 seed-pattern))
                                              (if default-behavior-pattern-lambda
                                                  (progn
                                                      (log-ui (eval tag) "default behavior")
                                                      (if (updated-changed-state-p INATE-BEHAVIOR-PATTERN-STATE)
                                                          (assert-boundary 1))
                                                      (set seed-pattern-iterator F)
                                                      (setq current-action
                                                            (funcall default-behavior-pattern-lambda current-action sensory-map))
                                                      (log-ui (eval tag) (concat "Selected next default behavior policy action: " current-action)))
                                                  (progn
                                                      (log-ui (eval tag) "selecting a random motor pattern")
                                                      (if (updated-changed-state-p EXECUTING-SIMPLE-PATTERN)
                                                          (assert-boundary 1))
                                                      (set seed-pattern-iterator
                                                           (select-another-motor-pattern))
                                                      (log-ui (eval tag) (concat "Executing new motor pattern: " seed-pattern))
                                                      (multiple-bind (current-action seed-pattern)
                                                                     (funcall seed-pattern-iterator)))))))))
                          (stop-moving)
                          (if (and max-steps (< max-steps i))
                              (progn (log-ui "Stopped" "Finished due to max steps exceeded")
                                     (log-info "Stopped" "Finished due to max steps exceeded")))

                          )

  (lambda ()
    (set interrupt-p 1)
    (log-info "Autonomous behavior" "interrupted")
    (break-process process-key)
    (stop-moving)))

  ; ******************************************************************************
  ;                Robot Full Screen Graphical Interface
  ; ******************************************************************************

  (setq _DEFAULT_LARGE_ICON_SIZE_DP 75)
  (setq _DEFAULT_ICON_SIZE_DP 50)
  (setq _DEFAULT_SMALL_ICON_SIZE_DP 35)
  (setq _DEBUG_P 1)
  (setq _MATCH_PARENT "match_parent")
  (setq _WRAP_CONTENT "wrap_content")
  (setq _MAIN_SCREEN_BACKGROUND_COLOR "#FFF7EB")
  (setq _NAVIGATION_PANEL_BACKGROUND_COLOR "#5D5D66")
  (setq _GLOBAL_DROPBOX_BASE_PATH "/speechbot")

(defun show-journal-dialog ()

      (defun get-log-item (data)
        (multiple-bind (time-str tag message color)
                       data)
        (setq color (or color _JOURNAL_DEFAULT_TAG_COLOR))

        (vertical-layout :width _MATCH_PARENT
                         :height _WRAP_CONTENT
                         (horizontal-layout :width _MATCH_PARENT
                                            :height _WRAP_CONTENT
                                           (text :width "50%"
                                                 :height _WRAP_CONTENT
                                                 :text-size 16
                                                 :text-style "bold"
                                                 :text-align "left"
                                                 :text-color color
                                                 tag)
                                           (text :width "50%"
                                                 :height _WRAP_CONTENT
                                                 :text-size 12
                                                 :text-style "bold"
                                                 :text-color "#022E00"
                                                 :text-align "right"
                                                 time-str))
                         
                         (text :width _MATCH_PARENT
                               :height _WRAP_CONTENT
                               :text-size 12
                               :text-color "#000373"
                               message)))

      (defun update-log-list ()
        (update-list-items log-list-view
                            (mapcar entry
                                    (get-current-journal)
                                    (get-log-item entry))))


    (setq log-list-view
          (listview :width _MATCH_PARENT
                    :height "100%"
                    :reverse-list "true"
                    (mapcar entry
                            (get-current-journal)
                            (get-log-item entry))))

    (defun create-color-spinner-item (color)
      (setq text-color "#000000")
      (list (if color
                (text color
                      :background-color color
                      :text-color text-color)
                (text "None"))
            (if color
                (text color
                      :background-color color
                      :text-color text-color)
                (text "None"))
            (lambda ()
                (set selected-color color))))

    (setq selected-color _JOURNAL_DEFAULT_TAG_COLOR)

    (defun get-selected-color ()
      selected-color
      )


    (dialog :dialog-title "World Notes"
            (vertical-layout :width _MATCH_PARENT
                           :height _MATCH_PARENT
                           :padding 15
                           log-list-view
                           (setq log-tag
                                 (edit :width _MATCH_PARENT
                                       :height _WRAP_CONTENT
                                       :singleline "true"
                                       :hint "Tag"
                                       :margin-bottom 5))
                           (setq clear-check
                                 (checkbox "Clear tag on new entry"
                                           :margin-bottom 5))
                           (spinner :width _MATCH_PARENT
                                    :height _WRAP_CONTENT
                                     :spinnerMode "dropdown"
                                     (mapcar color
                                             (list _JOURNAL_ENTRY_RED _JOURNAL_ENTRY_BLUE _JOURNAL_ENTRY_GREEN _JOURNAL_ENTRY_VIOLET)
                                             (create-color-spinner-item color)))
                           (setq log-entry
                                 (edit :width _MATCH_PARENT
                                       :height _WRAP_CONTENT
                                       :hint "Journal entry"))
                           
                           (horizontal-layout :width _MATCH_PARENT
                                              :height _WRAP_CONTENT
                                              (shadow-button "add"
                                                      :on-click (progn (append-current-journal (get-text log-tag)  (get-text log-entry) (get-selected-color)) 
                                                                        (update-log-list)
                                                                        (set-text log-entry "")
                                                                        (if (is-checked clear-check)
                                                                            (set-text log-tag ""))))
                                              (shadow-button "clear"
                                                      :on-click (progn (clear-current-journal) (update-log-list)))))
            :positive-text "Ok"))



  (defun get-robot-metadata-view (state-vec)
      (text (string (sort-strings-ascending (get-hash-keys (get-state-labels-meta-data state-vec))))))

  (defun get-robot-state-view (state-vec config text-color color)
    (setq data-map
          (get-robot-logical-state state-vec config))
    (text :width _MATCH_PARENT
          :height _WRAP_CONTENT
          :text-color text-color
          :background-color color
          (string (mapcar key
                          (sort-strings-ascending (get-hash-keys data-map))
                          (list key (gethash data-map key))))))


  ; Create a view that allows for viewing and limited editing of basic robot state vector data
  (defun get-detailed-robot-state-view (state-vec config text-color color)
    (setq data-map
          (get-robot-logical-state state-vec config))

    (defun refresh-labels ()
        (remove-all-views label-view)
        (add-view label-view (get-robot-metadata-view state-vec)))

    (vertical-layout :width _MATCH_PARENT
                     :height _WRAP_CONTENT
                     :background-color color
                     (text :text-style "bold"
                           :text-size 15
                           "Data:")
                     (text :width _MATCH_PARENT
                           :height _WRAP_CONTENT
                           :text-color text-color
                           
                           (string (mapcar key
                                           (sort-strings-ascending (get-hash-keys data-map))
                                           (list key (gethash data-map key)))))
                     (text :text-style "bold"
                           :text-size 15
                           "Labels:")

                     (setq label-view 
                           (relative :width _MATCH_PARENT
                                     :height _WRAP_CONTENT
                                     (get-robot-metadata-view state-vec)))
                      
                     (horizontal-layout :width _MATCH_PARENT
                                        :height _WRAP_CONTENT
                                        (shadow-button "Add"
                                                       :on-click (if (non-empty (get-text label-edit))
                                                                     (progn
                                                                        (add-label-to-state-meta-data state-vec (get-text label-edit))
                                                                        (refresh-labels))))
                                        (shadow-button "Remove"
                                                       :on-click (if (non-empty (get-text label-edit))
                                                                     (progn
                                                                        (remove-label-from-state-meta-data state-vec (get-text label-edit))
                                                                        (refresh-labels))))
                                        (setq label-edit
                                              (edit :width "100%"
                                                    :hint "state label")))))


  (defun get-standard-robot-state-view (state-vec text-color color)
      (get-robot-state-view state-vec _STANDARD_ROBOT_CONFIG text-color color))


  (setq PASTEL_GREEN
        "PASTEL_GREEN")

  (setq PASTEL_RED
        "PASTEL_RED")

    (setq PASTEL_BLUE
        "PASTEL_BLUE")

  (setq STANDARD_DATA
        "STANDARD_DATA")

    (setq HIGHLIGHT
        "HIGHLIGHT")
  


  (setq VARIANT_MAP
        (make-string-hashtable (list (list PASTEL_GREEN (list "#000000" "#ABFFAC"))
                                     (list PASTEL_RED (list "#000000" "#ABFFAC"))
                                     (list PASTEL_BLUE (list "#000000" "#C5E6FF"))
                                     (list HIGHLIGHT (list "#000000" "#FFEB0E"))
                                     (list STANDARD_DATA (list "#000000" "#FFFFFF")))))


  (defun get-simple-standard-robot-state-view (state-vec variant)
      (get-standard-robot-state-view state-vec 
                                     (first (gethash VARIANT_MAP variant)) 
                                     (second (gethash VARIANT_MAP variant))))


    (defun get-simple-detailed-standard-robot-state-view (state-vec variant)
      (get-detailed-robot-state-view state-vec 
                                     _STANDARD_ROBOT_CONFIG
                                     (first (gethash VARIANT_MAP variant)) 
                                     (second (gethash VARIANT_MAP variant))))

; ~>ooooo<~~>ooooo<~~>ooooo<~~>ooooo<~~>ooooo<~~>ooooo<~~>ooooo<~~>ooooo<~
; State transition view
; ~>ooooo<~~>ooooo<~~>ooooo<~~>ooooo<~~>ooooo<~~>ooooo<~~>ooooo<~~>ooooo<~

(defun show-state-transition-model ()
  (setq seleted-state F)

  (defun load-state-details (state-vec)
    (set seleted-state state-vec)
    (remove-all-views content-view)
    (add-view content-view
              (get-simple-detailed-standard-robot-state-view state-vec STANDARD_DATA)))


  
  (defun get-state-transition-display (current-state next-states)
      (log-ui "get-state-transition-display" current-state (length next-states))
      (horizontal-layout :width _MATCH_PARENT
                         :height _WRAP_CONTENT

                         (relative :width "50%"
                                   :height _WRAP_CONTENT
                                   (get-simple-standard-robot-state-view current-state (if (vequals _PRIOR_STATE current-state) HIGHLIGHT PASTEL_GREEN)))
                         (solid :width 2
                                :height _MATCH_PARENT
                                :background-color "#531018")
                         (vertical-layout :width "50%"
                                          :height _WRAP_CONTENT
                                          (mapcar state
                                                  next-states
                                                  (get-simple-standard-robot-state-view state PASTEL_BLUE)))))

  (setq spec-list ())

  (dialog :dialog-title "State Transition Model"
          (vertical-layout :width _MATCH_PARENT
                           :height _MATCH_PARENT
                           (listview :width _MATCH_PARENT
                                     :height "100%"
                                     :on-item-clicked (load-state-details (first (nth spec-list selected-index)))
                                     (mapcar transition-spec
                                             (set spec-list (get-vmap-entry-list (get-world-state-vectors-meta-data)))
                                             (get-state-transition-display (first transition-spec) (or (get-state-vector-transitions (second transition-spec)) () )  )))
                           (setq content-view
                                 (relative :width _MATCH_PARENT
                                           :height _WRAP_CONTENT))
                           (relative :width _MATCH_PARENT
                                     :height _WRAP_CONTENT
                                     (setq button-set-state
                                           (shadow-button "Set as current state"
                                                   :parent-align "left"
                                                   :on-click (set _PRIOR_STATE seleted-state)))
                                     (shadow-button "Clear prior state"
                                                   :parent-align "right"
                                                   :on-click (set _PRIOR_STATE F))))
                           
          :positive-text "close"))


; -O--O--O--O--O--O--O--O--O--O--O--O--O--O--O--O--O--O--O--O-
;           Log View 
; -O--O--O--O--O--O--O--O--O--O--O--O--O--O--O--O--O--O--O--O-


  (defun show-save-log-dialog (data filename)

      (defun upload-data (file-path)
          (upload-dropbox-file file-path
                               data
                               1
                               (lambda (target-file error)
                                    (if error
                                        (progn
                                          (show-short-toast "Error uploading file")
                                          (log-error "Failure uploading file"
                                                     error))
                                        (log-info "Successfully saved data" (concat "data saved to: " file-path))))))

      (show-dropbox-file-chooser _GLOBAL_DROPBOX_BASE_PATH
                                 (lambda (filename error)
                                      (if error
                                          (progn
                                              (show-short-toast error)
                                              (log-error "save log error" error))
                                          (upload-data filename)))))


  (setq log-list ())

  (setq log-list-view
        (listview :width _MATCH_PARENT
                  :height "100%"
                  :reverse-list "true"
                  ()
                  ))

  (defun get-log-item (tag message datetime)
    (vertical-layout :width _MATCH_PARENT
                     :height _WRAP_CONTENT
                     (horizontal-layout :width _MATCH_PARENT
                                        :height _WRAP_CONTENT
                                        (text :width "50%"
                                              :height _WRAP_CONTENT
                                              :text-size 12
                                              :text-color "#3F00BE"
                                              (get-datetime-string datetime))
                                        (text :width "50%"
                                              :height _WRAP_CONTENT
                                              :text-size 16
                                              :text-style "bold"
                                              :text-align "right"
                                              :text-color "#022E00"
                                              tag))
                     (text :width _MATCH_PARENT
                           :height _WRAP_CONTENT
                           :text-size 12
                           :text-color "#000373"
                           message)))

  (defun update-log-list ()
      (update-list-items log-list-view
                          (mapcar x
                                  log-list
                                  (get-log-item (first x) (second x) (last x)))))

  (defun add-log-line (tag data datetime)
      (set log-list
           (append-item log-list
                        (list tag data (or datetime (time))))))

  (defun log-ui (tag ...)
    (setq data (join (mapcar x ... (if (string-p x) x (string x))) " "))
    (add-log-line tag data (time)))

  (defun get-log-string ()
    (join (mapcar log 
                  log-list 
                  (progn
                      (multiple-bind (tag data time) log)
                      (concat (get-datetime-string time) " [" tag "] " data)))
          (newline)))
                 


  (setq log-view
        (vertical-layout :width _MATCH_PARENT
                         :height _MATCH_PARENT
                         :padding 15
                         log-list-view
                         (setq log-entry
                               (edit :width _MATCH_PARENT
                                     :height _WRAP_CONTENT
                                     :singleline "true"
                                     :hint "custom entry"))
                         (horizontal-layout :width _MATCH_PARENT
                                            :height _WRAP_CONTENT
                                            (shadow-button "add"
                                                    :on-click (progn (add-log-line "user entry"  (get-text log-entry) (time)) 
                                                                      (update-log-list)
                                                                      (set-text log-entry "")))
                                            (shadow-button "clear"
                                                    :on-click (progn (set log-list ()) (update-log-list)))
                                            (shadow-button "save-log"
                                                    :on-click (show-save-log-dialog (get-log-string))))))

; <>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>
;       Create World Settings View
; <>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>o<>
  (setq current-world-type-name _STANDARD_ROBOT_WORLD_TYPE)

  (setq _ROBOT_TYPE_NAMES 
        (list _STANDARD_ROBOT_WORLD_TYPE))

  (defun show-world-create-dialog ()
      (setq type F)
      (defun create-type-spinner-item (type-name)
          (list (text type-name)
                (text type-name)
                (lambda ()
                    (show-short-toast (concat "Selected: " type-name))
                    (set type type-name))))

      (dialog :dialog-title "create new world"
          (vertical-layout :width _MATCH_PARENT
                           :height _MATCH_PARENT
                           (horizontal-layout :width _MATCH_PARENT
                                              :height _WRAP_CONTENT
                                (text "Type:")
                                (spinner :width "100%"
                                         (mapcar name
                                                 _ROBOT_TYPE_NAMES
                                                 (create-type-spinner-item name))))
                           (horizontal-layout :width _MATCH_PARENT
                                              :height _WRAP_CONTENT
                                (text "Name:")
                                (setq edit-name
                                      (edit :width "100%"
                                            :height _WRAP_CONTENT
                                            :hint "world name"
                                            :singleline "true")))
                           (horizontal-layout :width _MATCH_PARENT
                                              :height _WRAP_CONTENT
                                (text "Max feature allocation:")
                                (setq edit-allocation
                                      (edit :width "100%"
                                            :height _WRAP_CONTENT
                                            :hint "max lstms"
                                            :singleline "true")))
                           (text "World Description:")
                           (setq edit-desc
                                 (edit :width _MATCH_PARENT
                                       :height _WRAP_CONTENT
                                       :hint "description of world")))
          :cancel-text "Cancel"
          :positive-text "Create robot"
          :on-positive (create-new-world (get-text edit-name)
                                         (get-text edit-desc)
                                         (integer (get-text edit-allocation))
                                         type
                                          (lambda (result error)
                                              (if error
                                                  (progn
                                                    (log-error "error creating world" error)
                                                    (return)))
                                              (log-ui "settings world desc: " current-world-desc)
                                              (set-text text-world-desc current-world-desc)
                                              (log-ui "settings world name: " current-world-name)
                                              (set-text text-world-name current-world-name)
                                              (log-ui "settings world allocation: " current-world-allocation)
                                              (set-text text-world-alloc (string current-world-allocation))))))

  (defun show-meta-state-dialog ()
      (dialog :dialog-title "State Parameters"
              :positive-text "Okay"
              (vertical-layout :width _MATCH_PARENT
                               :height _WRAP_CONTENT
                               (horizontal-layout :width _MATCH_PARENT
                                                  :height _WRAP_CONTENT
                                                  (text "Exploration actions: "
                                                        :text-style "bold")
                                                  (text :width _MATCH_PARENT
                                                        :height _WRAP_CONTENT
                                                        (string _EXPLORATORY_PATTERNS)))
                               (horizontal-layout :width _MATCH_PARENT
                                                  :height _WRAP_CONTENT
                                                  (text "Recovery actions: "
                                                        :text-style "bold")
                                                  (text :width _MATCH_PARENT
                                                        :height _WRAP_CONTENT
                                                        (string _HARM_RECOVERY_PATTERNS))))
        )

    )

  (defun show-load-world-dialog ()

    (multiple-bind (name desc alloc type)
                   (list F F F F))

    (defun select-world (index)
        (setq selected-name
              (nth robot-names index))

        (multiple-set (name desc alloc type)
                      (restore-world-metadata selected-name))
        (set-text text-name name)
        (set-text text-alloc (string alloc))
        (set-text text-desc desc))

    (defun get-list-item (world-name)
        (text world-name :width _MATCH_PARENT))

    (setq robot-names
          (or (get-all-names _ROBOT_METADATA_KEY) ()))

    (dialog :dialog-title "Load existing world"
        (vertical-layout :width _MATCH_PARENT
                         :height _WRAP_CONTENT
                         (text "Select world name:")
                         (listview :width _MATCH_PARENT
                                   :height 120
                                   :on-item-clicked (select-world selected-index)
                                   (mapcar name
                                           robot-names
                                           (get-list-item name)))
                         (horizontal-layout :width _MATCH_PARENT
                                            :height _WRAP_CONTENT
                                            (text "name"
                                                  :width "50%")
                                            (setq text-name
                                                  (text :width "50%"
                                                        :height _WRAP_CONTENT)))
                         (horizontal-layout :width _MATCH_PARENT
                                            :height _WRAP_CONTENT
                                            (text "max alloc"
                                                  :width "50%")
                                            (setq text-alloc
                                                  (text :width "50%"
                                                        :height _WRAP_CONTENT)))
                         (relative :width _MATCH_PARENT
                                   :heigth _WRAP_CONTENT
                                   (setq check-delete
                                         (checkbox "Delete"
                                                   :parent-align "left"))
                                   (setq edit-clone-name
                                         (edit :width _WRAP_CONTENT
                                               :height _WRAP_CONTENT
                                               :parent-align "right"
                                               :hint "new name")))
                         
                         (text "Description:")
                         (setq text-desc
                               (text :width _MATCH_PARENT
                                     :height _WRAP_CONTENT)))

        :cancel-text "Cancel"
        :positive-text "Load"
        :on-positive (if name
                         (if (is-checked check-delete)
                             (delete-selected-world name)
                          
                             (restore-to-current-world name
                                                       (lambda ()
                                                          (run-foreground (progn
                                                                            (set-text text-world-desc current-world-desc)
                                                                            (set-text text-world-name current-world-name)
                                                                            (set-text text-world-alloc (string current-world-allocation))
                                                                            (set _PATTERN_MAP (get-custom-motor-patterns))
                                                                            (clear-current-action-steps)
                                                                            (select-action F)
                                                                            (update-pattern-list (sort-strings-ascending (get-hash-keys _PATTERN_MAP)))
                                                                            )))
                                                       (non-empty (get-text edit-clone-name))))
                         (show-short-toast "no world selected"))))

  ; * 
  (defun create-new-world (name desc allocation type on-complete-lambda)
    (multiple-bind (save-world-lambda create-world-lambda load-world-lambda)
                   (setq value (gethash WORLD-TYPE-SAVER-MAP (or type current-world-type-name))))
    (if (not value)
        (progn
          (show-long-toast "undefined parameters")  
          (return)))

    (evaluate-background "creating-new"
                         (lambda (result error)
                              (if (not error)
                                  (run-foreground (update-world-viewer)))
                              (evaluate-foreground F (funcall on-complete-lambda result error)))
                         
                         (funcall create-world-lambda allocation)
                         ; must set this before calling (get-initial-meta-data)
                         (set _INSTRUMENTATION_USE_NEW_MAP 1)
                         (multiple-set (current-world-name current-world-desc current-world-allocation current-world-type-name current-world-custom-meta-data _INSTRUMENTATION_USE_NEW_MAP)
                                       (list name desc allocation (or type current-world-type-name) (get-initial-meta-data) 1))))

  (defun delete-selected-world (name)
      (if (setq clear-p (equals name current-world-name))
          (progn
            (set-text text-world-name "")
            (set-text text-world-alloc "")
            (set-text text-world-desc "")              
            ))

      (show-toast-off-thread "Deleted world")
      (log-ui "settings" "Deleted world: " name)
      (delete-data-value name _ROBOT_CORE_DATA_KEY)
      (delete-data-value name _ROBOT_METADATA_KEY)
      (multiple-set (robot current-world-name current-world-desc current-world-allocation current-world-type-name current-world-custom-meta-data)
                    (list F F F F F F))
      (if clear-p
          (update-world-viewer)))

  (defun restore-to-current-world (name on-complete-lambda new-name)
    (multiple-bind (world-name world-desc world-allocation world-type-name world-custom-meta-data optimization-method)
                   (restore-world-metadata name))

    (multiple-bind (save-world-lambda create-world-lambda load-world-lambda)
                   (gethash WORLD-TYPE-SAVER-MAP world-type-name))

    (if load-world-lambda
        (progn
            (log-ui "settings" "Trying to load " world-name)
            (log-info "setttings" (concat "Trying to load " world-name))
            (setq loading-p 1)
            (setq start-time
                  (time))
            (evaluate-background "loading"
                                 (lambda (result error)
                                        (set loading-p F)
                                        (if error
                                            (log-error "errors loading world" error)
                                            (progn
                                                (multiple-set (current-world-name current-world-desc current-world-allocation current-world-type-name current-world-custom-meta-data _INSTRUMENTATION_USE_NEW_MAP)
                                                              (list (or new-name world-name) world-desc world-allocation world-type-name world-custom-meta-data optimization-method))
                                                (log-info "settings" (concat "loaded world after " (string (integer (- (time) start-time))) " ms"))
                                                (log-ui "settings" (concat "loaded world after " (string (integer (- (time) start-time))) " ms"))
                                                (show-toast-off-thread "Loaded world")
                                                (run-foreground (update-world-viewer))
                                                (get-harm-recovery-actions)
                                                (get-exploration-actions)
                                                (restore-world-prior-state)
                                                (if on-complete-lambda
                                                    (funcall on-complete-lambda)))))
                                 (funcall load-world-lambda world-allocation name)))
        (progn
            (show-short-toast "undefined parameters")
            (log-error "settings" "error loading world due to undefined parameters")
            (log-ui "settings" "error loading world due to undefined parameters"))))


  (setq saving-p F)

  ; * 
  (defun get-vmap-serializable-form (map)
    (if _INSTRUMENTATION_USE_NEW_MAP
        (get-vector-map-keys map)
        map))

  ; * 
  (defun from-vmap-serialized-form (serialized-form)
    (if _INSTRUMENTATION_USE_NEW_MAP
        (make-vector-map serialized-form)
        serialized-form))

  ; * 
  (defun save-world-metadata (name desc allocation type world-custom-meta-data optimization-method)
    (setq context-key _ROBOT_METADATA_KEY)

    (set-data-value name 
                    (list name 
                          desc 
                          allocation 
                          type 
                          (make-string-hashtable (mapcar meta-key 
                                                         (get-hash-keys world-custom-meta-data)
                                                         (list meta-key
                                                               (if (equals meta-key
                                                                           _META_DATA_STATE_META_KEY)
                                                                   (get-vmap-serializable-form (gethash world-custom-meta-data meta-key))
                                                                   (gethash world-custom-meta-data meta-key)))))
                          optimization-method)
                    context-key))

  ; * 
  (defun restore-world-metadata (name)
      (set current-world-custom-meta-data F)

      (setq context-key _ROBOT_METADATA_KEY)
      (multiple-bind (world-name world-desc world-allocation world-type-name world-custom-meta-data optimization-method)
                     (get-data-value name context-key))

      (set _INSTRUMENTATION_USE_NEW_MAP optimization-method)

      (list world-name 
            world-desc 
            world-allocation 
            world-type-name 
            (make-string-hashtable (mapcar meta-key 
                                           (get-hash-keys world-custom-meta-data)
                                           (list meta-key
                                                 (if (equals meta-key
                                                             _META_DATA_STATE_META_KEY)
                                                     (from-vmap-serialized-form (gethash world-custom-meta-data meta-key))
                                                     (gethash world-custom-meta-data meta-key)))))
            optimization-method))

  (defun save-current-world (meta-only-p)
    (multiple-bind (save-world-lambda create-world-lambda load-world-lambda)
                   (gethash WORLD-TYPE-SAVER-MAP current-world-type-name))

    (if (and robot 
             current-world-name 
             save-world-lambda)
        (progn
            (log-ui "settings" "Trying to save " current-world-name)
            (log-info "setttings" (concat "Trying to save " current-world-name))
            (set saving-p 1)
            (setq start-time
                  (time))
            (evaluate-background "saving"
                                 (lambda (result error)
                                        (set saving-p F)
                                        (if error
                                            (log-error "errors saving world" error)
                                            (progn
                                                (log-info "settings" (concat "saved world after " (string (integer (- (time) start-time))) " ms"))
                                                (log-ui "settings" (concat "saved world after " (string (integer (- (time) start-time))) " ms"))
                                                (show-toast-off-thread "Saved world"))))
                                 (if (not meta-only-p)
                                     (progn
                                        (log-ui "settings" "Saving the current groups: " (string (list current-world-name current-world-desc current-world-allocation current-world-type-name)))
                                        (funcall save-world-lambda current-world-name))
                                     (log-ui "settings" "Only saving meta-data for " current-world-name))
                                 
                                 (log-ui "settings" "Saving the current meta-data")
                                 (save-current-prior-state)
                                 (save-world-metadata current-world-name current-world-desc current-world-allocation current-world-type-name current-world-custom-meta-data _INSTRUMENTATION_USE_NEW_MAP)))
        (progn
            (show-short-toast "undefined parameters")
            (log-error "settings" "error saving world due to undefined parameters")
            (log-ui "settings" "error saving world due to undefined parameters"))))




  (setq robot-settings
        (scrollview :width _MATCH_PARENT
                    :height _MATCH_PARENT
        (vertical-layout :width _MATCH_PARENT
                         :height _WRAP_CONTENT
                         :child-align "left"
                         (text "Settings"
                               :text-size 20
                               :text-color "#1C5DF8"
                               :text-style "bold"
                               :parent-align "left")
                         (horizontal-layout :width _MATCH_PARENT
                                            :height _WRAP_CONTENT
                                            (text "World name:"
                                                  :width "50%")
                                            (solid :width 1
                                                   :height _MATCH_PARENT
                                                   :background-color "#E1EBF7")
                                            (setq text-world-name
                                                  (text (or current-world-name "[undefined]")
                                                        :width "50%")))
                         (horizontal-layout :width _MATCH_PARENT
                                            :height _WRAP_CONTENT
                                            (text "Max allocation:"
                                                  :width "50%")
                                            (solid :width 1
                                                   :height _MATCH_PARENT
                                                   :background-color "#E1EBF7")
                                            (setq text-world-alloc
                                                  (text (or (and current-world-allocation (string current-world-allocation)) "")
                                                        :width "50%")))
                         (text "World Description:")
                         (setq text-world-desc
                               (text (or current-world-desc "")
                                     :width _MATCH_PARENT
                                     :height _WRAP_CONTENT))
                         (solid :width _MATCH_PARENT
                                :height 1
                                :background-color "#E1EBF7")
                         (relative :width _MATCH_PARENT
                                   :height _WRAP_CONTENT

                                   (shadow-button "new"
                                                  :text-size 12
                                                  :parent-align "left"
                                                  :on-click (show-world-create-dialog))
                                   (shadow-button "load"
                                                  :text-size 12
                                                  :parent-align "center|top"
                                                  :on-click (show-load-world-dialog))
                                   (vertical-layout :width _WRAP_CONTENT
                                                    :height _WRAP_CONTENT
                                                    :parent-align "right"
                                                    :background (create-background :border-width 1 :foreground-color "#FFFFF4" :border-color "#150035")
                                                    (shadow-button "save"
                                                                   :text-size 12
                                                                   :on-click (save-current-world (is-checked metabox)))
                                                    (setq metabox (checkbox "only meta"))))
                         (relative :width _MATCH_PARENT 
                                   :height _WRAP_CONTENT
                                   (shadow-button "State transitions"
                                                  :on-click (show-state-transition-model)
                                                  :parent-align "left")
                                   (shadow-button "Meta-state"
                                                  :on-click (show-meta-state-dialog)
                                                  :parent-align "right"))


                         (text "Robot state:"
                              :text-size 16
                               :text-color "#1C5DF8"
                               :text-style "bold"
                               :parent-align "left")

                        (setq world-view-container
                              (relative :width _MATCH_PARENT
                                        :height _WRAP_CONTENT))
                        (shadow-button "Refresh"
                                       :parent-align "right"
                                       :on-click (update-world-viewer))


                        )))


(defun update-world-viewer ()
  (remove-all-views world-view-container)
  (if robot
      (add-view world-view-container 
               (standard-robot-world-state-view robot _WRAP_CONTENT))))

; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;             Create Robot Control View
; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
(multiple-bind (test-robot-p recognition-group-names-map learn-from-pain-p)
               (list (or (not (var-exists-p "current-nxt")) 
                         (not current-nxt)) 
                     _RECOGNITION_NAME_MAP 
                     1))
(set _INITIAL_MOTOR_PATTERNS (load-motor-pattern))

(for basic-action
     (list _ACTION_KEY_LEFT _ACTION_KEY_RIGHT _ACTION_KEY_FORWARD _ACTION_KEY_BACKWARD _ACTION_KEY_STOP)
     F
     (defhash _PATTERN_MAP
              basic-action
              (list basic-action)))

  ; ooooooooooooooooooooo
  ; External functions - can be called off main thread
  ; ooooooooooooooooooooo

  (defun ex-update-pattern-list (name-list selected-index)
    (evaluate-foreground (lambda (result error) 
                                (if error
                                    (log-error "ex-update-pattern-list" error)))
                         (update-pattern-list name-list selected-index)))

  (defun update-pattern-list (name-list selected-index)
    (update-list-items pattern-list-view
                       (get-pattern-list-items name-list))
    (if selected-index
        (select-action (nth (get-pattern-list-names) selected-index))))

  (defun ex-append-action-step (step-name)
      (evaluate-foreground (lambda (result error) 
                                (if error
                                    (log-error "ex-append-action-step" error)))
                           (append-action-to-sequence-list step-name)))

  (defun ex-set-recording-state (new-state)
      (evaluate-foreground (lambda (result error) 
                                (if error
                                    (log-error "ex-set-recording-state" error)))
                           (set is-currently-recording-p new-state)
                           (update-recording-state-ui new-state)))

  (defun ex-update-executing-step (action-step step-id)
      (if step-id
          (if (< step-id 
                 (length current-action-steps))
              (evaluate-foreground (lambda (result error) 
                                        (if error
                                            (log-error "ex-update-executing-step" error)))
                                   (update-executing-step step-id))
              (ex-append-action-step action-step))
          (evaluate-foreground (lambda (result error) 
                                    (if error
                                        (log-error "ex-update-executing-step" error)))
                               (set current-executing-step-index F)
                               (update-action-steps current-action-steps))))

  (defun cb-save-current-pattern ()
      (set current-action-pattern-name
           (get-desired-pattern-name))
      (defhash _PATTERN_MAP 
               current-action-pattern-name
               current-action-steps)
      (setq new-items (get-pattern-list-names))
      (setq selected-index
            (map-some (key i) new-items (and (equals key current-action-pattern-name) i)))
      (log-info "updating action items on save"
                (string new-items))
      (update-pattern-list new-items selected-index))

  (defun cb-get-pattern-steps-by-name (pattern-name)
      (if (setq result 
                (non-empty-list (gethash _PATTERN_MAP
                                         pattern-name)))
          (progn
            (setq status-map
                  (get-pattern-meta-data pattern-name))))
      result)

  (defun cb-set-recording-state (new-state)
      (set is-currently-recording-p new-state)
      (if new-state
          (progn
            (set is-recording-p 1)
            (set-text recording-button "STOP")
            (set-text recording-state "RECORDING")
            (show-short-toast "setting recording button: STOP"))
          (progn
            (set is-recording-p F)
            (set-text recording-button "START")
            (set-text recording-state "NOT RECORDING")
            (show-short-toast "setting recording button: START"))))

  (defun cb-delete-action-pattern (name)
    (remhash _PATTERN_MAP name)
    (update-pattern-list))

  (setq current-action-pattern-name F)

  (setq current-action-steps 
        ())

  (setq current-executing-step-index F)

  (setq action-step-list-item-views F)

  (setq selected-action-step-map 
        (make-int-hashtable))

  (setq is-currently-recording-p F)

  (defun update-recording-state-ui (is-recording)
      (if is-recording
          (set-text recording-button "STOP")
          (set-text recording-button "START"))

      (if is-recording
          (set-text recording-state "RECORDING")
          (set-text recording-state "NOT RECORDING"))
      (if is-recording
          (update-parameters recording-state
                             :text-color "green")
          (update-parameters recording-state
                             :text-color "red")))

  (defun clear-pattern-name-text ()
    (set current-action-pattern-name "")
    (set-text pattern-name-edit ""))

  (setq selected-color "#FBEDFB")
  (setq executing-color "#A5FEB4")
  (setq non-executing-color "#D7F6FD")
  (setq divider-color "#0B0F10")

  (defun update-executing-step (step-id)
    (set current-executing-step-index step-id)
    (set-action-step-selection-state (nth action-step-list-item-views step-id) 
                                     step-id 
                                     (gethash selected-action-step-map step-id)))

  (defun toggle-action-step-selection-state (selected-action-step-index)
      (setq new-selection-state F)
      (if (setq prior-selected-view
                (gethash selected-action-step-map
                         selected-action-step-index))
          (progn
            (remhash selected-action-step-map
                     selected-action-step-index)
            (setq new-selection-state F))
          (progn
            (defhash selected-action-step-map
                     selected-action-step-index
                     (setq prior-selected-view
                           (nth action-step-list-item-views selected-action-step-index)))
            (setq new-selection-state 1)))
      (set-action-step-selection-state prior-selected-view selected-action-step-index new-selection-state))

  (defun set-action-step-selection-state (action-step-view step-index is-selected-p)

    (if is-selected-p
        (setq color selected-color)
        (if (= step-index current-executing-step-index)
            (setq color executing-color)
            (setq color non-executing-color)))

    (update-parameters action-step-view
                       1
                       :background (if (< step-index 0)
                                       (create-background :border-top-width 1 :border-color divider-color :foreground-color color )
                                       (create-background :foreground-color color))))

  (defun clear-current-action-steps ()
      (set selected-action-step-map
           (make-int-hashtable))
      (set action-index 0)
      (update-action-steps (set current-action-steps (list ))))

  (defun delete-current-action-pattern ()
      (if (setq defined
                (get-desired-pattern-name))
          (cb-delete-action-pattern defined))
      (clear-current-action-steps))

  (defun set-current-pattern-steps-by-name (pattern-name)
      (set current-action-steps
           (or (cb-get-pattern-steps-by-name pattern-name)
               (list))))

  (defun get-desired-pattern-name ()
      (trim (get-text pattern-name-edit)))

  (defun update-action-steps (step-list executing-index selected-index)
      (update-list-items action-step-list
                         (set action-step-list-item-views
                              (mapcar (step-name i)
                                      step-list
                                      (create-action-step-list-item step-name 
                                                                    i
                                                                    (and executing-index (= executing-index i))
                                                                    (and selected-index (= selected-index i)))))))

  (defun create-action-step-list-item (action-step-name index is-executing-p is-selected-p)
    (setq item-color
          (if is-selected-p
              selected-color
              (if is-executing-p
                  executing-color
                  non-executing-color)))

    (set-id (text action-step-name
                  :width _MATCH_PARENT
                  :height _WRAP_CONTENT
                  :background (if (> index 0)
                                  (create-background :border-top-width 1 :border-color divider-color :foreground-color item-color)
                                  (create-background :foreground-color item-color)))
            index))

(defun create-pattern-list-item (pattern-name index)
    (set-id (text pattern-name
                  :width _MATCH_PARENT
                  :height _WRAP_CONTENT)
            index))


  (defun append-action-to-sequence-list (action-id)
      (set current-action-steps
           (append-item current-action-steps
                        action-id))

      (set current-executing-step-index (- (length current-action-steps) 1))
      (update-action-steps current-action-steps current-executing-step-index))

  (defun get-pattern-list-names ()
    (sort-strings-ascending (get-hash-keys _PATTERN_MAP)))


  (defun select-action (action-id)
    (set-current-pattern-steps-by-name action-id)
    (set current-action-pattern-name action-id)
    (set current-executing-step-index F)
    (set selected-action-step-map 
         (make-int-hashtable))
    (set-text pattern-name-edit (or action-id ""))
    (update-action-steps current-action-steps))

  (defun get-pattern-list-items (name-list)
    (mapcar (name i)
            (or name-list (get-pattern-list-names))
            (create-pattern-list-item name i)))

  (defun execute-current-pattern-name (step-update-lambda on-complete-lambda)
    (if (or (empty-list (get-desired-pattern-name)) (empty-list current-action-steps))
        (show-short-toast "undefined action steps or pattern name")
        (background-run-pattern-name (get-desired-pattern-name) step-update-lambda on-complete-lambda)))

  (defun execute-current-pattern (step-update-lambda on-complete-lambda)
    (if (empty-list current-action-steps)
        (show-short-toast "undefined action steps")
        (background-run-pattern-iterator (get-list-iterator-with-index current-action-steps) step-update-lambda on-complete-lambda)))

  (setq run-header
        (relative :width _MATCH_PARENT
                  :height _WRAP_CONTENT))

  (setq _LOG_INDEX 0)
  (setq _LOG_TAG "")
  (setq _LOG_MESSAGE "")

  (defun log-index (tag message index)
    (set _LOG_INDEX
          (or index _LOG_INDEX))

    (log-ui (set _LOG_TAG (or tag _LOG_TAG))
           (concat  (or (and message (set _LOG_MESSAGE message))
                        _LOG_MESSAGE)
                    " - " 
                    (string (integer _LOG_INDEX))))
    (set _LOG_INDEX (+ 1 _LOG_INDEX))
    )

  (defun create-train-reward-model-spec ()
    (setq first 1)
    
    (setq prior-feature F)
    (setq step-update-lambda
          (lambda (state-vector awareness-map)
              (setq interrupt-p F)
              (catch 
                  (progn
                      (log-index "create-train-reward-model-spec" (concat "training on input: " awareness-map) 0)
                      (if first
                          (progn
                              (group-reset memory-cache-group)
                              (group-reset reward-seeking-group)
                              (set first F)))
                      (log-index F (concat "state-vector: " (string state-vector)))
                      (setq mode (group-process-input memory-cache-group state-vector))
                      
                      (setq short-term-feature
                            (group-get-focus-feature memory-cache-group))
                      (if short-term-feature
                          (progn
                             (if (and prior-feature
                                      (not (equals prior-feature short-term-feature)))
                                 (progn
                                    (feature-force-complete prior-feature)
                                    (group-remove-feature memory-cache-group prior-feature)
                                    (group-import-feature reward-seeking-group prior-feature 1)))
                              (set prior-feature short-term-feature))
                          (log-index F "(not short-term-feature)"))
                      F)
                  (log-error "step-update-create-train-reward-model-spec"
                             (first e)))
              interrupt-p))

    (setq on-pattern-finished-lambda
          (lambda (was-interrupted-p)
              (if (setq short-term-feature
                        (group-get-focus-feature memory-cache-group))
                  (progn
                      (log-index F (concat "Finished moving " short-term-feature " to reward model "))
                      (feature-force-complete short-term-feature)
                      (group-remove-feature memory-cache-group short-term-feature)
                      (group-import-feature reward-seeking-group short-term-feature 1)))))

    (list (and (non-empty current-action-steps)
               robot 
               (equals _STANDARD_ROBOT_WORLD_TYPE current-world-type-name))
          "train reward model"
          "Uses the current action steps to train the reward model"
          (lambda (on-complete) (execute-current-pattern step-update-lambda on-pattern-finished-lambda 1))
          F))

  (defun create-run-name-spec ()
    (list (non-empty (get-desired-pattern-name))
          (concat "Execute: " (get-desired-pattern-name))
          "Executes the currently defined pattern name"
          (lambda (on-complete-lambda) 
              (execute-current-pattern-name  (if (and robot (or (is-checked check-learn) (is-checked check-observer)))
                                                 (get-standard-learning-step-uppdate-lambda (is-checked check-learn) 1 (is-checked check-fear-response)))
                                             on-complete-lambda))
          (if robot
              (vertical-layout :width _MATCH_PARENT
                               :height _WRAP_CONTENT
                               (text (concat current-world-name " training parameters"))
                               (setq check-observer
                                     (checkbox "Observation enabled"))
                               (setq check-learn
                                     (checkbox "Learning enabled"))
                               (setq check-fear-response
                                     (checkbox "Respond to fear"))))
          ))


  (defun create-run-pattern-spec ()
    (list (non-empty current-action-steps)
          "run pattern" 
          "Executes the action steps provided"
          (lambda (on-complete-lambda) 
              (execute-current-pattern  (if (and robot (or (is-checked check-learn) (is-checked check-observer)))
                                            (get-standard-learning-step-uppdate-lambda (is-checked check-learn) 1 (is-checked check-fear-response)))
                                        on-complete-lambda))
          (if robot
              (vertical-layout :width _MATCH_PARENT
                               :height _WRAP_CONTENT
                               (text (concat current-world-name " training parameters"))
                               (setq check-observer
                                     (checkbox "Observation enabled"))
                               (setq check-learn
                                     (checkbox "Learning enabled"))
                               (setq check-fear-response
                                     (checkbox "Respond to fear"))))))

  (defun get-back-and-forth-policy (learn-p)
    (setq state "INITIAL")
    (setq max-distance 80)
    (setq min-distance 34)
    (setq prev-focus F)

    (lambda (action-cause state-vector awareness-map)
        (setq distance (gethash awareness-map _ULTRA_SONIC_DATA_KEY))
        (log-ui "get-back-and-forth-policy" "State is: [" state "] Distance is: " (string distance))
        (if learn-p
            (progn
              (if (equals state "INITIAL")
                  (progn
                      (group-reset memory-cache-group)
                      (group-reset reward-seeking-group)))
              (setq mode (group-process-input memory-cache-group state-vector))

              (if (equals mode "EXTRAPOLATION")
                  (progn
                    (log-ui "get-back-and-forth-policy" "Trying to free memory")
                    (group-force-sleep memory-cache-group)
                    (group-set-mode memory-cache-group "LEARNING")
                    (setq mode (group-process-input memory-cache-group state-vector))))

              (setq short-term-feature
                    (group-get-focus-feature memory-cache-group))

              (log-ui "get-back-and-forth-policy" "Mode: " mode " Focus <" (string (integer (feature-get-group-allocation-index short-term-feature))) "> " (string short-term-feature))

              (log-ui "get-back-and-forth-policy" "Current focus: " (string short-term-feature) " previous focus: " (if prev-focus (string prev-focus) "null"))


              (if (and prev-focus
                       (not (equals short-term-feature prev-focus)))
                  (progn
                      (feature-force-complete prev-focus)
                      (group-remove-feature memory-cache-group prev-focus)
                      (group-import-feature reward-seeking-group prev-focus 1)
                      (log-ui "get-back-and-forth-policy" "Imported: " (string prev-focus))
                      ))
              (set prev-focus short-term-feature)))
          
        (setq next-action F)
        (signal-block (state)
            (("INITIAL") (if (< distance max-distance)
                             (progn
                                (setq state "AWAY")
                                (setq next-action _ACTION_KEY_BACKWARD))
                             (progn
                                (setq state "TOWARD")
                                (setq next-action _ACTION_KEY_FORWARD))))
            (("AWAY") (if (>= distance max-distance)
                          (progn
                              (setq state "TOWARD")
                              (setq next-action _ACTION_KEY_FORWARD))
                          (setq next-action _ACTION_KEY_BACKWARD)
                          ))
            (("TOWARD") (if (< distance min-distance)
                          (progn
                              (setq state "AWAY")
                              (setq next-action _ACTION_KEY_BACKWARD))
                          (setq next-action _ACTION_KEY_FORWARD))))
        (log-ui "get-back-and-forth-policy" "Selected action is: " next-action)
        next-action))

  (defun run-custom-robot (action-policy-lambda)

      (setq awareness-map (make-string-hashtable))
      (setq current-action _ACTION_KEY_STOP)
      (setq interrupted-p F)
      (setq collision-distance-threshold 10)
      (unless (or stop-process
                  (not current-action)
                  interrupted-p)

          (setq state-vector 
                (do-timed-action current-action
                                 F
                                 awareness-map))
          (log-ui "Run custom robot" "action: " current-action " awareness-map: " (string awareness-map))
          (if (not (setq interrupted-p
                         (or (= 1 (gethash awareness-map _LEFT_BUMPER_DATA_KEY))
                             (<= (gethash awareness-map _ULTRA_SONIC_DATA_KEY) collision-distance-threshold)
                             (= 1 (gethash awareness-map _RIGHT_BUMPER_DATA_KEY)))))
              (progn
                  ; compute next action
                  (setq current-action (funcall action-policy-lambda current-action state-vector awareness-map))


                )
              (log-ui "Run custom robot" "Protection interrupt")))
      interrupted-p
      (stop-moving)
      )

  (defun create-custom-pattern-spec ()

      (setq finished-p F)
      (setq stop-button
            (shadow-button "stop custom"
                           :text-size 14
                           :on-click (set finished-p 1)))

      (setq action-policy-lambda
            F)

      (setq check-train
            (checkbox "Train robot?"))

      (list robot
            "custom"
            "Moves the robot between two values"
            (lambda (on-complete)
                (set-text pattern-header (concat "Training world: " current-world-name))
                (add-view (remove-all-views run-header)
                          (horizontal-layout :width _MATCH_PARENT
                                             :height _WRAP_CONTENT
                                             (text "Training ...")
                                             stop-button))

                (setq action-policy-lambda
                      (get-back-and-forth-policy (is-checked check-train)))

                (evaluate-background "custom"
                                     (lambda (result error)
                                        (if error
                                            (log-error "Errors executing custom" error)
                                            (log-ui "Finished custom" "")))
                                     (setq interrupted-p F)
                                     
                                     (set stop-process F)
                                     (run-custom-robot action-policy-lambda)
                                     (stop-moving)
                                     interrupted-p))

            check-train
      )
  )

  (defun create-run-autonomously-spec ()

      (setq on-finished-lambda
            (lambda (result error)
                (remove-all-views run-header)))
      (setq custom-data-item-background-color "#E1EBF7")
      (setq custom-data-item-margin 2)
      (setq custom-control-view
            (scrollview :width _MATCH_PARENT
                        :height 150
                        (vertical-layout :width _MATCH_PARENT
                                         :height 150
                                         (horizontal-layout :width _MATCH_PARENT
                                                            :height _WRAP_CONTENT
                                                            :background-color custom-data-item-background-color
                                                            :margin-bottom custom-data-item-margin
                                                            (text "num steps" :width "50%")
                                                            (setq edit-steps
                                                                  (edit :hint "num steps" :width "50%"
                                                                        "200"
                                                                        :text-align "left")))
                                          (setq check-use-current-pattern-seed
                                                (checkbox "Use current pattern as seed" :checked "false"
                                                          :margin-bottom custom-data-item-margin
                                                          :background-color custom-data-item-background-color))
                                          (setq check-allow-learning
                                                (checkbox "Enable learning" :checked "true"
                                                          :margin-bottom custom-data-item-margin
                                                          :background-color custom-data-item-background-color))
                                          (setq check-allow-lstm-guidance
                                                (checkbox "Allow learned behaviors" :checked "true"
                                                          :margin-bottom custom-data-item-margin
                                                          :background-color custom-data-item-background-color)))))

      (multiple-bind (pattern-map 
                      action-step-lambda 
                      seed-pattern 
                      utility-provider-lambda 
                      default-behavior-pattern-lambda 
                      action-step-interrupt-lambda 
                      max-steps 
                      enable-learning-p 
                      can-act-upon-learning-p
                      )
                     (list _PATTERN_MAP
                           F
                           F
                           F
                           F
                           F
                           50
                           F
                           F))
      (list (and robot (equals _STANDARD_ROBOT_WORLD_TYPE current-world-type-name))
            "simple autonomous" 
            (concat "Runs autonomously using avoidance and reward seeking behaviors from " current-world-name)
            
            (lambda () 
                (setq stop-lambda
                    (run-autonomously pattern-map
                                      action-step-lambda
                                      (if (is-checked check-use-current-pattern-seed)
                                          (if current-action-steps
                                              (get-pattern-iterator current-action-steps)
                                              (progn (show-short-toast "No steps defined") F))
                                          seed-pattern)
                                      utility-provider-lambda
                                      default-behavior-pattern-lambda
                                      action-step-interrupt-lambda
                                      (or (and (setq v (non-empty (get-text edit-steps))) (integer v)) max-steps)
                                      (is-checked check-allow-learning)
                                      (is-checked check-allow-lstm-guidance)
                                      on-finished-lambda))
                (set-text pattern-header (concat "Autonomous world: " current-world-name))
                (add-view (remove-all-views run-header)
                          (horizontal-layout :width _MATCH_PARENT
                                             :height _WRAP_CONTENT
                                             (text "Running autonomously...")
                                             (shadow-button "Stop"
                                                            :text-size 14
                                                            :on-click (funcall stop-lambda)))))
            custom-control-view))


  (defun create-predict-pattern-result-spec ()
    (list (and (non-empty current-action-steps) robot _PRIOR_STATE)
          "predict action" 
          "Tries to predict what will happen if robot executes motor pattern"
          (lambda (on-complete-lambda)
              (setq action-iterator
                    (get-pattern-iterator current-action-steps)) 
              (setq action-value-spec
                    (extrap-motor-pattern-value _PRIOR_STATE
                                                action-iterator
                                                ))

              (multiple-bind (PATTERN-COUNT NEUTRAL-COUNT REWARD-COUNT HARM-COUNT REWARD-ANTIC HARM-ANTIC)
                             action-value-spec)
              (multiple-bind (harm-prob harm-degree reward-prob reward-degree)
                             (list (/ HARM-COUNT
                                      PATTERN-COUNT)
                                   HARM-ANTIC
                                   (/ REWARD-COUNT
                                      PATTERN-COUNT)
                                   REWARD-ANTIC))

              (setq value-desc
                    (describe-action-value-spec action-value-spec))
              (log-ui "predicting action" "Expected values: " (string action-value-spec) " description: " value-desc)
              (setq verbal-desc
                    (make-string-hashtable (list (list _ACTION_VALUE_DESC_HARM "There is a high chance of harm")
                                                 (list _ACTION_VALUE_DESC_REWARD "I expect reward along this path")
                                                 (list _ACTION_VALUE_DESC_NEUTRAL "This path is okay"))))

              (log-ui "predicting action" "Action: " (string current-action-steps) " Predicted result from " (string (get-robot-logical-state _PRIOR_STATE _STANDARD_ROBOT_CONFIG)) ": [" value-desc "]")
              (speak (gethash verbal-desc value-desc))
              (if (and (is-checked check-run-if-okay)
                       (not (equals value-desc _ACTION_VALUE_DESC_HARM)))
                  (execute-current-pattern  (if (or (is-checked check-learn) (is-checked check-observer))
                                                (get-standard-learning-step-uppdate-lambda (is-checked check-learn) 1 (is-checked check-fear-response))))))
          (vertical-layout :width _MATCH_PARENT
                           :height _WRAP_CONTENT
                           (setq check-run-if-okay
                                 (checkbox "Run pattern if safe"))
                           (setq check-observer
                                 (checkbox "Observation enabled"))
                           (setq check-learn
                                 (checkbox "Learning enabled"))
                           (setq check-fear-response
                                 (checkbox "Respond to fear"))
                           )))


(setq _EXPLORATORY_PATTERNS ())

(setq _HARM_RECOVERY_PATTERNS ())

(defun set-harm-recovery-actions (actions)
  (set _HARM_RECOVERY_PATTERNS 
       (set-current-meta-data _META_DATA_RECOVERY_PATTERNS_KEY actions))

  (log-ui "settings" "Set harm recovery actions to: " (string actions))
  
  (show-toast-off-thread "updated harm recovery actions"))

(defun get-harm-recovery-actions ()
  (set _HARM_RECOVERY_PATTERNS
       (or (get-current-meta-data _META_DATA_RECOVERY_PATTERNS_KEY) () )))

(defun set-exploration-actions (actions)
  (set _EXPLORATORY_PATTERNS
       (set-current-meta-data _META_DATA_EXPLORATORY_PATTERNS_KEY actions))
  (log-ui "settings" "Set exporatory actions to: " (string actions))

  (show-toast-off-thread "updated exporatory actions")
  )

(defun get-exploration-actions ()
    (set _EXPLORATORY_PATTERNS (or (get-current-meta-data _META_DATA_EXPLORATORY_PATTERNS_KEY) ())))

(defun create-simple-planning-result-spec ()
    (setq tag "planning bot")
    (setq finished-p F)
    (setq stop-button 
          (shadow-button "stop planning"
                         :text-size 14
                         :on-click (set finished-p 1)))

    (setq num-planning-patterns 4)
    (setq must-include-n-distinct-p 1)

    (list (and robot _PRIOR_STATE)
          "planned behavior" 
          "Given current position, executes motor patterns repeatedly using planning to avoid harm and seek reward"
          (lambda (on-complete-lambda)

                (defun get-next-test-patterns (was-interrupted-p)
                    (if (and was-interrupted-p
                             (non-empty-list (get-harm-recovery-actions)))
                        (find pattern-name
                              (random-perm (get-harm-recovery-actions))
                              (not (gethash _BASE_ACTION_MAP pattern-name)))
                        (find pattern-name
                              (or (progn
                                      (setq map (make-string-hashtable (mapcar x (get-exploration-actions) (list x 1))))
                                      (non-empty-list (remove-string-duplicates (sample-n-strings (find x (get-current-pattern-preferences) (gethash map x)) 
                                                                                                  num-planning-patterns
                                                                                                  must-include-n-distinct-p))))
                                 (remove-string-duplicates (sample-n-strings (get-current-pattern-preferences) 
                                                                             num-planning-patterns
                                                                             must-include-n-distinct-p))
                                 ())
                              (not (gethash _BASE_ACTION_MAP pattern-name)))))

                (defun get-default-motor-pattern ()
                    (random-select (get-next-test-patterns)))

                (set-text pattern-header (concat "Planning behavior world: " current-world-name))
                (add-view (remove-all-views run-header)
                          (horizontal-layout :width _MATCH_PARENT
                                             :height _WRAP_CONTENT
                                             (text "Planning ...")
                                             stop-button))
                (evaluate-background "planning"
                                     (lambda (result error)
                                        (if error
                                            (log-error "Errors executing planning" error)
                                            (log-ui tag "finished")))
                                     
                                     (set stop-process F)
                                     (setq awareness-map (make-string-hashtable))
                                     (setq current-action _ACTION_KEY_STOP)
                                     
                                     (setq collision-distance-threshold 10)
                                     (setq acceptable-patterns ())
                                     (setq selected-pattern F)
                                     (setq patterns-to-test F)
                                     (setq interrupted-p F)
                                     (setq search-depth 6)
                                     (setq max-paths 15)
                                     (setq harm-threshold .45)

                                     (defun interruption-due-to-pain-p (interrupt)
                                        (and (list-p interrupt)
                                             (first interrupt)))


                                      (unless (or stop-process
                                                  finished-p)

                                          (if (not _PRIOR_STATE)
                                              (do-timed-action _ACTION_KEY_STOP
                                                               F
                                                               awareness-map))

                                          (setq patterns-to-test (get-next-test-patterns interrupted-p))
                                          (setq interrupted-p F)
                                          (log-ui tag "Planning what to do next.  Testing: " (string patterns-to-test))
                                          (setq acceptable-patterns ())
                                          (setq best-bad F)
                                          (setq best-bad-score F)
                                          (setq selected-pattern F)
                                          (run-foreground (speak "Planning next action"))
                                          (for pattern-name
                                               patterns-to-test
                                               F
                                               (if (equals _ACTION_VALUE_DESC_REWARD
                                                           (setq value-desc
                                                                 (describe-action-value-spec  (setq value-spec (extrap-motor-pattern-value _PRIOR_STATE
                                                                                                                                          (get-pattern-iterator (list pattern-name))
                                                                                                                                          search-depth
                                                                                                                                          max-paths
                                                                                                                                          
                                                                                                                                          )))))
                                                   (break (set selected-pattern pattern-name))
                                                   (if (equals value-desc _ACTION_VALUE_DESC_NEUTRAL)
                                                       (set acceptable-patterns
                                                            (append-item acceptable-patterns pattern-name))
                                                       (progn
                                                          (multiple-bind (pattern-count neutral-count reward-count harm-count reward-antic harm-antic)
                                                                         value-spec)

                                                          (if (< (/ harm-count pattern-count) harm-threshold)
                                                              (progn
                                                                (log-ui tag pattern-name ": Harm possible but expectation below threshold @" (/ harm-count pattern-count) " overall value: " value-spec)
                                                                (set acceptable-patterns
                                                                     (append-item acceptable-patterns pattern-name)))
                                                              (progn
                                                                (log-ui tag "Rejecting option: " pattern-name " = " value-spec)
                                                                (if (or (not best-bad-score)
                                                                        (< harm-antic best-bad-score))
                                                                    (progn
                                                                        (log-ui tag pattern-name " is new least bad option")
                                                                        (set best-bad-score harm-antic)
                                                                        (set best-bad pattern-name)))))))))
                                          
                                          (if (not selected-pattern)
                                              (if (non-empty-list acceptable-patterns)
                                                  (progn
                                                    (run-foreground (speak "Finished planning.  Found a safe path"))
                                                    (setq selected-pattern
                                                          (first acceptable-patterns)))
                                                  
                                                  (progn
                                                      (run-foreground (speak "No safe routes.  Doing my best using safest expectd route"))
                                                      (log-ui tag "Relying on least bad option: " best-bad)
                                                      (if best-bad
                                                          (setq selected-pattern best-bad)
                                                          (setq selected-pattern
                                                                (get-default-motor-pattern)))))
                                              (run-foreground (speak "Finished planning.  Found a good route")))

                                          (log-ui tag "Selected pattern to run: " selected-pattern)
                                          (setq last-processed-state-data
                                                F)

                                          (setq interrupted-p
                                                (run-pattern-recursive (get-pattern-iterator (list selected-pattern))
                                                                       (get-standard-learning-step-uppdate-lambda (is-checked check-learn) 
                                                                                                                  1 
                                                                                                                  (is-checked check-fear-response) 
                                                                                                                  DECAY-ALL-FEATURES-P 
                                                                                                                  selected-pattern
                                                                                                                  (lambda (last-step-state)
                                                                                                                      (set last-processed-state-data last-step-state)))
                                                                       1))
                                          (if interrupted-p 
                                              (progn
                                                  (log-ui tag selected-pattern " was interrupted " (if (interruption-due-to-pain-p interrupted-p) " due to pain" "due to fear"))

                                                  (if (and last-processed-state-data (interruption-due-to-pain-p interrupted-p))
                                                       (progn
                                                          (log-ui tag "Depressing motor pattern: " selected-pattern)
                                                          (set-current-pattern-preferences (decrement-key-from-list (get-current-pattern-preferences)
                                                                                                                    selected-pattern
                                                                                                                    1))
                                                          ; enhance fear models that predicted pain
                                                          (for harm-feature-id
                                                               (get-hash-keys (gethash last-processed-state-data STEP_KEY_GET_HARM_ANTICIPATORS))
                                                               F
                                                               (progn
                                                                  (setq model (gethash (gethash last-processed-state-data STEP_KEY_GET_HARM_ANTICIPATORS) harm-feature-id))
                                                                  (log-ui tag "reseting failure count of fear model: " model " due to correctly predicting harm")
                                                                  (reset-feature-failure-count model)
                                                                  (setq base-utility 1)
                                                                  (setq utility-increase-fraction FEAR-ACCURACY-VALUE-BONUS)
                                                                  (set-feature-base-utility model
                                                                                            (+ (get-feature-utility model)
                                                                                               (* utility-increase-fraction
                                                                                                  (- base-utility (get-feature-utility model)))))
                                                                  (log-ui tag "Increasing anticipatory strengh of correctly predicting fear model: " model " by " FEAR-ACCURACY-VALUE-BONUS)
                                                                  (group-increase-feature-value-fraction fear-level-group model FEAR-ACCURACY-VALUE-BONUS)))
                                                              
                                                          (for reward-feature-id
                                                               (get-hash-keys (gethash last-processed-state-data STEP_KEY_GET_REWARD_ANTICIPATORS))
                                                               F
                                                               (progn
                                                                  (setq model (gethash (gethash last-processed-state-data STEP_KEY_GET_REWARD_ANTICIPATORS) reward-feature-id))
                                                                  (log-ui tag "Reducing anticipatory strengh of mispredicting reward model: " model " by " MISPREDICTING-REWARD-MODEL-VALUE-REDUNCTION-FRACTION)
                                                                  (group-decrease-feature-value-fraction reward-seeking-group model MISPREDICTING-REWARD-MODEL-VALUE-REDUNCTION-FRACTION)
                                                                  (log-ui tag "Increasing failure count of model:" model " which predicted reward")
                                                                  (increment-feature-failure-count model)
                                                                  (if (decay-feature-model-utility model MINIMUM-UTILITY)
                                                                      (progn
                                                                          (log-ui tag "Removed reward seeking feature " model " due to obselescence")
                                                                          (group-remove-feature reward-seeking-group model))))))))
                                              (progn
                                                   (log-ui tag selected-pattern " executed successfully.  Last processed state data: " last-processed-state-data)
                                                   (if last-processed-state-data
                                                       (progn
                                                          
                                                          (if (and (setq reward-utility (gethash last-processed-state-data STEP_KEY_LAST_FEATURE_WAS_REWARDED))
                                                                   (setq prev-focus 
                                                                         (gethash last-processed-state-data STEP_KEY_LAST_FEATURE_PROCESSED)))
                                                              (progn
                                                                  
                                                                  (setq copied (feature-copy prev-focus))
                                                                  (set-feature-base-utility copied
                                                                                            reward-utility)
                                                                  (log-ui "Copied reward feature " prev-focus " to reward group forming: " copied " with utility: " (get-feature-utility copied) )
                                                                  (group-import-feature reward-seeking-group copied 1)))


                                                          (for harm-feature-id
                                                               (get-hash-keys (gethash last-processed-state-data STEP_KEY_GET_HARM_ANTICIPATORS))
                                                               F
                                                               (progn
                                                                  (setq model (gethash (gethash last-processed-state-data STEP_KEY_GET_HARM_ANTICIPATORS) harm-feature-id))
                                                                  (log-ui tag "Incrementing fear model failure count of " model " due to misprediction.")
                                                                  (if (gethash last-processed-state-data STEP_KEY_GET_WAS_REWARDS)
                                                                      (progn
                                                                        (log-ui tag "Incrementing fear model failure count of " model " again due to misprediction and positive reinforcement")
                                                                        (increment-feature-failure-count model)))

                                                                  (log-ui tag "Reducing anticipatory strengh of mispredicting avoidance model: " model " by " UNNECESSARY-FEAR-VALUE-REDUNCTION-FRACTION)
                                                                  (group-decrease-feature-value-fraction fear-level-group model UNNECESSARY-FEAR-VALUE-REDUNCTION-FRACTION)
                                                                  (if (decay-feature-model-utility model MINIMUM-UTILITY)
                                                                      (progn
                                                                          (log-ui tag "Removed harm avoiding feature " model " due to obselescence")
                                                                          (group-remove-feature fear-level-group model)))))

                                                          (for reward-feature-id
                                                               (get-hash-keys (gethash last-processed-state-data STEP_KEY_GET_REWARD_ANTICIPATORS))
                                                               F
                                                               (progn
                                                                  (setq model (gethash (gethash last-processed-state-data STEP_KEY_GET_REWARD_ANTICIPATORS) reward-feature-id))
                                                                  (reset-feature-failure-count model)
                                                                  (setq base-utility 1)
                                                                  (setq utility-increase-fraction REWARD-ACCURACY-VALUE-BONUS)
                                                                  (set-feature-base-utility model
                                                                                            (+ (get-feature-utility model)
                                                                                               (* utility-increase-fraction
                                                                                                  (- base-utility (get-feature-utility model)))))
                                                                  (log-ui tag "Increasing anticipatory strengh of correctly predicting reward model: " model " by " REWARD-ACCURACY-VALUE-BONUS)
                                                                  (group-increase-feature-value-fraction reward-seeking-group model REWARD-ACCURACY-VALUE-BONUS)))))
                                                )))
                                      
                                      (stop-moving)
                                      (show-toast-off-thread "Finished planning behavior")
                                      ))
          (vertical-layout :width _MATCH_PARENT
                           :height _WRAP_CONTENT
                           (setq check-learn
                                 (checkbox "Learning enabled"))
                           (setq check-fear-response
                                 (checkbox "Respond to fear")))
          ))


  (defun show-run-dialog ()
      (setq run-list
            (find spec 
                  (list (list 1 "none" "" F F) (create-custom-pattern-spec) (create-run-name-spec) (create-run-pattern-spec) (create-run-autonomously-spec) (create-train-reward-model-spec) (create-predict-pattern-result-spec) (create-simple-planning-result-spec))
                  (first spec)))

      (setq on-run-lambda F)

      (defun get-run-spinner-item (index)
          (multiple-bind (run-name run-description run-lambda custom-control-view)
                         (rest (nth run-list index)))
          (list (text run-name)
                (text run-name)
                (lambda ()
                    (set on-run-lambda run-lambda)
                    (set-text text-desc run-description)
                    (for spec
                         run-list
                         F
                         (if (last spec)
                             (if (equals custom-control-view (last spec))
                                 (show-view (last spec))
                                 (hide-view (last spec) 1)))))))

      (dialog :dialog-title "Run options"
              (vertical-layout :width _MATCH_PARENT
                               :height _WRAP_CONTENT
                               :padding 10
                               (text (or current-world-name "no robot"))
                               (spinner :width _WRAP_CONTENT
                                        :height _WRAP_CONTENT
                                        :spinnerMode "dropdown"
                                        (mapcar (run-spec i)
                                                run-list
                                                (get-run-spinner-item i)))
                               (setq custom-controls
                                     (vertical-layout :width _MATCH_PARENT
                                                      :height _WRAP_CONTENT
                                                      (mapcar spec
                                                              (find x run-list (last x))
                                                              (hide-view (last spec) 1)
                                                      )))
                               (setq text-desc
                                     (text :width _MATCH_PARENT
                                           :height _WRAP_CONTENT)))
              :on-positive (if on-run-lambda
                              (funcall on-run-lambda)
                              (show-short-toast "Nothing to do"))
             :positive-text "OK"
             :cancel-text "Cancel"))

    

  (setq action-pattern-display
        (vertical-layout :width _MATCH_PARENT
                         :height _WRAP_CONTENT
                         (setq pattern-header
                               (text))
                         run-header
                         (relative :width _MATCH_PARENT
                                            :height _WRAP_CONTENT
                                            (setq recording-button
                                                  (shadow-button (if is-currently-recording-p
                                                                     "STOP"
                                                                     "START")
                                                                 :text-size 14
                                                                 :parent-align "left"
                                                                 :width _WRAP_CONTENT
                                                                 :height _WRAP_CONTENT
                                                                 :on-click (cb-set-recording-state (not is-currently-recording-p))))
                                            (setq recording-state
                                                  (text (if is-currently-recording-p
                                                            "RECORDING"
                                                            "NOT RECORDING")
                                                        :parent-align "center"))
                                            (setq run-button
                                                   (shadow-button "run"
                                                                  :text-size 14
                                                                  :parent-align "right"
                                                                  :on-click (show-run-dialog))))
                          (horizontal-layout :width _MATCH_PARENT
                                             :height _WRAP_CONTENT
                                             (vertical-layout :width "100%"
                                                              :height _WRAP_CONTENT
                                                              (text "Patterns"
                                                                    :text-size 10)

                                                              (setq pattern-list-view
                                                                   (listview :width _MATCH_PARENT
                                                                             :height 100
                                                                             :on-item-long-click (append-action-to-sequence-list (nth (get-pattern-list-names) selected-index))
                                                                             :on-item-clicked (select-action (nth (get-pattern-list-names) selected-index))
                                                                             (get-pattern-list-items)))
                                                              (horizontal-layout :width _MATCH_PARENT
                                                                                 :height _WRAP_CONTENT
                                                                                 (setq pattern-name-edit
                                                                                       (edit :width "100%"
                                                                                             :height _WRAP_CONTENT
                                                                                             :hint "pattern name"
                                                                                             ""))
                                                                                 (image :width _DEFAULT_SMALL_ICON_SIZE_DP
                                                                                         :height _DEFAULT_SMALL_ICON_SIZE_DP
                                                                                         :background (get-drawable-resource-id "ic_close_black_24dp")
                                                                                         :on-click (clear-pattern-name-text)))

                                                             (setq action-step-list
                                                                   (listview :width _MATCH_PARENT
                                                                             :height 125
                                                                             :on-item-long-click (toggle-action-step-selection-state selected-index)
                                                                             (set action-step-list-item-views
                                                                                  (mapcar (action-name i)
                                                                                         (or (cb-get-pattern-steps-by-name)
                                                                                             (list ))
                                                                                         (create-action-step-list-item action-name
                                                                                                                       i))))))
                                              (scrollview :width _WRAP_CONTENT
                                                          :height _MATCH_PARENT
                                                          (vertical-layout :width _WRAP_CONTENT
                                                                         :height _WRAP_CONTENT
                                                                         (setq save-button
                                                                               (shadow-button "save"
                                                                                              :text-size 14
                                                                                             :on-click (if (empty-string (get-desired-pattern-name))
                                                                                                           (show-short-toast "Must name pattern")
                                                                                                           (cb-save-current-pattern))))
                                                                         
                                                                         (setq clear-button
                                                                               (shadow-button "clear"
                                                                                              :text-size 14
                                                                                              :on-click (clear-current-action-steps)))
                                                                         (setq delete-button
                                                                               (shadow-button "delete"
                                                                                              :text-size 14
                                                                                              :on-click (delete-current-action-pattern)))
                                                                         (setq set-exploration-actions-button
                                                                               (shadow-button "search"
                                                                                              :text-size 14
                                                                                              :on-click (set-exploration-actions current-action-steps)))
                                                                         (setq set-harm-actions-button
                                                                               (shadow-button "recovery"
                                                                                              :text-size 14
                                                                                              :on-click (set-harm-recovery-actions current-action-steps)))
                                                                         )))
                          
                           ))

  (set view-state-list
       (or (load-graph _LAST_GRAPH_NAME)
           (list)))

  (setq interrupt-iterator
        F)

  (setq bar-width 10)

  (setq bar-max-height 30)
  
  (setq margin 3)

  (setq running-p F)

  (setq action-step-milli 350)

  (setq boundary-color "#800000")

  (setq last-action F)

  (setq boundary-width 2)

  (setq current-graph-name "LAST-GRAPH")

  ; greenish
  (setq left-color 
        "#17D422")

  (setq right-color
        "#AB17D4")

  (setq forward-color
        "#000000")

  (setq stop-color
        "#000000")

  (setq color-spinner-width _WRAP_CONTENT)
  (setq color-spinner-height 5)

  (setq state-color-overide-colors
        (list F "#17D422" "#AB17D4" "#000000" "#C5F50A" "#D0021B" "#3902D0" "#F8E71C"))

  (setq state-color-overide F)

  (setq action-color-map
        (make-string-hashtable (list (list _ACTION_KEY_LEFT left-color)
                                     (list _ACTION_KEY_RIGHT right-color)
                                     (list _ACTION_KEY_FORWARD forward-color)
                                     (list _ACTION_KEY_BACKWARD stop-color)
                                     (list _ACTION_KEY_STOP stop-color))))

  (setq left-collision-view
        (text :text-align "center"
              :width _WRAP_CONTENT
              :height _WRAP_CONTENT
              ""))

  (setq right-collision-view
        (text :text-align "center"
              :width _WRAP_CONTENT
              :height _WRAP_CONTENT
              ""))

  (setq sonic-view
        (text :text-align "center"
              :width _WRAP_CONTENT
              :height _WRAP_CONTENT
              ""))

  (setq interval-edit
        (edit (or (and action-step-milli
                       (string (integer action-step-milli)))
                  "")
              :width 100
              :height _WRAP_CONTENT
              :hint "step milli"))

  (setq prediction-view
        (text :width _MATCH_PARENT
              :height _WRAP_CONTENT
              "none"))

  (setq utility-bar
        (seek-bar :width _MATCH_PARENT
                  :height _WRAP_CONTENT
                  :on-value-changed (progn (set current-utility value) (if on-utility-changed (funcall on-utility-changed value)))
                  0
                  11
                  5))

  (setq awareness-map
      (make-string-hashtable))

  (setq started-p F)

  (setq stop-process F)

  (setq current-utility 0)

  (setq append-state-p F)

  (setq interrupt-action-p F)

  (setq interrupt-enabled-p F)

  (setq collision-distance 30)

  (setq segmentation-list ())

  (setq last-feature-map F)

  (setq last-boundary-map F)

  (setq reset-user-utility-on-interrupt-p F)

  (setq run-autonomously-p F)

  (setq selected-action-pattern F)

  (defun reset-user-interrupt ()
      (evaluate-foreground F
                           (set-seek-value utility-bar 5)))

  ; Utility is on a 0 to 10 scale
  (setq on-utility-changed
        (lambda (value)
            F
            ))

  (defun create-color-spinner-item (color)
    (list (if color
              (text color
                    :background-color color
                    :text-color stop-color)
              (text "None"))
          (if color
              (text color
                    :background-color color
                    :text-color stop-color)
              (text "None"))
          (lambda ()
              (set state-color-overide color))))


  (defun is-boundary-state (state expected-state)
      (= state (or expected-state 10)))

  (defun has-boundary-value (value current-action expected-state)
      (multiple-bind (state color action)
                     value)

      (and (is-boundary-state state expected-state)
           (equals current-action action)))

  (defun reset-view-state ()
    (set view-state-list ()))

  (defun update-view-list-boundary (boundary-color current-state)
      (multiple-bind (current-action state)
                     current-state)
      (setq last-n-spec
            (right-list view-state-list
                        boundary-width
                        1))

      (multiple-bind (first-index last-index)
                     (list (second (first last-n-spec))
                           (second (last last-n-spec))))

      (setq transition-p
            (and last-feature-map
                 (gethash last-feature-map
                          (dec last-index))))

      (if (and (= (length last-n-spec) boundary-width)
               current-action 
               (all view-state-spec
                    last-n-spec
                    (has-boundary-value (first view-state-spec) current-action)))
          (progn
            (for view-state-spec
                 last-n-spec
                 (if transition-p
                     (on-new-feature))
                 (progn
                    (multiple-bind (view-state index) view-state-spec)
                    (multiple-bind (state color action)
                                   view-state)
                    (if (not last-boundary-map)
                        (set last-boundary-map
                             (make-int-hashtable)))

                    (if last-feature-map
                        (remhash last-feature-map index))

                    (defhash last-boundary-map 
                             index
                             state)

                    (set-nth view-state-list
                             index
                             (list state boundary-color action)))))
          (progn
            (if (not last-feature-map)
                (set last-feature-map
                     (make-int-hashtable)))

            (if (and last-boundary-map
                     (gethash last-boundary-map
                              (dec last-index)))
                (on-new-boundary))

            (defhash last-feature-map
                     last-index
                     state))))

  (defun append-view-state (distance action)
      (setq next-value
            (list (setq state
                        (simple-discretize distance
                                           10
                                           0
                                           255))
                  (or state-color-overide (gethash action-color-map action))
                  action))

      (set view-state-list
           (append-item view-state-list
                        next-value))

      (set view-state-segment-list
           (append-item view-state-segment-list
                        next-value))

      (update-view-list-boundary boundary-color (list action state)))


  (defun on-new-feature ()
    (set segmentation-list
         (append-item segmentation-list
                      last-feature-map))
    (set last-feature-map
         (make-int-hashtable)))

  (defun on-new-boundary ()
    (set segmentation-list
         (append-item segmentation-list
                      last-boundary-map))
    (set last-boundary-map
         (make-int-hashtable)))

  (defun is-collision-state ()
      (log-info "collision distance: " 
                (concat (string collision-distance) 
                        "\n distance: " 
                        (string (gethash awareness-map _ULTRA_SONIC_DATA_KEY))
                        " left: " (string (gethash awareness-map _LEFT_BUMPER_DATA_KEY))
                        " right: " (string (gethash awareness-map _RIGHT_BUMPER_DATA_KEY))))
      (or (< (gethash awareness-map _ULTRA_SONIC_DATA_KEY)
             collision-distance)
          (= 1 (gethash awareness-map _LEFT_BUMPER_DATA_KEY))
          (= 1 (gethash awareness-map _RIGHT_BUMPER_DATA_KEY))))
  
  (setq sensor-graph
        (horizontal-layout :width _MATCH_PARENT
                           :height _MATCH_PARENT
                           :child-align "bottom"))

  (defun get-collision-display-value (value)
      (if (and value (= value 1))
          "bump"
          ""))

  (defun update-sensor-display ()
    (if test-robot-p
        (return))
    (evaluate-foreground (lambda (result error) 
                                (if error 
                                    (log-error "Error setting sensory state" error)))
        (set-text left-collision-view
              (get-collision-display-value (gethash awareness-map _LEFT_BUMPER_DATA_KEY)))

        (set-text right-collision-view
                  (get-collision-display-value (gethash awareness-map _RIGHT_BUMPER_DATA_KEY)))

        (set-text sonic-view
                  (string (gethash awareness-map _ULTRA_SONIC_DATA_KEY)))))

  (defun stop-processing ()
    (persist-motor-pattern)
    (if test-robot-p
        (return))
    (action-stop)
    (set running-p F)
    (show-short-toast "User interrupt")
    (set stop-process 1))

  (defun full-exit ()
    (set running-p F)
    (persist-motor-pattern)
    (if started-p
        (progn
            (stop-processing)
            (set nxt F)
            (if (not test-robot-p)
                (quit)))))

  

  (defmacro run-foreground (expr) 
      `(evaluate-foreground (lambda (r e) (if e (log-error "run-foreground errors" e)))
                           ,expr))
    
  (defun create-configuration-panel ()
    (vertical-layout :width _MATCH_PARENT
                     :height _WRAP_CONTENT
                     (horizontal-layout :width _MATCH_PARENT
                                        :height _WRAP_CONTENT
                                        (text "Step interval")
                                        interval-edit
                                        (button "Update interval"
                                                :on-click (if (non-empty (setq step (get-text interval-edit)))
                                                              (set action-step-milli
                                                                   (integer step))
                                                              (set action-step-milli
                                                                   F))))
                     (horizontal-layout :width _MATCH_PARENT
                                        :height _WRAP_CONTENT
                                        (button "Update sensors"
                                                :on-click (update-sensor-display))
                                        (checkbox :width "100%"
                                                  :height _WRAP_CONTENT
                                                  :checked "false"
                                                  :on-check-changed (set run-autonomously-p is-checked-p)
                                                  "autonomous"))))

  (defun create-status-panel ()
    (vertical-layout :width _MATCH_PARENT
                     :height _WRAP_CONTENT
                     prediction-view
                     (horizontal-layout :width _MATCH_PARENT
                                       :height _WRAP_CONTENT
                                       (vertical-layout :width "33%"
                                                        :height _WRAP_CONTENT
                                                        (text :width _MATCH_PARENT
                                                              :height _WRAP_CONTENT
                                                              :text-style "bold"
                                                              :text-align "center"
                                                              "L Collision")
                                                        left-collision-view)
                                       (vertical-layout :width "33%"
                                                        :height _WRAP_CONTENT
                                                        (text :width _MATCH_PARENT
                                                              :height _WRAP_CONTENT
                                                              :text-style "bold"
                                                              :text-align "center"
                                                              "Sonar")
                                                        sonic-view)
                                       (vertical-layout :width "33%"
                                                        :height _WRAP_CONTENT
                                                        (text :width _MATCH_PARENT
                                                              :height _WRAP_CONTENT
                                                              :text-style "bold"
                                                              :text-align "center"
                                                              "R Collision")
                                                        right-collision-view))))


  (defun update-state-view-panel (data-list max-data min-data)
      (setq raw-data-points (mapcar x data-list (first x)))
      
      (setq data-width 
            (- max-data min-data))

      (log-info "Updating data"
                (concat "data width: " data-width
                        "\n points: " 
                        (string raw-data-points)
                        "\n heights: "
                        (string (mapcar value
                                        raw-data-points
                                        (* bar-max-height
                                           (/ (- value min-data)
                                              data-width))))))
      
      (evaluate-foreground (lambda (result error) 
                                (if error 
                                    (log-error "update-state-view-panel" error)))
                           (remove-all-views sensor-graph)
                           (for (value i)
                                data-list
                                F
                                (add-view sensor-graph
                                         (solid :background-color (second value)
                                                :height (* bar-max-height
                                                           (/ (- (first value) min-data)
                                                              data-width))
                                                :width bar-width
                                                :margin-left (if (> i 0) margin 0))))))
  
  (defun reload-graph (view-data-list)
      (multiple-bind (max-data min-data)
                     (list 10 0))
      (setq data-width 
            (- max-data min-data))
      (remove-all-views sensor-graph)
      (for (value i)
            view-data-list
            F
            (add-view sensor-graph
                     (solid :background-color (second value)
                            :height (* bar-max-height
                                       (/ (- (first value) min-data)
                                          data-width))
                            :width bar-width
                            :margin-left (if (> i 0) margin 0)))))

  (defun save-current-graph (all-p)
      (setq name
            (or (non-empty (get-text current-name-text))
                _LAST_GRAPH_NAME))

      (if all-p
          (persist-graph name view-state-list)
          (persist-graph name view-state-segment-list))
      (show-short-toast (concat "Saved: " name)))

  (defun remove-graph-name (name)
    (remove-graph name))

  (defun show-pattern-select-dialog ()

    (defun get-list-view (graph-name)
        (setq border-color "#410401")
        (setq selected-color "#B7FFE9")
        (text graph-name 
              :width _MATCH_PARENT
              :background (create-background :border-bottom-width 2 :foreground-color selected-color :border-color border-color)))

    (defun set-graph (name append-p)
      (set view-state-list
           (if append-p
               (append view-state-list
                       (load-graph name))
               (load-graph name)))

      (set-text current-name-text
                name)
      (reload-graph view-state-list))

    (setq item-names
          (load-graph-names))
    (setq this-dialog
          (dialog :dialog-title "Select Pattern"
                  :on-positive (dismiss-dialog this-dialog)
                  :positive-text "Ok"
                  (vertical-layout :width _MATCH_PARENT
                                   :height 200
                                   (horizontal-layout :width _MATCH_PARENT
                                                      :height _WRAP_CONTENT
                                                      (text "Select pattern:"
                                                            :width "100%")
                                                      (setq append-check
                                                            (checkbox "Append"
                                                                      :checked "true")))
                                   
                                   (listview :width _MATCH_PARENT
                                             :height _MATCH_PARENT
                                             :on-item-clicked (progn (set-graph (nth item-names selected-index) (is-checked append-check))
                                                                     (dismiss-dialog this-dialog))
                                             :on-item-long-click (progn (remove-graph-name (nth item-names selected-index))
                                                                        (dismiss-dialog this-dialog))
                                             (mapcar pattern-name
                                                     item-names
                                                     (get-list-view pattern-name)))))))

  (setq current-name-text
        (edit _LAST_GRAPH_NAME
              :width "100%"
              :height _WRAP_CONTENT))

  (defmacro create-state-view-panel ()
      (vertical-layout :width _MATCH_PARENT
                       :height _WRAP_CONTENT
                       :padding-left 10
                       :padding-right 10
                       (horizontal-layout :width _MATCH_PARENT  
                                          :height _WRAP_CONTENT
                                          (checkbox "Append data"
                                                    :checked "false"
                                                    :on-check-changed (set append-state-p is-checked-p))
                                          (spinner :width _MATCH_PARENT
                                                   :height _WRAP_CONTENT
                                                   :spinnerMode "dropdown"
                                                   (mapcar color
                                                           state-color-overide-colors
                                                           (create-color-spinner-item color))))
                       (horizontal-layout :width _MATCH_PARENT
                                          :height _WRAP_CONTENT
                                         (checkbox "Enable interrupts"
                                                   :width "50%"
                                                   :height _WRAP_CONTENT
                                                   :checked (display-boolean interrupt-enabled-p)
                                                   :on-check-changed (progn
                                                                        (if is-checked-p
                                                                            (show-short-toast "interrupt-enabled")
                                                                            (show-short-toast "interrupt-disabled"))
                                                                        (set interrupt-enabled-p is-checked-p)))
                                         (checkbox "Reset on Interrupt"
                                                   :width "50%"
                                                   :height _WRAP_CONTENT
                                                   :checked (display-boolean reset-user-utility-on-interrupt-p)
                                                   :on-check-changed (set reset-user-utility-on-interrupt-p is-checked-p)))

                       (horizontal-layout :width _MATCH_PARENT
                                          :height _WRAP_CONTENT
                                          (text "Utility")
                                          utility-bar)
                       (horizontal-layout :width _MATCH_PARENT
                                          :height _WRAP_CONTENT
                                         (setq save-append-chk
                                               (checkbox "Save whole pattern"
                                                         :width "50%"
                                                         :checked "true"))
                                          (button "Reset" 
                                                  :width "50%"
                                                  :text-align "right"
                                                  :on-click (set-seek-value utility-bar 5)))


                       (horizontal-layout :width _MATCH_PARENT
                                          :height _WRAP_CONTENT
                                          (image :width _DEFAULT_SMALL_ICON_SIZE_DP
                                                 :height _DEFAULT_SMALL_ICON_SIZE_DP
                                                 :margin-left 5
                                                 :scaleType "CENTER_INSIDE"
                                                 :src (get-drawable-resource-id "ic_save_black_24dp")
                                                 :on-click (save-current-graph (is-checked save-append-chk)))
                                          (text "Name: ")
                                          current-name-text
                                          (image :width _DEFAULT_SMALL_ICON_SIZE_DP
                                                 :height _DEFAULT_SMALL_ICON_SIZE_DP
                                                 :margin-left 5
                                                 :scaleType "CENTER_INSIDE"
                                                 :src (get-drawable-resource-id "ic_launch_black_24dp")
                                                 :on-click (show-pattern-select-dialog)))
                       (horizontal-scrollview :width _MATCH_PARENT
                                              :height (+ 10 bar-max-height)
                                              sensor-graph)))

   (if test-robot-p
       (update-state-view-panel (list (list 255 forward-color) 
                                      (list 12 forward-color)
                                      (list 12 forward-color) 
                                      (list 34 forward-color)
                                      (list 56 forward-color)
                                      (list 67 right-color) 
                                      (list 78 right-color)
                                      (list 23 right-color)
                                      (list 45 right-color)
                                      (list 67 right-color)
                                      (list 87 right-color))
                                256 0))

 (defun create-direct-control-panel ()
    (vertical-layout :width _MATCH_PARENT
                     :height _WRAP_CONTENT
                     (button :width _WRAP_CONTENT
                             :height _WRAP_CONTENT
                             :parent-align "center"
                             :on-click (progn (do-timed-action _ACTION_KEY_FORWARD F awareness-map ) (update-sensor-display))
                             "Forward")
                     (relative :width _MATCH_PARENT
                               :height _WRAP_CONTENT
                               (button :width _WRAP_CONTENT
                                       :height _WRAP_CONTENT
                                       :parent-align "left"
                                       :on-click (progn (do-timed-action _ACTION_KEY_LEFT F awareness-map ) (update-sensor-display))
                                       "Left")
                               (button :width _WRAP_CONTENT
                                       :height _WRAP_CONTENT
                                       :parent-align "center"
                                       :on-click (progn (do-timed-action _ACTION_KEY_STOP F awareness-map ) (update-sensor-display))
                                       "Stop")
                               (button :width _WRAP_CONTENT
                                       :height _WRAP_CONTENT
                                       :parent-align "right"
                                       :on-click (progn (do-timed-action _ACTION_KEY_RIGHT F awareness-map ) (update-sensor-display))
                                       "Right"))
                     (button :width _WRAP_CONTENT
                             :height _WRAP_CONTENT
                             :parent-align "center"
                             :on-click (progn (do-timed-action _ACTION_KEY_BACKWARD F awareness-map)  (update-sensor-display))
                             "Backward")))

  (defun setup-robot ()
      (if test-robot-p
          (return))
      (setup-standard-rover)
      (set nxt current-nxt)
      (add-sensory-data awareness-map)
      (set started-p 1)
      (update-sensor-display))

  (defun do-timed-action (action-name output-has-sensors-before-action-p awareness-map state-update-lambda)
      (if is-recording-p
          (ex-append-action-step action-name))

      (if test-robot-p
          (return))

      (setq stop-time
            (if action-step-milli
                (+ (time) action-step-milli)))
      (execute-base-action action-name)
      (setq initial-awareness-map
            (copy-map awareness-map))
      (setq interrupted-p F)
      (unless (or (and stop-time
                       (> (time) stop-time))
                  interrupted-p
                  stop-process)
          (if state-update-lambda
              (setq interrupted-p
                    (funcall state-update-lambda 
                             (get-next-robot-state action-name awareness-map)))))

      (if stop-time
          (action-stop))

      (if output-has-sensors-before-action-p
          (progn
              (setq state-vector
                    (get-next-robot-state action-name initial-awareness-map))
              (add-sensory-data awareness-map))
          (progn
              (add-sensory-data awareness-map)
              (setq state-vector
                    (get-next-robot-state action-name awareness-map))))
      (append-view-state (gethash awareness-map _ULTRA_SONIC_DATA_KEY)
                         action-name)

    (if _PRIOR_STATE
        (add-state-vector-transition _PRIOR_STATE state-vector _TRANSITION_MODEL_HISTORY_LENGTH))

    (set _PRIOR_STATE state-vector)

      state-vector)

    (progn
      (multiple-bind (is-recording-p  was-reset-p step-delay-milli action-id)
                     (list F F 4000 0 ))
    

      (defun get-pattern-steps (name)
          (gethash _PATTERN_MAP name))

      (defun get-pattern-meta-data (pattern-name)
          (make-string-hashtable (list (list _BEING_RECOGNIZED_KEY (contains-hash-key recognition-group-names-map pattern-name)))))

      (defun leaf-action (action-name)
          (or (not (setq steps (gethash _PATTERN_MAP action-name)))
              (equals action-name
                      (singleton-list steps))))


      (defun background-run-pattern-name (pattern-name state-update-lambda on-complete-lambda)
          (evaluate-background (concat "running" pattern-name)
                                (lambda (result e) (if e 
                                                       (log-error "errors" e)
                                                       (progn
                                                          
                                                          (log-ui "nested-actions" 
                                                                "finished executing action " pattern-name))))
                                (setq out (run-pattern-recursive (get-list-iterator-with-index (get-pattern-steps pattern-name)) state-update-lambda))
                                (if on-complete-lambda (funcall on-complete-lambda out))))

      (defun background-run-pattern-iterator (pattern-iterator state-update-lambda on-complete-lambda)
          (evaluate-background (concat "running unknown pattern" )
                                (lambda (result e) (if e 
                                                       (log-error "errors" e)
                                                       ))
                                (setq out (run-pattern-recursive pattern-iterator state-update-lambda))
                                (if on-complete-lambda (funcall on-complete-lambda out))
                                ))

      ; [state-update-lambda] - optional lambda function that is notified on each action step iteration.  Takes the robot state-vec and the awareness-map
      ;                         in that order.  If this returns true then the pattern is interrupted
      (defun run-pattern-recursive (pattern-iterator state-update-lambda reset-view-state-p) 
          (setq pattern-root-path
                (list pattern-iterator))
          (setq interrupt-p F)
          (setq awareness-map
                (make-string-hashtable))
          (set stop-process F)
          (set running-p 1)
          (setq reset-view-state-p 1)
          (if reset-view-state-p
              (reset-view-state))

          (unless (or (empty-list pattern-root-path)
                      (not running-p)
                      stop-process
                      interrupt-p)
              (if (setq pattern-spec
                        (funcall (last pattern-root-path)))
                  (progn
                      (multiple-bind (pattern-step-name index)
                                     pattern-spec)
                      (log-ui "nested-actions" 
                              "executing action " pattern-step-name)
                      (if (singleton-list pattern-root-path)
                          (ex-update-executing-step pattern-step-name index))

                      (if (or (not (setq steps (gethash _PATTERN_MAP pattern-step-name)))
                              (equals pattern-step-name
                                      (singleton-list steps)))
                          (progn
                              (setq state-vec (do-timed-action pattern-step-name F awareness-map ))

                              (setq interrupt-p
                                    (or (if state-update-lambda
                                            (funcall state-update-lambda state-vec awareness-map))
                                        (< (gethash awareness-map _ULTRA_SONIC_DATA_KEY) 15)
                                        (= 1 (gethash awareness-map _LEFT_BUMPER_DATA_KEY))
                                        (= 1 (gethash awareness-map _RIGHT_BUMPER_DATA_KEY))))
                              (log-ui "nested-actions"
                                      "Interrupted: "
                                      (if interrupt-p "true" "false")
                                      " state: "
                                      (string awareness-map)))
                          (set pattern-root-path
                               (append-item pattern-root-path
                                            (get-list-iterator-with-index steps)))))
                  (set pattern-root-path
                       (all-but-last pattern-root-path))))
      
      (ex-update-executing-step F F)
      (log-ui "nested-actions" 
              "finished executing pattern")
      (stop-moving)
      interrupt-p)

    (defun get-pattern-iterator (pattern-list)
        (log-ui "get-pattern-iterator" 
                "Iterating over: " (string pattern-list))
        (setq pattern-iterator
              (get-list-iterator-with-index pattern-list))

        (setq pattern-root-path
              (list pattern-iterator))

        (setq i -1)
        
        (lambda ()

            (while (non-empty-list pattern-root-path)
                
                (if (not pattern-iterator)
                    (set pattern-iterator
                         (last pattern-root-path)))

                (if (setq pattern-spec (funcall pattern-iterator))
                    (progn
                        (multiple-bind (pattern-step-name index)
                                       pattern-spec)
                        (log-ui "nested-action-iterator" 
                                "executing action " pattern-step-name)
                        
                        (if (or (not (setq steps (gethash _PATTERN_MAP pattern-step-name)))
                                (equals pattern-step-name
                                        (singleton-list steps)))
                            (return (list pattern-step-name (set i (integer (+ 1 i)))))
                            (progn
                              (set pattern-iterator F)
                              (set pattern-root-path
                                   (append-item pattern-root-path
                                                (get-list-iterator-with-index steps))))))
                    (progn
                        (set pattern-iterator F)
                        (set pattern-root-path
                             (all-but-last pattern-root-path))))

              )
              F))

      )

  
  
  (setup-robot)

  (setq robot-control-view
        (scrollview :width _MATCH_PARENT
                    :height _MATCH_PARENT
                    :padding 1
                    (vertical-layout :width _MATCH_PARENT
                                     :height _WRAP_CONTENT

                                     (create-collapsible-container "Show configuration" "closed"
                                                                   (create-configuration-panel))
                                     (create-collapsible-container "Show action panel" "opened"
                                                                   action-pattern-display)
                                     (create-collapsible-container "Show state container" "closed"
                                                                   (create-state-view-panel))
                                     (create-collapsible-container "Show status panel" "opened"
                                                                   (create-status-panel))
                                     (create-collapsible-container "Show direction panel" "closed"
                                                                   (create-direct-control-panel))
                                     (horizontal-layout :width _MATCH_PARENT
                                                        :height _WRAP_CONTENT
                                               (shadow-button "stop"
                                                       :width "33%"
                                                       :on-click (stop-processing))
                                               (vertical-layout :width "33%"
                                                                :height _WRAP_CONTENT
                                                                (shadow-button "Save pattern-map"
                                                                               :width _WRAP_CONTENT
                                                                               :on-click (persist-motor-pattern (is-checked check-save-world-map)))
                                                                (setq check-save-world-map
                                                                      (checkbox "world")))
                                               
                                               (shadow-button "Disconnect"
                                                       :width "33%"
                                                       :on-click (full-exit))))))

; o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o 
;               Finish Robot control view
; o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o o(:)o 



  (setq content
        (relative :width _MATCH_PARENT
                  :height "100%"
                  robot-control-view))

  (defun show-main ()
    (remove-all-views content)
    (add-view content robot-control-view))

  (defun show-log ()
    (remove-all-views content)
    (add-view content log-view)
    (update-log-list))

  (defun show-settings ()
    (remove-all-views content)
    (add-view content robot-settings))

  (setq robot-view
        (vertical-layout :width _MATCH_PARENT
                         :height _MATCH_PARENT
                         (horizontal-layout :width _MATCH_PARENT
                                            :height _WRAP_CONTENT
                                            (shadow-button :width _WRAP_CONTENT
                                                           :height _WRAP_CONTENT
                                                           "Main"
                                                           :on-click (show-main))
                                            (shadow-button :width _WRAP_CONTENT
                                                           :height _WRAP_CONTENT
                                                           "Log"
                                                           :on-click (show-log))
                                            (shadow-button :width _WRAP_CONTENT
                                                           :height _WRAP_CONTENT
                                                           "Settings"
                                                           :on-click (show-settings))
                                            (shadow-button :width _WRAP_CONTENT
                                                          :height _WRAP_CONTENT
                                                          "Journal"
                                                          :on-click (show-journal-dialog)))
                        content))

  (set-top-view robot-view)
  (setq type-name-to-function-map
        (make-string-hashtable (list (list "ENUM" "vector-type-create-enum")
                                     (list "UNSIGNED_TALLY" "vector-type-create-tally-number")
                                     (list "SCALED_VALUE" "vector-type-create-scaled-number")
                                     (list "SET" "vector-type-create-set")
                                     (list "STRUCT" "vector-type-create-struct"))))


  (defun get-type-spec (type)
    (setq type-name (vector-type-get-name type))
    (cond 
      ((equals "ENUM"  type-name) (list "vector-type-create-enum" (enum-type-get-possible-values type)))
      ((equals "UNSIGNED_TALLY"  type-name) (list "vector-type-create-tally-number" (tally-type-get-max-value type)))
      ((equals "SCALED_VALUE" type-name) (list "vector-type-create-scaled-number" (scaled-number-get-min type) (scaled-number-get-max type) (scaled-number-get-num-steps type)))
      ((equals "SET" type-name) (list "vector-type-create-set" (set-type-get-possible-values type)))
      ((equals "STRUCT" type-name) (list "vector-type-create-struct" (make-string-hashtable (mapcar key (struct-type-get-field-names type) (list key (get-type-spec (struct-type-get-field-type type key))))))))

    )

  (defun create-type (spec)
    
    (if (equals "vector-type-create-struct" (first spec))
        (progn
            (setq arg (first (rest spec)))
            (funcall (first spec) 
                     (make-string-hashtable (mapcar key (get-hash-keys arg) (list key (create-type (gethash arg key)))))))
        (apply (first spec) (rest spec))))

  )




(comment

    (comment 
        ; rear bumper tests

        (get-rear-bumper-state)

        (progn
            (action-backward)
            (unless (= 1 (get-rear-bumper-state)))
            (action-stop))

      )

    (create-type spec)

    (first simple-robot-state-spec)
    (rest simple-robot-state-spec)

    (setq sonar-spec (get-type-spec sonar))

    (setq boolean-sensors-spec (get-type-spec boolean-sensors))

    (create-type boolean-sensors-spec)

    (struct-type-get-field-names simple-robot-state)

    (comment 
        ; Test vector types

        ; enum vector
        (setq actions
              (vector-type-create-enum ("left" "right" "forward" "backward" "stop" "stop-boundary")))

        ; set vector
        (setq boolean-sensors
              (vector-type-create-set ("collision-left" "collision-right")))

        ; tally vector
        (setq sonar (vector-type-create-tally-number 20))
        ; scaled-number

        (setq precision-sonar (vector-type-create-scaled-number 0 255 4))


        (setq reward
              (vector-type-create-scaled-number -10 10 3))

        (setq simple-robot-state
              (vector-type-create-struct (make-string-hashtable (list (list "actions" actions)
                                                               (list "collisions" boolean-sensors)
                                                               (list "sonar" sonar)
                                                               (list "reward" reward)))))

        (setq spec (get-type-spec simple-robot-state))

        (setq mask-spec
              (make-string-hashtable (list (list "actions" F)
                                           (list "collisions" ("collision-left"))
                                           (list "sonar" -3)
                                           (list "reward" F))))

        (vector-type-get-mask simple-robot-state mask-spec)

        ; simple sample
        (vector-type-sample actions)

        ; convert to viewable type

        (vector-type-vector-to-value actions (vector-type-sample actions))

        ; sample with filter
        (setq filter ("left" "right" "forward"))

        (vector-type-vector-to-value actions (vector-type-sample actions filter))

        (vector-type-get-mask actions filter)



        (setq type boolean-sensors)

        (vector-type-vector-to-value type (vector-type-sample type))

        (setq filter ("collision-left"))

        (vector-type-vector-to-value type (vector-type-sample type filter))


        (setq type sonar)
        (vector-type-vector-to-value type (vector-type-sample type))
        ; Values greater than or equal to 10
        (setq filter 10)
        ; values less than or equal to 10
        (setq filter -10)
        (vector-type-vector-to-value type (vector-type-sample type filter))

        ; mask
        (vector-type-get-mask type filter)



        (setq type precision-sonar)

        (vector-type-vector-to-value type (vector-type-sample type))


        (setq type reward)
        (vector-type-vector-to-value type (vector-type-sample type))



        (setq type simple-robot-state)
        (vector-type-vector-to-value type (vector-type-sample type ))

        (setq filter
              (make-string-hashtable (list (list "actions" F)
                                           (list "collisions" ("collision-left"))
                                           (list "sonar" -3)
                                           (list "reward" F))))

        (vector-type-vector-to-value type (vector-type-sample type filter))

        ; 
        (vector-type-sample type filter)
      )
    

    





    ; Test feature specific focus

    (setq focus-meta-key (Group.FOCUS_KEY))

    ; getting custom meta data

    (feature-get-custom-meta-data)
    
    

    ; setting custom meta data

    (feature-set-custom-meta-data)
    

    (report-get-text (setq internal-report (group-get-state-report memory-cache-group 1)))
    (report-get-log internal-report)

    (report-get-text (setq report (group-get-state-report memory-cache-group)))
    (report-get-warnings report)
    (report-get-text report)
    (report-get-log report)
    (setq report
          (group-fix-memory-inconsistencies memory-cache-group))

    
    (group-set-mode memory-cache-group "LEARNING")
    (group-get-mode memory-cache-group)

    ; Reset the utility after processign user utility
    (setq reset-user-utility-on-interrupt-p 1)


    (setq MINIMUM-UTILITY 0.1)

    (setq UTILITY-DECAY-FRACTION 0.9)

    (setq DECAY-ALL-FEATURES-P 1)

    (setq standard-pattern-decay 0.9)

    (setq test-model
          (first (setq ordered-features (group-get-ordered-processed-features fear-level-group))))

    ; View {pref value}/{utility value}

    (mapcar f 
            ordered-features 
            (list (group-get-feature-pref-value fear-level-group f)
                  (get-feature-utility f)))


    (decay-feature-model-utility test-model MINIMUM-UTILITY)

    ; simulate misprediction

    (progn
        ; life cycle of a model
        ; Something good happens
        (set-feature-base-utility test-model 1)

        ; It's desirability decays a bit with time

        (setq i 0)
        (progn
          (if (decay-feature-model-utility test-model MINIMUM-UTILITY)
              "extinct"
              (incr i)))

        ; The robot acts in support of the reward but is not rewarded

        (increment-feature-failure-count test-model)

        ; More decay
        (setq i 0)
        (progn
          (if (decay-feature-model-utility test-model MINIMUM-UTILITY)
              "extinct"
              (incr i)))

        ; Finally the motor pattern is rewarded.  Restore failure count and raise model utility
        (reset-feature-failure-count test-model)
        (set-feature-base-utility test-model
                                  (+ (get-feature-utility test-model)
                                     (* 0.2
                                        (- 1 (get-feature-utility test-model)))))


        ; more decay
        (setq i 0)
        (progn
          (if (decay-feature-model-utility test-model MINIMUM-UTILITY)
              "extinct"
              (incr i)))

        ; failures
        (increment-feature-failure-count test-model)

        ; More decay
        (setq i 0)
        (progn
          (if (decay-feature-model-utility test-model MINIMUM-UTILITY)
              "extinct"
              (incr i)))


      )


    (increment-feature-failure-count test-model)

    ; *****************************
    ; Tests 
    ; *****************************

    ; reducing feature-model pref
    (progn
      (group-decrease-feature-value-fraction fear-level-group test-model standard-pattern-decay)
      (group-get-feature-pref-value fear-level-group test-model)
      )

    ; in response to fear confirmation
    (progn
      (group-increase-feature-value-fraction fear-level-group
                                             test-model 
                                             0.1)
      (group-get-feature-pref-value fear-level-group test-model)
      (set-feature-base-utility test-model 0.8)
      )


    ; testing 

    ; pattern preference list

    (get-current-pattern-prefs-max-length)

    ; get current pattern preferences
    (get-current-pattern-preferences)

    ; update preferences

    (setq pattern-name (first (progn
                                      (setq map (make-string-hashtable (mapcar x (get-exploration-actions) (list x 1))))
                                      (non-empty-list (remove-string-duplicates (sample-n-strings (find x (get-current-pattern-preferences) (gethash map x)) 
                                                                                                  3
                                                                                                  1))))))

    (set-current-pattern-prefs-max-length 10)

    (progn
        (update-pattern-preferences pattern-name)
        (get-current-pattern-preferences))

    _PATTERN_MAP

    _INSTRUMENTATION_USE_NEW_MAP
    ; Switch to fast vector maps
    (switch-vmap-type 1)

    ; Switch to slow vector maps
    (switch-vmap-type F)

    (setq _INSTRUMENTATION_PLAN_WITH_MODEL F)

    (setq _INSTRUMENTATION_PLAN_WITH_MODEL 20)

    (setq _INSTRUMENTATION_PLAN_WITH_MODEL 10)

    (catch
      (extrap-motor-pattern-value _PRIOR_STATE
                                              (get-pattern-iterator (list "small-recover-left"))
                                              6
                                              15
                                              )
      e)

    (timed-execute (progn
                      (set _DEBUG_PLANNING 0)
                      (extrap-motor-pattern-value _PRIOR_STATE
                                              (get-pattern-iterator (list "small-recover-left"))
                                              6
                                              15
                                              )
                      _DEBUG_PLANNING
                    ))
    
    (timed-execute (copy-group memory-cache-group))

    (multiple-bind (test-group group-type)
                   (list memory-cache-group memcache-group-type))
    (multiple-bind (test-group group-type)
                   (list fear-level-group behavior-group-type))
    (multiple-bind (test-group group-type)
                   (list reward-seeking-group behavior-group-type))
    
    (group-get-model test-group)

    (progn
        (multiple-bind (serialization-time data)
                       (timed-execute (group-serialize test-group)))
        (multiple-bind (serialization-time restored-group)
                       (timed-execute (group-deserialize group-type data)))

        (group-set-mode restored-group "EXTRAPOLATION")

        (timed-execute (group-process-input restored-group _PRIOR_STATE))
      )

    


   (get-custom-motor-patterns)
   (get-hash-keys _PATTERN_MAP)
   (get-pattern-list-names)
   _PATTERN_MAP

   _EXPLORATORY_PATTERNS
   _HARM_RECOVERY_PATTERNS

   (display-boolean reset-user-utility-on-interrupt-p)
   (length (get-custom-motor-patterns))

    ; First set _PRIOR_STATE and select a motor pattern

    (setq action-iterator
          (get-pattern-iterator current-action-steps)) 

    (extrapolate-motor-pattern _PRIOR_STATE action-iterator)

    (setq action-value-spec
          (extrap-motor-pattern-value _PRIOR_STATE
                                      action-iterator))




  ; test extrap-motor-pattern-value

  

)






(progn

    ; ***********************************
    ; Base functions.  Execute this at least once when reloading
    ; *****************************************

    ; <><><><><><><><><><><><><><><><><><><><><><><><><>
    ; Step (1) - Execute this progn
    ; <><><><><><><><><><><><><><><><><><><><><><><><><>
    (setq _ACTION_STATUS_STOP_FAILURE "FAILED")
    (setq _ACTION_STATUS_STOP_INTERRUPT "INTERRUPTED")
    (setq _ACTION_STATUS_COMPLETE "COMPLETE")
    (setq _ACTION_STATUS_RUNNING "RUNNING")

    (setq _TEST_P 1)
    (setq _TEST_P F)
    (setq _GLOBAL_DROPBOX_BASE_PATH "/speechbot")

    (setq _LOADED_AI_LEGACY_UI_P F)

    (setq _RELOAD_AI_LEGACY_P F)

    (if (not _TEST_P)
        (if (or (not (var-exists-p "_LOADED_AI_LEGACY_UI_P"))
                _RELOAD_AI_LEGACY_P)
            (progn
                ; <><><><><><><><><><><><><><><><><><><><><><><><><>
                ; Step (2) - Execute this progn
                ; <><><><><><><><><><><><><><><><><><><><><><><><><> 
                (load-page-by-title "core-robot")
                (log-info "loaded legacy ai user interface")
                (set _RELOAD_AI_LEGACY_P F))))

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



    (defmacro initialization (robot-name)
      `(progn
        (defun log-robot (tag ...)

            (if ui-logging-enabled-p
                (progn
                    (setq data (join (mapcar x ... (if (string-p x) x (string x))) " "))
                    (log-ui tag message)
                  ) 
                
              )
          )
          (setq ctx ,robot-name)

          (setq BEHAVIOR_TYPE_NAME
                "REINFORCED-WORLD-GROUP-TYPE")

          (setq STANDARD_ROBOT_TYPE_NAME
                "GENERIC-WORLD-GROUP-TYPE")

          (setq NUM_CELL_STATES
                20)

          (setq _BEHAVIOR_GROUP_WEIGHT 1)
          (setq _MEMORY_CACHE_GROUP_WEIGHT 1)
          (setq _DEFAULT_FEATURE_BUFFER_LENGTH 4)
          (setq _MINIMUM_FOCUS_BUFFER_OVERLAP_LENGTH 3)
          ; max time spent learning
          (setq STANDARD_ROBOT_PROCESSING_MS 750)
          ; max time spent learning
          (setq BEHAVIOR_TYPE_PROCESSING_MS 4000)

          (setq FEAR_LEVEL_ROBOT_GROUP_NAME "FEAR-LEVEL")
          (setq REWARD_SEEKING_GROUP_NAME "REWARD-SEEKING")
          (setq MEMCACHE_GROUP_NAME "SHORT-TERM-MEMORY")
          (setq MIMIC_GROUP_NAME "MIMICRY")
          (setq _ROBOT_METADATA_KEY "robot-meta-data")
          (setq _ROBOT_CORE_DATA_KEY "robot-data")
          (setq _WORLD_NAME_DATA_KEY "WORLD-DATA-NAME")

          (setq _ENABLE_INTERNAL_REPORTING_P 1)

          (setq default-allocation 500)

          (setq robot F)

          (setq reward-seeking-group F)
          (setq fear-level-group F)
          (setq memory-cache-group F)

          (setq behavior-group-type F)
          (setq memcache-group-type F)

          (setq interrupt-p F)

          (setq current-state-vec F)
          (setq next-logical-input F)

          (setq previous-focus-group F)
          (setq previous-state F)

        (setq ui-logging-enabled-p F)
        

            ))

    (defmacro add-basic-action-and-sensor-functions ()
      '(progn

        (defun testSensorInterrupt (state)
            (or (and (< (gethash state "sonar") COLLISION-SONIC-THRESHOLD) "sonar")
                (and (gethash (gethash state "collisions") "collision-left") "collision-left")
                (and (gethash (gethash state "collisions") "collision-right") "collision-right")
                (and (gethash (gethash state "collisions") "collision-back") "collision-back")))
        
        (defun get-sonar-value ()
               (get-sonar))

        (defun add-sensors-to-state (state-map)
            (if (not state-map)
                (setq state-map
                      (make-string-hashtable)))
            (defhash state-map 
                     "sonar"
                     (get-sonar-value))

            (setq collision-map
                  (make-string-hashtable))

            (if (= 1 (get-left-bumper-state))
                (defhash collision-map "collision-left" 1)
                (remhash collision-map "collision-left"))

            (if (= 1 (get-right-bumper-state))
                (defhash collision-map "collision-right" 1)
                (remhash collision-map "collision-right"))

            (if (= 1 (get-rear-bumper-state))
                (defhash collision-map "collision-back" 1)
                (remhash collision-map "collision-back"))
            (defhash state-map
                     "collisions"
                     collision-map)
            state-map)

        (defun forward-duration (interval-ms stop-p)
            (setq stop-time
                  (+ (time) interval-ms))

            (action-forward)
            (unless (> (time) stop-time))
            (if stop-p
                (action-stop)))

        (defun backward-duration (interval-ms stop-p)
            (setq stop-time
                  (+ (time) interval-ms))

            (action-backward)
            (unless (> (time) stop-time))
            (if stop-p
                (action-stop)))

        (defun right-duration (interval-ms stop-p)
            (setq stop-time
                  (+ (time) interval-ms))

            (action-rotate-right)
            (unless (> (time) stop-time))
            (if stop-p
                (action-stop)))

        (defun left-duration (interval-ms stop-p)
            (setq stop-time
                  (+ (time) interval-ms))

            (action-rotate-left)
            (unless (> (time) stop-time))
            (if stop-p
                (action-stop)))
    ))

    (defmacro include-basic-action-and-sensor-functions (action-notification-runtime-interface)
      `(progn

        (defun testSensorInterrupt (state)
            (or (and (gethash (gethash state "collisions") "collision-left") "collision-left")
                (and (gethash (gethash state "collisions") "collision-right") "collision-right")
                (and (gethash (gethash state "collisions") "collision-back") "collision-back")
                (and (< (gethash state "sonar") COLLISION-SONIC-THRESHOLD) "sonar")))
        
        (defun get-sonar-value ()
               (get-sonar))

        (defun add-sensors-to-state (state-map)
            (if (not state-map)
                (setq state-map
                      (make-string-hashtable)))
            (defhash state-map 
                     "sonar"
                     (get-sonar-value))

            (setq collision-map
                  (make-string-hashtable))

            (if (= 1 (get-left-bumper-state))
                (defhash collision-map "collision-left" 1)
                (remhash collision-map "collision-left"))

            (if (= 1 (get-right-bumper-state))
                (defhash collision-map "collision-right" 1)
                (remhash collision-map "collision-right"))

            (if (= 1 (get-rear-bumper-state))
                (defhash collision-map "collision-back" 1)
                (remhash collision-map "collision-back"))
            (defhash state-map
                     "collisions"
                     collision-map)
            state-map)

        (defun forward-duration (interval-ms stop-p)

            (setq stop-time
                  (+ (time) interval-ms))

            (queue-runtime-expr ,action-notification-runtime-interface
                                (setq +current-base-action _ACTION_KEY_FORWARD))
            (action-forward)
            (unless (> (time) stop-time))
            (if stop-p
                (stop-with-notify)))

        (defun backward-duration (interval-ms stop-p)
            (setq stop-time
                  (+ (time) interval-ms))
            (queue-runtime-expr ,action-notification-runtime-interface
                                (setq +current-base-action _ACTION_KEY_BACKWARD))
            (action-backward)
            (unless (> (time) stop-time))
            (if stop-p
                (stop-with-notify)))

        (defun right-duration (interval-ms stop-p)
            (setq stop-time
                  (+ (time) interval-ms))

            (action-rotate-right)
            (queue-runtime-expr ,action-notification-runtime-interface
                                (setq +current-base-action _ACTION_KEY_RIGHT))
            (unless (> (time) stop-time))
            (if stop-p
                (stop-with-notify)))

        (defun left-duration (interval-ms stop-p)
            (setq stop-time
                  (+ (time) interval-ms))

            (action-rotate-left)
            (queue-runtime-expr ,action-notification-runtime-interface
                                (setq +current-base-action _ACTION_KEY_LEFT))
            (unless (> (time) stop-time))
            (if stop-p
                (stop-with-notify)))

        (defun stop-with-notify ()
            (action-stop)
            (queue-runtime-expr ,action-notification-runtime-interface
                    (setq +current-base-action _ACTION_KEY_STOP))
          )

        (defun add-item-to-state-situation-set (item)
            (defhash (gethash outer-state "situation") item 1)
            outer-state)
    ))

    (defmacro robot-group-setup (robot-name robot-type-map)
      `(progn

        (setq base-type
              ,robot-type-map)

        (setq name ,robot-name)

        (setq num-input-output-nodes
              (vector-type-get-width ,robot-type-map))



        ; Helper
        (defun get-default-memory-listener (group-name)
          (lambda (is-starting-p result)
                    (if is-starting-p
                        (progn
                          (println "sleep" "dreaming starting for " group-name " group")
                          )
                        (progn
                           (println "sleep" "Finished dreaming for " group-name " group.  Released: " result)
                          )
                        )))

        ; Creates new robot if it doesn't exist
        ; TODO: refactor the way data is persisted.  Should be based on: instance-name = context, field name = key name
        (defun reload-robot (create-new-p)
            (do-base-configuration)

            (setq start (time))

            (setq data-b64
                  (get-data-value name _ROBOT_CORE_DATA_KEY))
            
            (if data-b64
                (world-load-serialized-group-data robot data-b64))

            (setq load-time
                  (integer (/ (- (time) start) 1000)))

            (if (or create-new-p (not data-b64))
                (return (create-initial-data)))

            (log-info "reloading" (concat "reloading existing: " (string create-new-p)))
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

            (do-world-post-configuration)
            load-time)

        (defun do-base-configuration ()
            (model-set-thread-count 4)

            (set robot
                 (model-create-world default-allocation (model-get-default-config)))

            (set behavior-group-type
                 (world-create-group-type robot 
                                          BEHAVIOR_TYPE_NAME
                                          num-input-output-nodes
                                          NUM_CELL_STATES
                                          _BEHAVIOR_GROUP_WEIGHT
                                          _DEFAULT_FEATURE_BUFFER_LENGTH
                                          _MINIMUM_FOCUS_BUFFER_OVERLAP_LENGTH
                                          (model-get-default-config)))

            (set memcache-group-type
                 (world-create-group-type robot 
                                          STANDARD_ROBOT_TYPE_NAME
                                          num-input-output-nodes
                                          NUM_CELL_STATES
                                          _MEMORY_CACHE_GROUP_WEIGHT
                                          _DEFAULT_FEATURE_BUFFER_LENGTH
                                          _MINIMUM_FOCUS_BUFFER_OVERLAP_LENGTH
                                          (model-get-default-config)))

            (group-type-set-default-string-serializer behavior-group-type)
            (group-type-set-default-string-serializer memcache-group-type))

        ; Call this after adding/configuring groups
        (defun do-world-post-configuration ()
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



        (defun create-initial-data ()
            (do-base-configuration)
            (log-info "reloading" "created-new")

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
            (do-world-post-configuration)
            0)

        (defun process-next-state (state-map viewer group)
            (setq start-time (time))
            (setq state-vec
                  (vector-type-value-to-vector base-type 
                                               state-map))
            (setq group (or group memory-cache-group))
            (setq mode (group-process-input group state-vec))
            (if viewer
                (log-robot "processing" (concat "Processed input: " (funcall viewer state-map) ".  Result: [" mode "] Time: " (string (integer (- (time) start-time)))))
                (log-robot "processing" (concat "Processed input: " (string state-map) ".  Result: [" mode "] Time: " (string (integer (- (time) start-time))))))
            
            )

        (defun get-matching-features (group)
            (find feature
                  (group-get-ordered-processed-features group)
                  (equals (feature-get-state feature) "MATCHING")
                 ))

        (defun set-feature-data (feature key value)
            (feature-set-custom-meta-data feature 
                                          (defhash* (or (feature-get-custom-meta-data feature)
                                                        (make-string-hashtable))
                                                    key
                                                    value))

            feature)

        (defun get-feature-data (feature key)
            (gethash (or (feature-get-custom-meta-data feature)
                         (make-string-hashtable))
                     key))

        (defun set-feature-name (feature name)
            (set-feature-data feature "id" name)
            )

        (defun get-feature-name (feature)
          (get-feature-data feature "id"))

        (defun get-feature-outcome-delay-history (feature)
            (or (get-feature-data feature "delay-history")
                (list)))


        (defun append-feature-outcome-delay-ms (feature delay-ms limit)
           (setq limit 
                 (or limit 30))
           (set-feature-data feature 
                             "delay-history"
                             (right-list (append-item (get-feature-outcome-delay-history feature)
                                                      delay-ms)
                                         limit)))

        (defun get-feature-event-probability (feature)
              (if ( > (setq event-count
                            (length (setq history (get-feature-outcome-delay-history feature))))
                      0)
                  (/ (length (find event history))
                     event-count))
          )

        (defun transfer-feature-metadata (group source target)
            
            (if (and (setq source-pred-prob
                           (get-feature-event-probability source))
                     (setq target-pred-prob
                           (get-feature-event-probability target)))
                (if (<= target-pred-prob source-pred-prob)
                    (group-increase-feature-value-fraction group target (/ (- source-pred-prob target-pred-prob) source-pred-prob))
                    (group-decrease-feature-value-fraction group target (/ source-pred-prob target-pred-prob))))
            (set-feature-data target
                              "delay-history"
                              (get-feature-outcome-delay-history source)))



        (defun process-batch-input (state-map-list group boundary-on-end-p boundary-on-start-p viewer)
              (if boundary-on-start-p
                  (group-reset group))

              (setq prev (if (and (setq prior-focus (group-get-focus-feature group))
                                  (equals "BUILDING" (feature-get-state prior-focus)))
                             prior-focus))

              (if viewer
                  (log-robot "learning batch" (string (mapcar x state-map-list (funcall viewer x))))
                  (log-robot "learning batch" (string state-map-list)))
              
              (setq start-time (time))
              (for (data i)
                   state-map-list
                   (progn
                      (log-robot "timing" "Total time for Group: [" (group-get-name group) "]  to learn pattern: " (string (integer (/ (- (time) start-time) 1000))))
                      (log-info "timing" "Total time for Group: [" (group-get-name group) "]  to learn pattern: " (string (integer (/ (- (time) start-time) 1000))))
                      (if boundary-on-end-p
                          (group-reset group)))
                   (progn
                      (setq mode (group-process-input group
                                                      (vector-type-value-to-vector base-type 
                                                                                   data)))
                      
                      (setq focus
                            (group-get-focus-feature group))
                      (if (and focus (not (equals focus prev)))
                          (set-feature-data (set-feature-name focus 
                                                              (setq feature-name (concat (group-get-name group) "-" (unique-id))))
                                            "creation-datetime"
                                            (get-datetime-string)))
                      (set prev focus))))

        (defun force-boundary ()
            (group-reset memory-cache-group)
            (group-reset reward-seeking-group)
            (group-reset fear-level-group))

        (defun transform-focus-to-harm ()
            (if (setq short-term-feature
                      (group-get-focus-feature memory-cache-group))
                (progn
                    (log-robot "focus" "Was set")
                    (setq copied (feature-copy short-term-feature))
                    (group-import-feature fear-level-group copied 1)
                  )))
      

        (defun save-world ()
            (setq start (time))
            (setq data-b64
                  (world-serialized-group-data robot))
                    
            
            (set-data-value name data-b64 _ROBOT_CORE_DATA_KEY)
            (/ (- (time) start) 1000))))

   (defun map-difference (first second transform-lambda)
      (setq transform-lambda 
            (or transform-lambda
                (lambda (map key second)
                      (gethash map key))))
      (if (list-p second)
          (setq second
                (make-string-hashtable (mapcar v second (list v 1)))))

      (make-string-hashtable (map-filter key
                                        (get-hash-keys first)
                                        (and (not (gethash second key))
                                             (list key 
                                                   (funcall transform-lambda first key second))))))

   (defun temporal-enqueue (tqueue item)
      (if (not tqueue)
          (setq tqueue ()))
      (append-item tqueue (list (time) item)))

   (defun temporal-queue-update (tqueue max-age-ms)
      (setq cutoff (- (time) max-age-ms))
      (if (or (not tqueue)
              (empty tqueue))
          (return ()))
      (while (and (non-empty tqueue)
                  (setq head (first tqueue))
                  (< (first head)
                     cutoff))
          (setq tqueue
                (rest tqueue)))
      tqueue)

   (defun temporal-dequeue (tqueue max-age-ms)
      (setq new
            (temporal-queue-update tqueue max-age-ms))
      (setq v (first new))
      (setq remaining
            (rest new))
      (list remaining v))

   ; item-spec is (key future-delay lambda)
   ; If lambda
   (defun add-to-action-timeline (timeline-list item-spec)
      (multiple-bind (key offset-ms action-fct)
                     item-spec)
      (append-item (or timeline-list ()) 
                   (list key (+ (time) offset-ms) action-fct)))

   (defun refresh-timeline (timeline-list range)
      (setq new-timeline ())
      (setq current-time (time))
      (setq expired-items ())

      (if range
          (if (list-p range)
              (multiple-bind (min-value max-value) range)
              (multiple-bind (min-value max-value) 
                             (list 0
                                   (+ current-time (/ range 2)))))
          (multiple-bind (min-value max-value)
                         (list 0 current-time)))

      (for item-spec
           timeline-list
           (list new-timeline expired-items)
           (if (and (multiple-bind (key cutoff-time action-fct)
                                   item-spec)
                    (<= min-value cutoff-time)
                    (>= max-value cutoff-time))
                (set new-timeline
                     (append-item new-timeline item-spec))
                (set expired-items
                     (append-item expired-items item-spec))
                )))

   (defun remove-all-timeline-items (timeline-list item-key)
      (setq new-timeline ())
      (for item-spec
           timeline-list
           new-timeline
           (if (and (multiple-bind (key cutoff-time action-fct)
                                   item-spec)
                    (equals key item-key))
                (set new-timeline
                     (append-item new-timeline item-spec)))))



   (defmacro save-global-var (var)
      `(set-data-value (string (quote ,var)) ,var "GLOBAL-VARIABLE"))

   (defmacro load-global-var (var)
      `(get-data-value (string (quote ,var)) "GLOBAL-VARIABLE")
    )



   (defun get-state-map-string (state)
        (join (mapcar key 
                      (get-hash-keys state)
                      (if (equals key "collisions")
                          (concat key ":" (string (get-hash-keys (gethash state "collisions"))))
                          (if (equals key "situation")
                              (concat key ":" (string (get-hash-keys (gethash state "situation"))))
                              (concat key ":" (string (gethash state key))))))
                    " "))

   (defun copy-group (group)
      (group-deserialize (group-get-type group) (group-serialize group)))

   (defun add-to-temporal-map (map key value)
      (setq update-time (time))
      (multiple-bind (create-time update-time old-value)
                     (or (gethash map key)
                         (list (time) (time) value)))

      (defhash map 
               key
               (list create-time (time) (or value old-value))))


   (defun update-temporal-map (map ejection-ms-ago)
      (setq cutoff-time
            (- (time) ejection-ms-ago))

      (setq rejected ())
      (for key
           (get-hash-keys map)
           rejected
           (if (and (multiple-bind (create-time refresh-time value)
                                   (setq item
                                         (gethash map key)))
                    (< refresh-time cutoff-time))
               (progn
                  (remhash map key)
                  (set rejected (append-item rejected item))))))


   (setq playing-ringtone-p F)
   (defun play-positive-ringtone ()

      (unless (not (is-playing-ringtone 8)))
      (repeat-ringtone-by-index 8 1 800 3000 (lambda (result error)
                                                                  (if error
                                                                     (log-error "ringtone"
                                                                               error))
                                                                  (set playing-ringtone-p F)))
    )


   (defun play-confused-ringtone ()

      (unless (not (is-playing-ringtone 0)))
      (repeat-ringtone-by-index 0 1 700 3000 (lambda (result error)
                                                                  (if error
                                                                     (log-error "ringtone"
                                                                               error))
                                                                  (set playing-ringtone-p F)))
    )


   (defun play-angry-ringtone ()

      (unless (not (is-playing-ringtone 0)))
      (repeat-ringtone-by-index 0 1 1000 3000 (lambda (result error)
                                                                  (if error
                                                                     (log-error "ringtone"
                                                                               error))
                                                                  (set playing-ringtone-p F)))
    )


   (defun play-curiosity-1-ringtone ()

      (unless (not (is-playing-ringtone 9)))
      (repeat-ringtone-by-index 9 1 1000 3000 (lambda (result error)
                                                                  (if error
                                                                     (log-error "ringtone"
                                                                               error))
                                                                  (set playing-ringtone-p F)))
    )

   (defun play-curiosity-2-ringtone ()

      (unless (not (is-playing-ringtone 9)))
      (repeat-ringtone-by-index 9 1 1700 3000 (lambda (result error)
                                                                  (if error
                                                                     (log-error "ringtone"
                                                                               error))
                                                                  (set playing-ringtone-p F)))
    )


(defun play-curiosity-3-ringtone ()

      (unless (not (is-playing-ringtone 9)))
      (repeat-ringtone-by-index 9 1 2300 3000 (lambda (result error)
                                                                  (if error
                                                                     (log-error "ringtone"
                                                                               error))
                                                                  (set playing-ringtone-p F)))
    )

   (defun show-ringtone-select-dialog (on-select-lambda)
        (setq ringtone-select-item-height 50)
        (setq ringtone-dialog-height 200)
        (set playing-ringtone-p F)
        (setq selected-ringtone-index F)
        (setq ringtone-title-height 30)

        (defun create-tone-list-item (i)
              (setq title
                    (get-ringtone-name-by-index i))

              (text :width _MATCH_PARENT
                    :heigth ringtone-select-item-height
                    title
                    :on-click (if (not playing-ringtone-p)
                                  (progn
                                    (set playing-ringtone-p 1)
                                    (set selected-ringtone-index i)
                                    (repeat-ringtone-by-index i
                                                              3
                                                              250
                                                              5000
                                                              (lambda (result error)
                                                                  (if error
                                                                     (log-error (concat "title: " title)
                                                                               error))
                                                                  (set playing-ringtone-p F)))))))
        (dialog :dialog-title "Choose your ringtone"
                :on-positive (if on-select-lambda
                                 (funcall on-select-lambda selected-ringtone-index))
                :positive-text "Ok"
                :cancel-text "Cancel"
                (scrollview :width _MATCH_PARENT
                            :height ringtone-dialog-height
                            (vertical-layout :width _MATCH_PARENT
                                             :height _WRAP_CONTENT
                                             (mapcar (uri-name-pair i)
                                                     (get-ringtones)
                                                     (create-tone-list-item i))))))

    
    

)


(comment
  ; Main Application Comment block 3/22/2019
  
  (progn

      ; **************************************
      ; Applying functions above
      ; This code makes more specific assumptions about the structure of the robot
      ; motor pattern analysis
      ; **************************************

      (progn
        ; ********************************
        ; General cnstants
        ; **********************************
        ; <><><><><><><><><><><><><><><><><><><><><><><><><>
        ; Step (3) - Execute this progn
        ; <><><><><><><><><><><><><><><><><><><><><><><><><>

          (setq LEFT-90-DURATION
                2500)

          (setq RIGHT-90-DURATION
                2500)

          (setq SHORT-DURATION 3000)
          (setq MEDIUM-DURATION 5000)
          (setq LONG-DURATION 10000)
          (setq FOREVER-DURATION 15000)

          (setq action-sequence
                    ())

          (setq UPDATE-INTERVAL-MS 250)

          (setq COLLISION-SONIC-THRESHOLD 25)

          
          (setq action-map
              (make-string-hashtable (list (list "forward-short" '(forward-duration SHORT-DURATION))
                                           (list "forward-medium" '(forward-duration MEDIUM-DURATION))
                                           (list "forward-long" '(forward-duration LONG-DURATION))
                                           (list "forward-forever" '(forward-duration FOREVER-DURATION))

                                           (list "backward-short" '(backward-duration SHORT-DURATION))
                                           (list "backward-medium" '(backward-duration MEDIUM-DURATION))
                                           (list "backward-long" '(backward-duration LONG-DURATION))
                                           (list "backward-forever" '(backward-duration FOREVER-DURATION))

                                           (list "right-45" '(right-duration (/ RIGHT-90-DURATION 2)))
                                           (list "left-90" '(left-duration LEFT-90-DURATION))
                                           (list "left-180" '(left-duration (* 2 LEFT-90-DURATION)))
                                           (list "left-360" '(left-duration (* 4 LEFT-90-DURATION)))
                                           (list "left-forever" '(left-duration 10000))

                                           (list "right-45" '(right-duration (/ RIGHT-90-DURATION 2)))
                                           (list "right-90" '(right-duration RIGHT-90-DURATION))
                                           (list "right-180" '(right-duration (* 2 RIGHT-90-DURATION)))
                                           (list "right-360" '(right-duration (* 4 RIGHT-90-DURATION)))
                                           (list "right-forever" '(right-duration 10000))

                                           (list "around-left-short" '(progn (left-duration LEFT-90-DURATION) (forward-duration SHORT-DURATION) (right-duration RIGHT-90-DURATION)))
                                           (list "around-left-medium" '(progn (left-duration LEFT-90-DURATION) (forward-duration MEDIUM-DURATION) (right-duration RIGHT-90-DURATION)))

                                           (list "around-right-short" '(progn (right-duration RIGHT-90-DURATION) (forward-duration SHORT-DURATION) (left-duration LEFT-90-DURATION)))
                                           (list "around-right-medium" '(progn (right-duration RIGHT-90-DURATION) (forward-duration MEDIUM-DURATION) (left-duration LEFT-90-DURATION))))))

          (defun view-state-list (state-list)
              (defun get-state-view (state)
                  (text (join (mapcar key 
                                      (get-hash-keys state)
                                      (if (equals key "collisions")
                                          (concat key ":" (string (get-hash-keys (gethash state "collisions"))))
                                          (concat key ":" (string (gethash state key)))))
                              " ")))

              (defun add-state-list (state-list)
                  (remove-all-views state-container)
                  (for s
                       state-list
                       F
                       (add-view state-container (get-state-view s))))

              (setq this-dialog
                    (dialog :dialog-title "State View"
                            :on-positive (dismiss-dialog this-dialog)
                            :positive-text "Ok"
                            (vertical-layout :width _MATCH_PARENT
                                             :height _MATCH_PARENT
                                             (text (concat "Data:" (string (length state-list))))

                                             (scrollview :width _MATCH_PARENT
                                                         :height _MATCH_PARENT
                                                         (setq state-container
                                                                (vertical-layout :width _MATCH_PARENT
                                                                                 :height _WRAP_CONTENT
                                                                                 ))))))
              (add-state-list state-list))

          (defun run-physical-robot (action-expr stop-when-finish-p allow-sensor-interrupt-p)
                (setq interface (queue-thread (quit sensor-interrupt-p +action-complete-p)
                                (list F F F)
                                (progn
                                    (setq allow-sensor-interrupt-p 1)
                                    (add-basic-action-and-sensor-functions)
                                    (set action-sequence ())
                                    (setq inner-interface
                                          (queue-thread (-action-interrupt done state)
                                                        (list F F outer-state)
                                                        (unless (or done -action-interrupt)
                                                            (eval action-expr)
                                                            (setq done 1))
                                                        (if done 
                                                            (set +action-complete-p 1)
                                                            (println "Action interrupted"))))
                                    (setq sensor-interrupt-p
                                          (and allow-sensor-interrupt-p (testSensorInterrupt outer-state)))
                                    (if sensor-interrupt-p
                                        (queue-runtime-expr inner-interface
                                                            (setq -action-interrupt 1)))

                                    (setq update-time 0)

                                    (setq i 0)

                                    (unless (or sensor-interrupt-p quit +action-complete-p)
                                        (incr i)
                                        (queue-runtime-expr inner-interface
                                                            (setq state outer-state))
                                        (setq outer-state
                                              (add-sensors-to-state outer-state))
                                        
                                        (if (> (time) update-time)
                                            (progn
                                              (log-info "sensor" (concat (string i) ") " (string (gethash outer-state "sonar"))))
                                              (set action-sequence (append-item action-sequence (copy-map outer-state)))
                                              
                                              (set update-time (+ UPDATE-INTERVAL-MS (time)))))

                                        (setq sensor-interrupt-p
                                              (and allow-sensor-interrupt-p (testSensorInterrupt outer-state)))
                                        (if sensor-interrupt-p
                                            (progn
                                                (log-info "Timings" (concat (string i) ") stop signal " (string (time))))
                                                (action-stop)
                                                (queue-runtime-expr inner-interface
                                                                    (setq -action-interrupt 1)))))
                                    (break-runtime inner-interface)

                                    (if stop-when-finish-p
                                        (progn
                                            (action-stop)
                                            (log-info "Timings" (concat "stop final " (string (time))))))
                                    (evaluate-foreground (lambda (result error) (if error (log-error "speech error" error))) (tts "Finished"))
                                    )
                                    (if +action-complete-p 
                                        (set _ROBOT_SUCCESS "Success")
                                        (if quit
                                            (set _ROBOT_SUCCESS "User interrupt")
                                            (set _ROBOT_SUCCESS "harm avoidance"))))))

          (comment
            ; Test running physical robot
            (setq robot-runtime 
                  (run-physical-robot (gethash action-map "forward-long")
                                       1))

            (evaluate-foreground (lambda (result error) (if error (log-error "show dialog error" error))) (view-state-list action-sequence))


            (defun set-base-duration (dur)
                (set RIGHT-90-DURATION dur)
                (set LEFT-90-DURATION dur))

            (set-base-duration 1000)
            (set-base-duration 1250)
            (set-base-duration 1500)
            (set-base-duration 1750)
            (set-base-duration 2000)
            (set-base-duration 2250)
            (set-base-duration 2500)
            (set-base-duration 2750)
            (set-base-duration 3000)
            (set-base-duration 3250)
            (set-base-duration 3500)
            
            (setq robot-runtime 
                  (run-physical-robot (gethash action-map "left-90")
                                       1))

            (setq robot-runtime 
                  (run-physical-robot (gethash action-map "right-90")
                                       1))
            
            (queue-runtime-expr robot-runtime (setq quit 1))

            (action-stop))
        ; end step 3 progn  
        )
    
    (progn
        ; <><><><><><><><><><><><><><><><><><>
        ;   Step 4 - Execute experimental code
        ; <><><><><><><><><><><><><><><><><><>

        (defun run-smart-action (top-action stop-when-finish-p robot-name state-type save-world-when-finished-p create-new-p)
            (setq action-expr
                  (gethash action-map top-action))
            (setq interface (queue-thread (quit sensor-interrupt-p +action-complete-p +sensor-complete-p +current-base-action)
                            (list F F F F F)
                            (progn
                                (include-basic-action-and-sensor-functions (get-runtime-interface))
                                (defun complete-state (state-map)
                                    (if (not state-map)
                                        (setq state-map (make-string-hashtable)))

                                    (defhash state-map "actions" (or +current-base-action _ACTION_KEY_STOP))
                                    (defhash state-map "situation" (list top-action))
                                    (defhash state-map "reward" 0)
                                    state-map)

                                (setq outer-state (add-sensors-to-state (complete-state)))
                                (set action-sequence ())
                                (set state-queue (concurrency-make-queue))
                                (concurrency-enqueue state-queue outer-state)

                                (defun enqueue-next-state (state)
                                    (concurrency-enqueue state-queue state))

                                (defun dequeue-oldest-state ()
                                    (concurrency-dequeue state-queue))

                                (setq inner-interface
                                      (queue-thread (-action-interrupt done state)
                                                    (list F F outer-state)
                                                    (unless (or done -action-interrupt)
                                                        (eval action-expr)
                                                        (setq done 1))
                                                    (if done 
                                                        (set +action-complete-p 1)
                                                        (println "Action interrupted"))))

                                (setq learning-interface
                                      (queue-thread (-action-interrupt)
                                                    (list F )
                                                    (progn
                                                        (initialization robot-name)
                                                        (robot-group-setup robot-name state-type (vector-type-get-width state-type))
                                                        (reload-robot create-new-p)
                                                        (group-reset memory-cache-group)

                                                        (unless (and -action-interrupt
                                                                     (concurrency-queue-is-empty state-queue))
                                                                (if (setq next-state
                                                                          (dequeue-oldest-state))
                                                                    (process-next-state next-state)))

                                                        (if save-world-when-finished-p
                                                            (progn
                                                              (setq save-time (save-world))
                                                              (log-info "saved" (concat "Saved world " robot-name " after " (string (integer save-time)) " ms"))
                                                              ))
                                                      )
                                                    
                                                    (set +sensor-complete-p 1)))


                                (setq sensor-interrupt-p
                                      (testSensorInterrupt outer-state))
                                (if sensor-interrupt-p
                                    (progn
                                      (queue-runtime-expr learning-interface
                                                          (set -action-interrupt 1))                                      
                                      (queue-runtime-expr inner-interface
                                                          (set -action-interrupt 1))))

                                (setq update-time 0)

                                (setq i 0)

                                (unless (or sensor-interrupt-p quit +action-complete-p)
                                    (incr i)
                                    (queue-runtime-expr inner-interface
                                                        (setq state (copy-map outer-state)))
                                    (setq outer-state
                                          (add-sensors-to-state (complete-state outer-state)))
                                    
                                    (if (> (time) update-time)
                                        (progn
                                          
                                          (set action-sequence (append-item action-sequence (copy-map outer-state)))
                                          (enqueue-next-state (copy-map outer-state))
                                          (set update-time (+ UPDATE-INTERVAL-MS (time)))))

                                    (setq sensor-interrupt-p
                                          (testSensorInterrupt outer-state))
                                    (if sensor-interrupt-p
                                        (progn
                                                                                
                                          (queue-runtime-expr inner-interface
                                                              (set -action-interrupt 1)))))
                                (queue-runtime-expr learning-interface
                                                    (set -action-interrupt 1))
                              
                                (if stop-when-finish-p
                                    (progn
                                        (action-stop)
                                        ))
                                (unless +sensor-complete-p)
                                (evaluate-foreground (lambda (result error) (if error (log-error "speech error" error))) (tts "Finished"))
                                )
                                (if +action-complete-p 
                                    (set _ROBOT_SUCCESS "Success")
                                    (if quit
                                        (set _ROBOT_SUCCESS "User interrupt")
                                        (set _ROBOT_SUCCESS "harm avoidance"))))))


      (defun simple-robot-group-state-view (group group-state-type)
            (defun get-state-map-string (state)
                (join (mapcar key 
                              (get-hash-keys state)
                              (if (equals key "collisions")
                                  (concat key ":" (string (get-hash-keys (gethash state "collisions"))))
                                  (if (equals key "situation")
                                      (concat key ":" (string (get-hash-keys (gethash state "situation"))))
                                      (concat key ":" (string (gethash state key))))))
                            " "))

            (defun get-standard-feature-view (feature-model)
                (setq state-vectors
                      (feature-extrapolate-values feature-model)) 

                (setq vlayout
                      (vertical-layout :width _MATCH_PARENT
                                       :height _WRAP_CONTENT
                                       (text :width _MATCH_PARENT
                                             (string (feature-get-custom-meta-data feature-model)))
                                       ))

                (for predicted-state-vector
                     state-vectors
                     vlayout
                     (add-view vlayout
                               (vertical-layout :width _MATCH_PARENT
                                                :height _WRAP_CONTENT
                                                (solid :width _MATCH_PARENT
                                                       :height 1
                                                       :background-color "#531018")
                                                (text :width _MATCH_PARENT
                                                      (get-state-map-string (vector-type-vector-to-value group-state-type predicted-state-vector)))))))

          (vertical-layout :width _MATCH_PARENT
                           :height _WRAP_CONTENT
                           (mapcar feature
                                   (group-get-ordered-processed-features group)
                                   (get-standard-feature-view feature))))


      (defun show-robot-world (name group-name state-type)
            (initialization name)
            (robot-group-setup name state-type)
            (reload-robot)
            (evaluate-foreground  (lambda (result error) (if error (log-error "Group View Error" error)))
              (dialog 
                  :dialog-title (concat "Robot: " name " Group: " group-name)
                  (scrollview :width _MATCH_PARENT
                              :height _MATCH_PARENT
                              (simple-robot-group-state-view (world-find-group robot group-name) state-type)))
              ))


      (defun run-action-learn-harm (action-map top-action stop-when-finish-p robot-name state-type save-world-when-finished-p create-new-p)
            (setq action-expr
                  (second (gethash action-map top-action)))
            (setq interface (queue-thread (quit sensor-interrupt-p +action-complete-p +sensor-complete-p +current-base-action)
                                          (list F F F F F)
                                          (progn
                                              (include-basic-action-and-sensor-functions (get-runtime-interface))
                                              (defun complete-state (state-map)
                                                  (if (not state-map)
                                                      (setq state-map (make-string-hashtable)))

                                                  (defhash state-map "actions" (or +current-base-action _ACTION_KEY_STOP))
                                                  (defhash state-map "situation" (list top-action))
                                                  (defhash state-map "reward" 0)
                                                  state-map)

                                              (setq outer-state (add-sensors-to-state (complete-state)))
                                              (log-robot "Initial state" outer-state)
                                              (set action-sequence ())
                                              (set state-queue (concurrency-make-queue))
                                              (concurrency-enqueue state-queue outer-state)
                                              (initialization robot-name)
                                              (robot-group-setup robot-name state-type (vector-type-get-width state-type))
                                              (reload-robot create-new-p)
                                              (group-reset memory-cache-group)

                                              (defun enqueue-next-state (state)
                                                  (concurrency-enqueue state-queue state))

                                              (defun dequeue-oldest-state ()
                                                  (concurrency-dequeue state-queue))

                                              (setq inner-interface
                                                    (queue-thread (-action-interrupt done state)
                                                                  (list F F outer-state)
                                                                  (unless (or done -action-interrupt)
                                                                      (eval action-expr)
                                                                      (setq done 1))
                                                                  (if done 
                                                                      (set +action-complete-p 1)
                                                                      (println "Action interrupted"))))

                                              (setq learning-interface
                                                    (queue-thread (-action-interrupt)
                                                                  (list F )
                                                                  (progn


                                                                      (unless (and -action-interrupt
                                                                                   (concurrency-queue-is-empty state-queue))
                                                                              (if (setq next-state
                                                                                        (dequeue-oldest-state))
                                                                                  (progn
                                                                                      (process-next-state next-state)

                                                                                      )))
                                                                      
                                                                      
                                                                    )
                                                                  
                                                                  (set +sensor-complete-p 1)))


                                              (setq sensor-interrupt-p
                                                    (testSensorInterrupt outer-state))
                                              (if sensor-interrupt-p
                                                  (progn
                                                    (queue-runtime-expr learning-interface
                                                                        (set -action-interrupt 1))                                      
                                                    (queue-runtime-expr inner-interface
                                                                        (set -action-interrupt 1))))

                                              (setq update-time 0)

                                              (setq i 0)

                                              (unless (or sensor-interrupt-p quit +action-complete-p)
                                                  (incr i)
                                                  (queue-runtime-expr inner-interface
                                                                      (setq state (copy-map outer-state)))
                                                  (setq outer-state
                                                        (add-sensors-to-state (complete-state outer-state)))
                                                  
                                                  (if (> (time) update-time)
                                                      (progn
                                                        
                                                        (set action-sequence (append-item action-sequence (copy-map outer-state)))
                                                        (enqueue-next-state (copy-map outer-state))
                                                        (set update-time (+ UPDATE-INTERVAL-MS (time)))))

                                                  (setq sensor-interrupt-p
                                                        (testSensorInterrupt outer-state))
                                                  (if sensor-interrupt-p
                                                      (progn
                                                                                              
                                                        (queue-runtime-expr inner-interface
                                                                            (set -action-interrupt 1)))))
                                              (queue-runtime-expr learning-interface
                                                                  (set -action-interrupt 1))
                                            
                                              (if stop-when-finish-p
                                                  (progn
                                                      (action-stop)
                                                      ))

                                              (unless +sensor-complete-p)
                                              (if sensor-interrupt-p
                                                  (transform-focus-to-harm))
                                              (if save-world-when-finished-p
                                                  (progn
                                                    (setq save-time (save-world))
                                                    (log-info "saved" (concat "Saved world " robot-name " after " (string (integer save-time)) " ms"))
                                                    ))
                                              (group-reset memory-cache-group)
                                              (evaluate-foreground (lambda (result error) (if error (log-error "speech error" error))) (tts "Finished"))
                                              )
                                (if +action-complete-p 
                                    (set _ROBOT_SUCCESS "Success")
                                    (if quit
                                        (set _ROBOT_SUCCESS "User interrupt")
                                        (set _ROBOT_SUCCESS "harm avoidance"))))))

        (defun run-autonomous-harm-adaption (action-pref-spec-map initial-action stop-when-finish-p robot-name state-type save-world-when-finished-p create-new-p logging-enabled-p max-steps max-pattern-steps)
            
            (setq interface (queue-thread (quit sensor-interrupt-p +action-complete-p +sensor-complete-p +current-base-action fear-interrupt-p -work-queue user-action-interrupt-p)
                                          (list F F F F F F (get-runtime-interface) F)
                                          (progn
                                            
                                              (setq use-avoidance-actions-p 1)
                                              (include-basic-action-and-sensor-functions -work-queue)
                                              (initialization robot-name)
                                              (robot-group-setup robot-name state-type (vector-type-get-width state-type))
                                              (reload-robot create-new-p)
                                              (setq ui-logging-enabled-p logging-enabled-p)
                                              (set-runtime-poolsize 10)
                                              (setq step-count 0)
                                              (setq pattern-step-count 0)
                                              
                                              (setq wait-for-learning-p 1)
                                              (setq +learning-complete-p F)

                                              (setq log-tag "EXECUTION")
                                              ; Stores the last N seconds of sensorimotor patterns that can be associated with a harm signal
                                              (setq state-temporal-queue ())

                                              ; Stores feature models that matched the current sensorimotor pattern for the last N seconds.
                                              ; Technically, this contains a mapping between the name of a feature-model that was matching the
                                              ; current sensorimotor pattern for the last N seconds and a lembda function which is the operation
                                              ; to perform upon feature match expiration 
                                              (setq harm-temporal-map (make-string-hashtable))

                                              ; Contains scheduled interruptions of the currently executing motor pattern
                                              (setq scheduled-work-timeline
                                                    ())
                                              ; each item in a command-queue is either (state-map state-list feature-meta-data-map generic-command-spec)
                                              (setq learning-command-queue
                                                    (concurrency-make-queue))

                                              ; this is the group that does the actual prediction of harms                                              
                                              (setq harm-prediction-group
                                                    fear-level-group)

                                              ; Queue of harm-prediction-group after having learned the last sensorimotor pattern
                                              (setq next-harm-prediction-group-queue
                                                    (concurrency-make-queue))

                                              (concurrency-enqueue next-harm-prediction-group-queue fear-level-group)
                                              (setq next-action-name initial-action)
                                              (defun complete-state (state-map)
                                                  (if (not state-map)
                                                      (setq state-map (make-string-hashtable)))

                                                  (defhash state-map "actions" (or +current-base-action _ACTION_KEY_STOP))
                                                  (defhash state-map "situation" (list next-action-name))
                                                  (defhash state-map "reward" 0)
                                                  state-map)

                                              (defun reset-weighted-preferences ()
                                                  (make-string-hashtable (mapcar action-name
                                                                                 (get-hash-keys action-pref-spec-map)
                                                                                 (list action-name 
                                                                                       (first (gethash action-pref-spec-map action-name))))))


                                              (defun enqueue-next-state (state)
                                                  (concurrency-enqueue learning-command-queue (list state F F F)))

                                              (defun enqueue-next-metadata (group)
                                                  (concurrency-enqueue learning-command-queue (list F F (get-feature-name-map group) F))
                                                  )

                                              (defun enqueue-next-sensimotor-pattern (pattern)
                                                  (concurrency-enqueue learning-command-queue (list F (mapcar x state-temporal-queue (second x)) F F)))

                                              (defun enqueue-general-command (command-spec)
                                                  (concurrency-enqueue learning-command-queue (list F F F command-spec))
                                                )

                                              (defun adapt-feature-outcome-delay-ms (feature delay-ms limit)
                                                  (setq max-value 4)
                                                  (if (empty (get-feature-outcome-delay-history feature))
                                                      (progn
                                                         (append-feature-outcome-delay-ms feature delay-ms limit)
                                                         (return)))

                                                  (if (setq prob (get-feature-event-probability feature))
                                                      (for i
                                                           (setq multiplier (max 1 (abs (* max-value 2 (- prob 0.5)))))
                                                           F
                                                           (append-feature-outcome-delay-ms feature delay-ms limit))))


                                              (defun dequeue-oldest-command-spec ()
                                                  (concurrency-dequeue learning-command-queue))


                                              (defun rest-duration (interval-ms boundary-on-start-p boundary-on-end-p)
                                                      (setq stop-time
                                                            (+ (time) interval-ms))
                                                      (if boundary-on-start-p
                                                          (queue-runtime-expr -work-queue (group-reset harm-prediction-group)))
                                                      (stop-with-notify)
                                                      
                                                      (unless (> (time) stop-time))
                                                      
                                                      (if boundary-on-start-p
                                                          (queue-runtime-expr -work-queue (group-reset harm-prediction-group))))

                                              (defun get-feature-name-map (group)
                                                  (make-string-hashtable (mapcar feature-model
                                                                                 (group-get-ordered-processed-features group)
                                                                                 (list (get-feature-name feature-model)
                                                                                       feature-model))))

                                              (defun process-remaining-harm-features-upon-finish (sensor-interrupt-p user-action-interrupt-p)
                                                  ; okay
                                                  (setq feature-map
                                                        (get-feature-name-map harm-prediction-group))
                                                  (log-robot log-tag "Processing remaining features: " feature-map)
                                                  (log-info log-tag (concat "Processing remaining features: " (string sensor-interrupt-p) (string user-action-interrupt-p)))
                                                  (log-info log-tag (concat "Harm temporal model :") (get-hash-keys harm-temporal-map))
                                                  (for feature-key
                                                       (setq x (get-hash-keys feature-map))
                                                       F
                                                       (if (setq prior-data (gethash harm-temporal-map feature-key))
                                                           (progn
                                                                (multiple-bind (create-time refresh-time expire-lambda)
                                                                               prior-data)
                                                                (if (or sensor-interrupt-p user-action-interrupt-p)
                                                                    (adapt-feature-outcome-delay-ms (gethash feature-map feature-key)
                                                                                                     (- (time) create-time))
                                                                    (adapt-feature-outcome-delay-ms (gethash feature-map feature-key) F))

                                                                (log-info log-tag (concat "Adaptation: " feature-key ": " (string (get-feature-outcome-delay-history (gethash feature-map feature-key)))  ))
                                                                ))))


                                              (defun get-feature-expiration-lambda (feature-model)
                                                  (lambda ()
                                                      (setq name (get-feature-name feature-model))
                                                      (set scheduled-work-timeline (remove-all-timeline-items scheduled-work-timeline name))
                                                      (adapt-feature-outcome-delay-ms feature-model F)))

                                              (defun get-harm-avoidance-lambda (feature-model)
                                                  (lambda ()
                                                       (log-robot log-tag "Robot avoidance")
                                                       (set fear-interrupt-p 1)))

                                              (defun get-user-interrupt-action ()
                                                  (if (gethash action-pref-spec-map user-action-interrupt-p)
                                                      user-action-interrupt-p))

                                              (defun get-harm-avoidance-action (state)
                                                  (setq avoidance
                                                        (testSensorInterrupt state))

                                                  (cond
                                                      ((equals avoidance "collision-back") "forward-short")
                                                      ((equals avoidance "collision-left") "backward-medium")
                                                      ((equals avoidance "collision-right") "backward-medium")
                                                      ((equals avoidance "sonar") (vectorToValue-distribution (list ("backward-medium" 20) ("right-90" 30) ("left-90" 30) ("left-180" 20))))
                                                    )

                                                )

                                              (setq weighted-pref-map
                                                    (reset-weighted-preferences))

                                              (setq learning-interface
                                                      (queue-thread (-action-interrupt)
                                                                    (list F )
                                                                    (progn
                                                                        (unless quit
                                                                                (if (multiple-bind (next-state-map next-state-map-list new-feature-data-map command-spec)
                                                                                                   (dequeue-oldest-command-spec))
                                                                                    (if next-state-map
                                                                                        (process-next-state next-state-map F fear-level-group)
                                                                                        (if next-state-map-list
                                                                                            (progn
                                                                                              (log-robot log-tag "Starting batch processing")
                                                                                              (group-set-mode fear-level-group "LEARNING")
                                                                                              (process-batch-input next-state-map-list fear-level-group 1 1)
                                                                                              (group-set-mode fear-level-group "EXTRAPOLATION")
                                                                                              (concurrency-enqueue next-harm-prediction-group-queue
                                                                                                                   (copy-group fear-level-group))
                                                                                              (play-curiosity-1-ringtone)
                                                                                              (log-robot log-tag "Finished batch processing")
                                                                                              )
                                                                                            (if new-feature-data-map
                                                                                                (progn
                                                                                                  (log-robot log-tag "New feature data-map")
                                                                                                  (setq feature-map
                                                                                                        (get-feature-name-map fear-level-group))

                                                                                                  (for feature-name
                                                                                                       (get-hash-keys new-feature-data-map)
                                                                                                       F
                                                                                                       (if (setq my-feature
                                                                                                                 (gethash feature-map feature-name))
                                                                                                           (transfer-feature-metadata fear-level-group
                                                                                                                                      (gethash new-feature-data-map feature-name)
                                                                                                                                      my-feature))))
                                                                                                (if command-spec
                                                                                                    (cond
                                                                                                        ((equals command-spec "SAVE") (progn
                                                                                                                                        (setq save-time (save-world))
                                                                                                                                        (log-robot log-tag (concat "Saved world " robot-name " after " (string (integer save-time)) " ms"))
                                                                                                                                        ))
                                                                                                      )
                                                                                                  )
                                                                                                )
                                                                                            ))))
                                                                        
                                                                      )
                                                                    
                                                                    (progn (set +learning-complete-p 1)
                                                                           (log-info log-tag "Learning thread: Finished learning thread"))))

                                              ; Step (-1): Update state with initial sensory values
                                              (setq outer-state (add-sensors-to-state (complete-state)))
                                              (log-robot "Initial state" outer-state)
                                              (set action-sequence ())
                                              
                                              (enqueue-next-state outer-state)

                                              ; Learning thread will process first state

                                              (setq i 0)

                                              ; Main process loop

                                              (setq cont 1)
                                              (setq last-action-type F)
                                              (setq failure-temporal-scope-ms 4000)
                                              ; Last N seconds of sensorimotor pattern names
                                              (setq temporal-queue ())
                                              
                                              ; Step 0: Set next action name which is the first one to execute
                                              
                                              ; Action names to avoid because they are associated with a harm within the last
                                              ; N seconds
                                              (setq avoidance-actions ())

                                              (setq play-recognition-p 1)
                                              (unless (or quit 
                                                          (not cont) 
                                                          (and max-steps
                                                              (setq quit (>= step-count max-steps))))
                                                ; Reset variables
                                                
                                                (setq _ROBOT_SUCCESS F)
                                                (setq state-temporal-queue ())
                                                (setq harm-temporal-map (make-string-hashtable))

                                                (log-robot log-tag "Step (1): action: " next-action-name)
                                                ; Step 1: select the next action expr
                                                (setq action-expr
                                                      (second (gethash action-pref-spec-map next-action-name)))
                                                
                                                (log-robot log-tag "Step (2): action expr: " action-expr)
                                                ; [avoidance-actions] is reset here and may be updated during the course of executing an action-pattern by the success or failure of that pattern
                                                (setq avoidance-actions ())
                                                (multiple-bind (sensor-interrupt-p +action-complete-p +sensor-complete-p +current-base-action)
                                                               (list F F F F))

                                                ; Step (2): Execute current action expr.  Action express will continue until either the user quits the 
                                                ;           whole robot, or she quits the current action or the sensors interrupt the current action
                                                ;           Whether the interrupt was due to sensors or user or total robot shutdown doesn't matter
                                                ;           to this action thread
                                                (setq action-thread-interface
                                                      (queue-thread (-action-interrupt done state)
                                                                    (list F F outer-state)
                                                                    (progn
                                                                        (log-robot "action-thread" "Starting action: " (string action-expr))
                                                                        (unless (or quit done -action-interrupt)
                                                                            (eval action-expr)
                                                                            (setq done 1))
                                                                        (log-robot "action-thread" "Finished: [done: " (string done) " quit: " (string quit) " -action-interrupt: " (string -action-interrupt)))
                                                                    (if done 
                                                                        (set +action-complete-p 1)
                                                                        (println "Action interrupted"))))

                                              ; Step (3): Check if the current action should be interrupted due to sensors.  If so, then interrupt the
                                              ;           currently executing action

                                              (setq sensor-interrupt-p
                                                    (testSensorInterrupt outer-state))
                                              (if sensor-interrupt-p
                                                  (progn
                                                    (log-robot log-tag "Initial sensor interrupt")                                      
                                                    (queue-runtime-expr action-thread-interface
                                                                        (set -action-interrupt 1))))

                                              (setq update-time 0)

                                              (setq fear-interrupt-p F)

                                              (setq user-action-interrupt-p F)

                                              ; Step (4): Continue executing the main action until the entire robot is shutdown (quit == true)
                                              ;           or the current action completes (+action-complete-p == true) or the sensors detect a
                                              ;           harm-interrupt (sensor-interrupt-p == true) or the user externally interrupts the 
                                              ;           current action (user-action-interrupt-p == true)
                                              (unless (or sensor-interrupt-p quit +action-complete-p user-action-interrupt-p fear-interrupt-p )

                                                  (incr i)
                                                  ; Step (4.1) - Update the internal state visible to the action.
                                                  ; TODO: fix this, it is not synchronized across the threads
                                                  (queue-runtime-expr action-thread-interface
                                                                      (setq state (copy-map outer-state)))
                                                  ; Step (4.2) - Update current state with updates sensor data
                                                  (setq outer-state
                                                        (add-sensors-to-state (complete-state outer-state)))

                                                  ; Step 4.5
                                                  (setq sensor-interrupt-p
                                                        (testSensorInterrupt outer-state))


                                                  (setq state-temporal-queue 
                                                        (temporal-enqueue state-temporal-queue (copy-map outer-state)))
                                                  ;(log-robot log-tag "Step 3.1: updated state-temporal-queue " (string state-temporal-queue))
                                                  ; Step (4.2.1) - Update the harm prediction-group
                                                  ; If the learning-thread has finished processing a batch of inputs (i.e., there is something on next-harm-prediction-group-queue), 
                                                  ; then it will provide the next version of harm-prediction-group and we need to:
                                                  ; (1) - apply harm-prediction-group's feature meta-data to the learning thread
                                                  ; (2) - apply harm-prediction-group's feature meta-data to the newly provided group
                                                  (setq harm-prediction-group
                                                        (or (and (setq g (concurrency-dequeue next-harm-prediction-group-queue))
                                                                 (setq next-group g)
                                                                (while (setq g (concurrency-dequeue next-harm-prediction-group-queue))
                                                                       (setq next-group g))
                                                                (enqueue-next-metadata harm-prediction-group)
                                                                (log-robot log-tag "Step 3.1.1: New harm-prediction-group: " (string g))
                                                                (setq current-feature-map
                                                                      (get-feature-name-map harm-prediction-group))
                                                                (setq next-feature-map
                                                                      (get-feature-name-map next-group))
                                                                (for feature-name
                                                                     (get-hash-keys current-feature-map)
                                                                     next-group
                                                                     (if (setq prior-version-of-feature
                                                                               (gethash next-feature-map feature-name))
                                                                         (transfer-feature-metadata next-group
                                                                                                    (gethash current-feature-map feature-name)
                                                                                                    prior-version-of-feature))))
                                                            harm-prediction-group))


                                                  ; Step (4.4.1) - Process harm temporal map
                                                  (for temporal-item-spec
                                                       (update-temporal-map harm-temporal-map failure-temporal-scope-ms)
                                                       (if (empty (get-hash-keys harm-temporal-map))
                                                           (set play-recognition-p 1))
                                                       (progn
                                                          (multiple-bind (create-time refresh-time expiration-lambda)
                                                                         temporal-item-spec)
                                                          (funcall expiration-lambda)))

                                                  ;(log-robot log-tag "Step 3.2: Processed temporal model: " )

                                                  ; Step (4.4) - Process the next batch of work items that had been scheduled.  These will be fear
                                                  ;              interrupt lambdas.  Each one will perform a test of whether to interrupt the current
                                                  ;              action based on a prediction of a feature model
                                                  (multiple-bind (scheduled-work-timeline scheduled-work-items)
                                                                 (refresh-timeline scheduled-work-timeline))

                                                  (if (non-empty scheduled-work-items)
                                                      (for work-item-spec
                                                           scheduled-work-items
                                                           F
                                                           (progn
                                                              (multiple-bind (key execute-time work-lambda)
                                                                             work-item-spec)

                                                              (log-robot log-tag "work for " key " scheduled at: " (get-datetime-string execute-time))
                                                              (if (gethash harm-temporal-map key)
                                                                  (progn
                                                                      (funcall work-lambda)
                                                                      (log-robot log-tag (concat key " interrupted action " next-action-name)))
                                                                  (log-robot log-tag (concat key " did not interrupte action because item expired"))))))

                                                  
                                                  ; Step (4.3) - Process time

                                                  (if (> (time) update-time)
                                                      (progn
                                                        (log-robot log-tag "Temporal processing step")
                                                        (set update-time (+ UPDATE-INTERVAL-MS (time)))

                                                        ; Step (4.4.2) - Process current logical state
                                                        (process-next-state outer-state F harm-prediction-group)

                                                        ; Step (4.4.3) - Process all features recognizing this state
                                                        (setq avoidance-harm-delay-fraction 0.75)
                                                        (setq recognized-p F)
                                                        (for feature-model
                                                             (get-matching-features harm-prediction-group)
                                                             (if recognized-p
                                                                 (progn
                                                                    (log-robot log-tag "Potential harm recognition")
                                                                    (if play-recognition-p
                                                                        (play-positive-ringtone))
                                                                    (set play-recognition-p F)))
                                                             (progn
                                                                ; Make sure feature has a name if it doesn't already (shouldn't be necessary since name is
                                                                ; supposed to be assigned at the point the feature model is added in batch)
                                                                (if (not (setq feature-name (get-feature-name feature-model)))
                                                                    (set-feature-name feature-model
                                                                                      (setq feature-name (concat (group-get-name harm-prediction-group) "-" (unique-id)))))

                                                                ; If the feature model is newly added to temporal-map then add the feature name
                                                                (if (not (gethash harm-temporal-map feature-name))
                                                                    (progn
                                                                        (set recognized-p (or recognized-p 1))
                                                                        ; newly added feature
                                                                        (add-to-temporal-map harm-temporal-map
                                                                                             feature-name
                                                                                             (get-feature-expiration-lambda feature-model))
                                                                        (if (non-empty (setq expected-harm-delay-history
                                                                                             (get-feature-outcome-delay-history feature-model)))
                                                                            (progn
                                                                                (if (setq selected-delay
                                                                                          (random-select expected-harm-delay-history))
                                                                                    (add-to-action-timeline scheduled-work-timeline (list feature-name (+ (time) (* avoidance-harm-delay-fraction selected-delay)) (get-harm-avoidance-lambda feature-model))))

                                                                              )))
                                                                    (add-to-temporal-map harm-temporal-map
                                                                                         feature-name))))

                                                        (set action-sequence (append-item action-sequence (copy-map outer-state)))
                                                        ; Notify learning-thread of next data
                                                        (enqueue-next-state (copy-map outer-state))))

                                                  )
                                              ; Step 5 - Action has completed
                                              ; maybe clear temporal-work-items, (setq temporal-work-items ())
                                              (if (or sensor-interrupt-p user-action-interrupt-p)
                                                  (progn
                                                      (queue-runtime-expr action-thread-interface
                                                                          (set -action-interrupt 1))
                                                      (process-remaining-harm-features-upon-finish sensor-interrupt-p user-action-interrupt-p)
                                                      (if (non-empty state-temporal-queue)
                                                          (enqueue-next-sensimotor-pattern state-temporal-queue))
                                                    )
                                                  (if (not fear-interrupt-p)
                                                      (process-remaining-harm-features-upon-finish)))

                                        
                                              (if stop-when-finish-p
                                                  (progn
                                                      (action-stop)
                                                      ))


                                              ; Step (6) - Report on result of attempting to execute action
                                              
                                              (if save-world-when-finished-p
                                                  (enqueue-general-command "SAVE"))
                                              
                                              (evaluate-foreground (lambda (result error) (if error (log-error "speech error" error))) (tts "Finished: " next-action-name))
                                              ; Step (7) - If the action executed to completion than set variable
                                              (if +action-complete-p 
                                                  (set _ROBOT_SUCCESS "Success")
                                                  (if user-action-interrupt-p
                                                      (set _ROBOT_SUCCESS "User interrupt")
                                                      (if fear-interrupt-p
                                                          (set _ROBOT_SUCCESS "harm avoidance")
                                                          (if sensor-interrupt-p
                                                              (set _ROBOT_SUCCESS "recoil reflex")))))
                                                
                                                (setq message (concat "Finished: " _ROBOT_SUCCESS))
                                                (log-info log-tag message)

                                                ; TODO: - run-smart-action should be updating avoidance-actions

                                                (if (or (and (not (get-user-interrupt-action))
                                                             user-action-interrupt-p) 
                                                        sensor-interrupt-p)
                                                    (set avoidance-actions
                                                         (append-item avoidance-actions next-action-name)))

                                                (for avoid-action
                                                     avoidance-actions
                                                     F
                                                     (set temporal-queue
                                                          (temporal-enqueue temporal-queue avoid-action)))

                                                (if (non-empty avoidance-actions)
                                                    (log-info "Avoidance" (concat "Items: " 
                                                                                  (string avoidance-actions) 
                                                                                  " over time: "
                                                                                  (string failure-temporal-scope-ms))))

                                                (if (non-empty temporal-queue)
                                                    (log-info "Avoidance" (concat "Total avoidance: " 
                                                                                  (string temporal-queue) 
                                                                                  " over time: "
                                                                                  (string failure-temporal-scope-ms))))

                                                (if (get-user-interrupt-action)
                                                    (evaluate-foreground (lambda (result error) (if error (log-error "speech error" error))) (tts (concat "User has selected: " user-action-interrupt-p))))

                                                (if (and sensor-interrupt-p use-avoidance-actions-p)
                                                    (progn
                                                        (setq next-action-name (get-harm-avoidance-action outer-state))
                                                        (log-robot log-tag "Selected avoidance action: " next-action-name)
                                                        (unless (not (setq harm (testSensorInterrupt (setq outer-state (add-sensors-to-state (complete-state))))))
                                                            (cond
                                                                ((equals harm "collision-back") (forward-duration 1000))
                                                                ((equals harm "collision-left") (backward-duration 1000))
                                                                ((equals harm "collision-right") (backward-duration 1000))
                                                                ((equals harm "sonar") (backward-duration 1000)))
                                                          )


                                                        )
                                                    (setq next-action-name
                                                          (or (get-user-interrupt-action)
                                                              (vectorToValue-distribution (map-difference weighted-pref-map 
                                                                                                          (setq temporal-queue 
                                                                                                                (temporal-queue-update temporal-queue
                                                                                                                                       failure-temporal-scope-ms)))))))
                                                
                                                (incr step-count)
                                                (incr pattern-step-count)
                                                )

                                              (log-info "DONE" "")
                                              ; End main loop
                                              (if stop-when-finish-p
                                                  (progn
                                                      (action-stop)
                                                      ))

                                              ; Waiit for learning thread to finish
                                              (if wait-for-learning-p
                                                  (progn
                                                      (log-info log-tag (concat "Waiting for learning thread to finish: " (string (concurrency-queue-size learning-command-queue))))
                                                      (unless +learning-complete-p)
                                                      (log-info log-tag (concat "Finished waiting for learning thread: " (string (concurrency-queue-size learning-command-queue))))
                                                      ))

                                              (log-info "DONE" "Done waiting for learning queue to finish")
                                              (setq new-feature-data-map
                                                    (get-feature-name-map harm-prediction-group))

                                              (setq feature-map
                                                    (get-feature-name-map fear-level-group))

                                              (if (non-empty (get-hash-keys new-feature-data-map))
                                                       (log-info log-tag "Finished transferring meta-data to fear-level-group")
                                                    )
                                              (for feature-name
                                                   (setq x (get-hash-keys new-feature-data-map))
                                                   (if (non-empty x)
                                                       (log-robot log-tag "Finished transferring meta-data to fear-level-group")
                                                    )
                                                   (if (setq my-feature
                                                             (gethash feature-map feature-name))
                                                       (transfer-feature-metadata fear-level-group
                                                                                  (gethash new-feature-data-map feature-name)
                                                                                  my-feature)))      

                                              (if save-world-when-finished-p
                                                  (progn
                                                    (setq save-time (save-world))
                                                    (log-info "saved" (concat "Saved world " robot-name " after " (string (integer save-time)) " ms"))
                                                    ))


                                              )
                                (log-info "done" "Done autonomy"))))

      (comment
           (run-physical-robot '(progn (left-duration LEFT-90-DURATION) ) 1)


           (comment 
            ; Test ringtones
            (setq learning-notification-ringtone-id (load-global-var learning-notification-ringtone-id))

            (evaluate-foreground  (lambda (result error)
                                            (if error
                                               (log-error "ringtone"
                                                         error))
                                            (set playing-ringtone-p F))
                (show-ringtone-select-dialog (lambda (ringtone-id) (set learning-notification-ringtone-id ringtone-id)
                                                                   (save-global-var learning-notification-ringtone-id))))
            
            (play-curiosity-1-ringtone)

            (play-curiosity-2-ringtone)

            (play-curiosity-3-ringtone)

            (play-angry-ringtone)

            (play-confused-ringtone)

            (play-positive-ringtone)
            )


            ; <><><><><><><><><><><><><> Full Robot Tests <><><><><><><><><><><><><><><><>

            ; Step (1) - Create the set of higher order actions
            (setq action-map
                  (make-string-hashtable (list (list "forward-short" (list 10 '(forward-duration SHORT-DURATION)))
                                               (list "forward-medium" (list 10 '(forward-duration MEDIUM-DURATION)))
                                               (list "forward-long" (list 10 '(forward-duration LONG-DURATION)))
                                               (list "forward-forever" (list 10 '(forward-duration FOREVER-DURATION)))

                                               (list "backward-short" (list 3 '(backward-duration SHORT-DURATION)))
                                               (list "backward-medium" (list 1 '(backward-duration MEDIUM-DURATION)))
                                               (list "backward-long" (list 1 '(backward-duration LONG-DURATION)))
                                               (list "backward-forever" (list 1 '(backward-duration FOREVER-DURATION)))

                                               (list "left-45" (list 4 '(left-duration (/ LEFT-90-DURATION 2))))
                                               (list "left-90" (list 4 '(left-duration LEFT-90-DURATION)))
                                               (list "left-180" (list 4 '(left-duration (* 2 LEFT-90-DURATION))))
                                               (list "left-360" (list 0 '(left-duration (* 4 LEFT-90-DURATION))))
                                               (list "left-forever" (list 0 '(left-duration 10000)))

                                               (list "right-45" (list 4 '(right-duration (/ RIGHT-90-DURATION 2))))
                                               (list "right-90" (list 4 '(right-duration RIGHT-90-DURATION)))
                                               (list "right-180" (list 4 '(right-duration (* 2 RIGHT-90-DURATION))))
                                               (list "right-360" (list 0 '(right-duration (* 4 RIGHT-90-DURATION))))
                                               (list "right-forever" (list 0 '(right-duration 10000)))

                                               (list "rest-brief" (list 10 '(rest-duration 3000 1)))
                                               (list "rest-short" (list 10 '(rest-duration 5000 1)))
                                               (list "rest-medium" (list 10 '(rest-duration 8000 1)))
                                               (list "rest-long" (list 10 '(rest-duration 16000 1)))


                                               (list "around-left-short" (list 6 '(progn (left-duration LEFT-90-DURATION) (forward-duration SHORT-DURATION) (right-duration RIGHT-90-DURATION))))
                                               (list "around-left-medium" (list 6 '(progn (left-duration LEFT-90-DURATION) (forward-duration MEDIUM-DURATION) (right-duration RIGHT-90-DURATION))))

                                               (list "around-right-short" (list 6 '(progn (right-duration RIGHT-90-DURATION) (forward-duration SHORT-DURATION) (left-duration LEFT-90-DURATION))))
                                               (list "around-right-medium" (list 6 '(progn (right-duration RIGHT-90-DURATION) (forward-duration MEDIUM-DURATION) (left-duration LEFT-90-DURATION)))))))

            ; Test learning during basic sensorimotor pattern
            (setq top-state-type
                  (create-type (list "vector-type-create-struct" 
                                     (make-string-hashtable (list (list "actions" (list "vector-type-create-enum" (list _ACTION_KEY_STOP _ACTION_KEY_LEFT _ACTION_KEY_RIGHT _ACTION_KEY_BACKWARD _ACTION_KEY_FORWARD)))
                                                                  (list "collisions" (list "vector-type-create-set" ("collision-left" "collision-right" "collision-back")))
                                                                  (list "sonar" (list "vector-type-create-scaled-number" 0 255 5))
                                                                  (list "situation" (list "vector-type-create-set" (get-hash-keys action-map)))
                                                                  (list "reward" (list "vector-type-create-scaled-number" -10 10 3)))))))


            ; Create a robot with old mind.  Don't save
            (setq save-after-finish F)
            (setq create-new F)
            (setq run-count 2)
            (setq robot-runtime 
                  (run-autonomous-harm-adaption action-map
                                                "forward-long"
                                                1
                                                "TEST-harm-adaptation-bot"
                                                top-state-type
                                                save-after-finish
                                                create-new
                                                F
                                                run-count))

            

            ; Gently Kill robot process
            (queue-runtime-expr robot-runtime (setq quit 1))

            ; Increase step count

            (queue-runtime-expr robot-runtime (setq max-steps 100))

            ; Verify stop has occurred

            (get-process-count)

            ; Forcefully attempt halt of robot
            (progn
                (queue-runtime-expr robot-runtime (setq quit 1))
                (queue-runtime-expr robot-runtime (break-runtime learning-interface))
                (queue-runtime-expr robot-runtime (break-runtime action-thread-interface))
                (break-runtime robot-runtime)
                (action-stop))

            ; interrupt current action
            (queue-runtime-expr robot-runtime (setq user-action-interrupt-p 1))

            ; interrupt current action with a new one moving forward
            (queue-runtime-expr robot-runtime (setq user-action-interrupt-p "forward-medium"))

            ; interrupt current action with a new one moving backward
            (queue-runtime-expr robot-runtime (setq user-action-interrupt-p "backward-medium"))


            ; Uses new robot mind
            (setq robot-runtime 
                  (run-autonomous-harm-adaption action-map
                                                "forward-long"
                                                1
                                                "TEST-harm-adaptation-bot"
                                                top-state-type
                                                1
                                                1))
            
            ; WORKING

            (multiple-bind (memory-cache-group fear-level-group)
                  (let ()

                      (setq name "TEST-harm-adaptation-bot")
                      (setq state-type
                            (create-type (list "vector-type-create-struct" 
                                               (make-string-hashtable (list (list "actions" (list "vector-type-create-enum" (list _ACTION_KEY_STOP _ACTION_KEY_LEFT _ACTION_KEY_RIGHT _ACTION_KEY_BACKWARD _ACTION_KEY_FORWARD)))
                                                                            (list "collisions" (list "vector-type-create-set" ("collision-left" "collision-right" "collision-back")))
                                                                            (list "sonar" (list "vector-type-create-scaled-number" 0 255 5))
                                                                            (list "situation" (list "vector-type-create-set" (get-hash-keys action-map)))
                                                                            (list "reward" (list "vector-type-create-scaled-number" -10 10 3)))))))
                      (initialization name)
                      (robot-group-setup name state-type)
                      (reload-robot 1)

                      (group-reset memory-cache-group)

                      (setq viewer
                            (lambda (logical-state)
                                (join (mapcar key 
                                              (get-hash-keys logical-state)
                                              (if (equals key "collisions")
                                                  (concat key ":" (string (get-hash-keys (gethash logical-state "collisions"))))
                                                  (if (equals key "situation")
                                                      (concat key ":" (string (get-hash-keys (gethash logical-state "situation"))))
                                                      (concat key ":" (string (gethash logical-state key))))))
                                      " ")))


                      (setq prev F)
                      (log-robot "testing" "Testing patterns")
                      (setq start-time (time))
                      (list memory-cache-group fear-level-group)))

            (group-get-name memory-cache-group)

            (show-robot-world "TEST-harm-adaptation-bot" FEAR_LEVEL_ROBOT_GROUP_NAME top-state-type)

            

            (progn
              

              (setq feature
                      (group-get-focus-feature memory-cache-group))

              (mapcar (svec i)
                      (setq state-vectors
                            (feature-extrapolate-values feature))
                      (concat (string i) ") " (get-state-map-string (vector-type-vector-to-value top-state-type svec))))

              (setq copied (feature-copy short-term-feature))
              (group-import-feature fear-level-group copied 1)
              )
            



            (setq m-features (group-get-ordered-processed-features fear-level-group))

            (setq out (first features))

            (feature-set-custom-meta-data out "Hellow World!!!")

            (setq copied (feature-copy out))

            (group-import-feature fear-level-group copied 1)

            (group-import-feature fear-level-group copied)

            (setq features (group-get-ordered-processed-features fear-level-group))

            (evaluate-foreground (lambda (result error) (if error (log-error "show dialog error" error))) (view-state-list action-sequence))

            (queue-runtime-expr robot-runtime (setq quit 1))

            (action-stop)

            (show-robot-world "TEST-run-av-1-action" MEMCACHE_GROUP_NAME top-state-type)
            (show-robot-world "TEST-run-av-1-action" FEAR_LEVEL_ROBOT_GROUP_NAME top-state-type)

            ; Test data reloading 

            

            ; End top
            )


      

      ; end step 4
      )
       
          


      



  )


)



  


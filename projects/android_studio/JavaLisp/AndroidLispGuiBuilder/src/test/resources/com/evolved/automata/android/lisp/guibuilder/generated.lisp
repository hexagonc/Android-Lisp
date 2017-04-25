"/Applications/Android Studio.app/Contents/jre/jdk/Contents/Home/bin/java" -Didea.launcher.port=7536 "-Didea.launcher.bin.path=/Applications/Android Studio.app/Contents/bin" -Didea.junit.sm_runner -Dfile.encoding=UTF-8 -classpath "/Applications/Android Studio.app/Contents/lib/idea_rt.jar:/Applications/Android Studio.app/Contents/plugins/junit/lib/junit-rt.jar:/Users/Evolved8/development/android-sdk-mac_x86/platforms/android-19/data/res:/Users/Evolved8/ALGB2/Android-Lisp/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/build/intermediates/classes/test/debug:/Users/Evolved8/ALGB2/Android-Lisp/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/build/intermediates/classes/debug:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/com.fasterxml.jackson.core/jackson-core/2.6.0/ba4506602d0f51d214ba16b80e2e70a714152a20/jackson-core-2.6.0.jar:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/de.greenrobot/eventbus/2.4.0/ddd166d01b3158d1c00576d29f7ed15c030df719/eventbus-2.4.0.jar:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/com.fasterxml.jackson.core/jackson-databind/2.6.0/d2e494b027c26edc4d4e7e9251e168e6af26d2d3/jackson-databind-2.6.0.jar:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/com.fasterxml.jackson.core/jackson-annotations/2.6.0/a0990e2e812ac6639b6ce955c91b13228500476e/jackson-annotations-2.6.0.jar:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/org.hamcrest/hamcrest-core/1.3/42a25dc3219429f0e5d060061f71acb49bf010a0/hamcrest-core-1.3.jar:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/junit/junit/4.12/2973d150c0dc1fefe998f834810d68f278ea58ec/junit-4.12.jar:/Users/Evolved8/ALGB2/Android-Lisp/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/libs/dropbox-core-sdk-0-SNAPSHOT.jar:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/org.mockito/mockito-core/1.10.19/e8546f5bef4e061d8dd73895b4e8f40e3fe6effe/mockito-core-1.10.19.jar:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/org.objenesis/objenesis/2.1/87c0ea803b69252868d09308b4618f766f135a96/objenesis-2.1.jar:/Users/Evolved8/ALGB2/Android-Lisp/projects/android_studio/JavaLisp/AndroidTools/build/intermediates/classes/debug:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/com.google.android/support-v4/r7/24d0f6da34c3a2bfcf736ab42d51c91ac821ee22/support-v4-r7.jar:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/com.nostra13.universalimageloader/universal-image-loader/1.9.1/296dfb44fbf31397c0306e60cb62e06ca40df07f/universal-image-loader-1.9.1.jar:/Users/Evolved8/ALGB2/Android-Lisp/projects/android_studio/JavaLisp/common-library/build/classes/test:/Users/Evolved8/ALGB2/Android-Lisp/projects/android_studio/JavaLisp/common-library/build/classes/main:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/org.apache.commons/commons-lang3/3.4/5fe28b9518e58819180a43a850fbc0dd24b7c050/commons-lang3-3.4.jar:/Users/Evolved8/.gradle/caches/modules-2/files-2.1/org.apache.commons/commons-math3/3.5/13af68e9a11576953f5c4b09436f8207be9d3a27/commons-math3-3.5.jar:/Users/Evolved8/ALGB2/Android-Lisp/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/build/intermediates/sourceFolderJavaResources/test/debug:/Users/Evolved8/ALGB2/Android-Lisp/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/build/intermediates/sourceFolderJavaResources/debug:/Users/Evolved8/ALGB2/Android-Lisp/projects/android_studio/JavaLisp/AndroidTools/build/intermediates/sourceFolderJavaResources/debug:/Users/Evolved8/ALGB2/Android-Lisp/projects/android_studio/JavaLisp/build/generated/mockable-android-19.jar" com.intellij.rt.execution.application.AppMain com.intellij.rt.execution.junit.JUnitStarter -ideVersion5 com.evolved.automata.android.lisp.guibuilder.NodeParsingTests,largeTest
Took 378 ms to process input.

java.lang.AssertionError: Failed to match parsed result to input.  Result is: (setq X-MARK-ID 
    1)

(setq O-MARK-ID
    2)

(setq EMPTY-MARK-ID
    0)


(multiple-bind (T M B)
        (0 1 2))

(multiple-bind (L C R)
        (0 1 2))

(setq mark-display-map
    (make-int-hashtable (list (list X-MARK-ID "X")
                  (list O-MARK-ID "O")
                  (list EMPTY-MARK-ID "_"))))


(setq X-WINNABLE-BOARD-SET
    (make-int-hashtable))

(setq O-WINNABLE-BOARD-SET
    (make-int-hashtable))

(setq X-PLAYER-WON-BOARD-SET
    (make-int-hashtable))

(setq O-PLAYER-WON-BOARD-SET
    (make-int-hashtable))

(setq DEFAULT-STATE-ACTION-PREF-MAP
    (make-int-hashtable))

(setq STATE-ACTION-SET-MAP
    (make-int-hashtable))

(setq BOARD-ID-TO-STATE-MAP 
    (make-int-hashtable))

(defun get-place-id (vert horz)
  (+ (* horz 3)
     vert))

(defun get-place-pair-id (place-id-pair)
  (get-place-id (first place-id-pair)
          (second place-id-pair)))

(defun get-place-board-id (place-id)
  (pow 3
     place-id))

(defun get-action-board-id (place-board-id mark-id)
  (* place-board-id
     mark-id))



(defun get-initial-boardstate ()
  (fill-list 9))


(defun vec-equals (v1 v2)
  (and (= (length v1)
      (length v2))
    (all (x i) v2 (= (nth v1 i) x))))


(defun get-action-id (user-place-id user-mark)
  (get-action-board-id (get-place-board-id user-place-id) user-mark))




(setq LINEAR_RUNS
    (list (list (get-place-pair-id (list T L))
            (get-place-pair-id (list T C))
            (get-place-pair-id (list T R)))
        (list (get-place-pair-id (list M L))
            (get-place-pair-id (list M C))
            (get-place-pair-id (list M R)))
        (list (get-place-pair-id (list B L))
            (get-place-pair-id (list B C))
            (get-place-pair-id (list B R)))

        (list (get-place-pair-id (list T L))
            (get-place-pair-id (list M L))
            (get-place-pair-id (list B L)))
        (list (get-place-pair-id (list T C))
            (get-place-pair-id (list M C))
            (get-place-pair-id (list B C)))
        (list (get-place-pair-id (list T R))
            (get-place-pair-id (list M R))
            (get-place-pair-id (list B R)))

        (list (get-place-pair-id (list T L))
            (get-place-pair-id (list M C))
            (get-place-pair-id (list B R)))

        (list (get-place-pair-id (list T R))
            (get-place-pair-id (list M C))
            (get-place-pair-id (list B L)))))

(setq LINEAR_RUN_MAP_LIST
    (list (make-int-hashtable (list (list (get-place-pair-id (list T L)) 1)
                      (list (get-place-pair-id (list T C)) 1) 
                        (list (get-place-pair-id (list T R)) 1)))

        (make-int-hashtable (list (list (get-place-pair-id (list M L)) 1)
                      (list (get-place-pair-id (list M C)) 1) 
                        (list (get-place-pair-id (list M R)) 1)))


        (make-int-hashtable (list (list (get-place-pair-id (list B L)) 1)
                      (list (get-place-pair-id (list B C)) 1) 
                        (list (get-place-pair-id (list B R)) 1)))


        (make-int-hashtable (list (list (get-place-pair-id (list T L)) 1)
                      (list (get-place-pair-id (list M L)) 1) 
                        (list (get-place-pair-id (list B L)) 1)))

        (make-int-hashtable (list (list (get-place-pair-id (list T C)) 1)
                      (list (get-place-pair-id (list M C)) 1) 
                        (list (get-place-pair-id (list B C)) 1)))

        (make-int-hashtable (list (list (get-place-pair-id (list T R)) 1)
                      (list (get-place-pair-id (list M R)) 1) 
                        (list (get-place-pair-id (list B R)) 1)))

        
        (make-int-hashtable (list (list (get-place-pair-id (list T L)) 1)
                      (list (get-place-pair-id (list M C)) 1) 
                        (list (get-place-pair-id (list B R)) 1)))

        (make-int-hashtable (list (list (get-place-pair-id (list T R)) 1)
                      (list (get-place-pair-id (list M C)) 1) 
                        (list (get-place-pair-id (list B L)) 1)))

        ))



(defun get-mark-id-from-board-place (board-state place-id)
  (setq place-value
      (nth board-state place-id))

  (setq base
      (integer (pow 3 place-id)))

  (/ place-value base))

(defun get-board-state-from-id (board-id)
  (if (setq state (gethash BOARD-ID-TO-STATE-MAP board-id))
    (return state))

  (setq out ())

  (setq id board-id)
  (for i
     9
     (defhash BOARD-ID-TO-STATE-MAP board-id out)
     (progn
      (setq mark-id 
          (mod id 3))

      (setq action-id
          (* mark-id
             (pow 3 i)))

      (set out
         (append-item out 
                (integer action-id)))
      (set id
         (/ (- id mark-id)
          3)))))

(defun get-board-state-id (board-state)
  (setq board-id
      (apply "+"
           board-state))
  (defhash BOARD-ID-TO-STATE-MAP board-id board-state)
  board-id)

; 99

(defun x-winnable-run-p (board-state linear-run)
  (not (some place-id
           linear-run
           (= O-MARK-ID
            (get-mark-id-from-board-place board-state place-id)))))


(defun o-winnable-run-p (board-state linear-run)
  (not (some place-id
           linear-run
           (= X-MARK-ID
            (get-mark-id-from-board-place board-state place-id)))))


(defun get-run-num-empty-places (board-state run)
  (length (find place-id run (= 0 (nth board-state place-id)))))


(defun x-winnable-board-p (board-state)
  (setq board-id
      (get-board-state-id board-state))

  (if (contains-hash-key X-WINNABLE-BOARD-SET board-id)
    (gethash X-WINNABLE-BOARD-SET board-id)
    (defhash X-WINNABLE-BOARD-SET
         board-id
         (some linear-run
             LINEAR_RUNS
             (x-winnable-run-p board-state linear-run)))))

(defun o-winnable-board-p (board-state)
  (setq board-id
      (get-board-state-id board-state))

  (if (contains-hash-key O-WINNABLE-BOARD-SET board-id)
    (gethash O-WINNABLE-BOARD-SET board-id)
    (defhash O-WINNABLE-BOARD-SET
         board-id
         (some linear-run
             LINEAR_RUNS
             (o-winnable-run-p board-state linear-run)))))



(defun draw-board-p (board-state)
  (and (not (o-winnable-board-p board-state))
     (not (x-winnable-board-p board-state))))


(defun o-player-has-won-p (board-state)
  (setq board-id
      (get-board-state-id board-state))

  (if (contains-hash-key O-PLAYER-WON-BOARD-SET board-id)
    (return (gethash O-PLAYER-WON-BOARD-SET board-id)))

  (defhash O-PLAYER-WON-BOARD-SET
       board-id
       (some linear-run
           LINEAR_RUNS
           (all place-id
              linear-run
              (= O-MARK-ID 
                 (get-mark-id-from-board-place board-state place-id))))))


(defun x-player-has-won-p (board-state)
  (setq board-id
      (get-board-state-id board-state))

  (if (contains-hash-key X-PLAYER-WON-BOARD-SET board-id)
    (return (gethash X-PLAYER-WON-BOARD-SET board-id)))

  (defhash X-PLAYER-WON-BOARD-SET
       board-id
       (some linear-run
           LINEAR_RUNS
           (all place-id
              linear-run
              (= X-MARK-ID 
                 (get-mark-id-from-board-place board-state place-id))))))


(defun finished-board-p (board-state)
  (or (draw-board-p board-state)
    (o-player-has-won-p board-state)
    (x-player-has-won-p board-state)))

(defun get-possible-state-actions (board-state)
  (setq board-id
      (get-board-state-id board-state))

  (setq action-list
      (gethash STATE-ACTION-SET-MAP board-id))

  (if action-list
    (return action-list))

  (setq action-map
      (make-int-hashtable))

  (for run
     LINEAR_RUNS
     (defhash STATE-ACTION-SET-MAP board-id (get-hash-keys action-map))
     (if (or (x-winnable-run-p board-state run)
         (o-winnable-run-p board-state run))
      (for place-id
         run
         F
         (if (= 0 (nth board-state place-id))
           (defhash action-map place-id 1))))))



(defun get-default-action-preference (board-id)
  
  (setq board-state 
      (get-board-state-from-id board-id))

  (setq action-scores
      (gethash DEFAULT-STATE-ACTION-PREF-MAP
             board-id))

  (if action-scores
    (return action-scores))

  (setq action-scores
      (fill-list 9))

  (for run
     LINEAR_RUNS
     (defhash DEFAULT-STATE-ACTION-PREF-MAP
            board-id
            action-scores)
     (progn
      
      (setq num-blanks
          (get-run-num-empty-places board-state run))

      (if (x-winnable-run-p board-state run)
        (for place-id
           run
           F
           (if (= 0 (nth board-state place-id))
             (set-nth action-scores
                  place-id
                  (+ (nth action-scores
                          place-id)
                     (- 4 num-blanks))))))

      (if (o-winnable-run-p board-state run)
        (for place-id
           run
           F
           (if (= 0 (nth board-state place-id))
             (set-nth action-scores
                  place-id
                  (+ (nth action-scores
                          place-id)
                     (- 4 num-blanks)))))))))





(setq _QVALUE_MAP_KEY "QVALUE-MAP")

(setq _POLICY_KEY "POLICY")
(setq _STATE_ACTION_PREF_KEY "STATE-ACTION-PREF-MAP")
(setq _VALUE_MAP_KEY "VALUE_MAP")
(setq _STATE_ACTION_ELIGILIBILTY_TRACE_KEY "ELIGIBILITY-TRACE")
(setq _ALPHA_KEY "ALPHA")
(setq _DISCOUNT_KEY "y")
(setq _LAMBDA_KEY "lambda")
(setq _STATES_VISITED_MAP_KEY "visited-states")


(setq _ACTOR_UPDATE_DELTA_KEY "actor-update-delta")

(setq _LAST_ACTION_PLACE_ID "last-action-place")
(setq _LAST_INITIAL_BOARD_ID "previous-initial-board")

(setq _X_KNOWLEDGE_MAP_KEY "x-knowledge")

(setq _O_KNOWLEDGE_MAP_KEY "o-knowledge")

(setq _TTT_BOARD_STATE_KEY "board-state")

(setq _PLAYER_MARK_KEY "mark")

(setq _X_PLAYER_FINISHED_KEY "x-player-finished-p")

(setq _O_PLAYER_FINISHED_KEY "o-player-finished-p")

(setq _STATE_VISIT_COUNT_KEY "visit-count")

(setq _MIN_EPSILON_KEY "e")
(setq _MAX_EPSILON_KEY "max epsilon")
(setq _MIN_EPSILON_DECAY_INTERVAL_KEY "epsilon count interval")


(setq DEFAULT_DISCOUNT 1)
(setq DEFAULT_UPDATE_DELTA 1)
(setq _DEFAULT_MIN_EPSILON 0.1)
(setq _DEFAULT_MAX_EPSILON 0.15)
(setq _MIN_EPSILON_DECAY_INTERVAL 1)
(setq _DEFAULT_LAMBDA 1)
(setq _MIN_ELIGIBILITY_TRACE 0.002)

(setq _USE_ELIGIBILITY_TRACE_P 1)
(setq _RESET_ELIGILITY_TRACE_P F)


(defun get-knowledge-eligibility-trace (knowledge-map board-id)
  (or (gethash (gethash knowledge-map _STATE_ACTION_ELIGILIBILTY_TRACE_KEY)
         board-id)
    0))

(defun set-knowledge-eligibility-trace (knowledge-map board-id value)
  (defhash (gethash knowledge-map _STATE_ACTION_ELIGILIBILTY_TRACE_KEY)
       board-id
       value)
  knowledge-map)


(defun multiply-knowledge-eligibility-trace (knowledge-map multiplier)
  (setq eligibility-trace
      (gethash knowledge-map _STATE_ACTION_ELIGILIBILTY_TRACE_KEY))

  (setq visited-state-map
      (gethash knowledge-map _STATES_VISITED_MAP_KEY))

  (for visited-state-id
     (get-hash-keys visited-state-map)
     knowledge-map
     (progn
      (setq prior-trace
          (get-knowledge-eligibility-trace knowledge-map visited-state-id))
      (setq new-trace
          (* prior-trace multiplier))

      (if (< new-trace _MIN_ELIGIBILITY_TRACE)
        (progn
          (remhash visited-state-map visited-state-id)
          (defhash eligibility-trace visited-state-id 0))
        (defhash eligibility-trace 
             visited-state-id
             new-trace)))))

(defun increment-knowledge-eligibility-trace (knowledge-map board-id increment)
  (setq eligibility-trace
      (gethash knowledge-map _STATE_ACTION_ELIGILIBILTY_TRACE_KEY))

  (defhash eligibility-trace
       board-id
       (+ (get-knowledge-eligibility-trace knowledge-map board-id)
        increment))
      
  (return knowledge-map))



(defun get-knowledge-count (knowledge-map board-id)
  (or (gethash (gethash knowledge-map _STATE_VISIT_COUNT_KEY)
         board-id)
    0))


(defun get-epsilon-action (action-score-list epsilon)
  
  (setq max-score-actions
      (make-int-hashtable))
  (setq max-score F)

  (setq allow-all-possible-p
      (< (random 0 1)
       epsilon))

  (for (score place-id)
     action-score-list
     (and (> (length max-score-actions) 0)
        (nth (setq maximal-actions
             (get-hash-keys max-score-actions))
             (integer (* (length maximal-actions)
                     (random 0 1)))))
     (if (not (= 0 (nth action-score-list place-id)))
       (if (or allow-all-possible-p
           (not max-score)
           (= score max-score))
         (progn
          (defhash max-score-actions place-id 1)
          (set max-score score))
         (if (> score
            max-score)
           (progn
            (set max-score-actions
               (make-int-hashtable (list (list place-id 1))))
            (set max-score
               score)))))))

(defun get-action-pref-list (board-id state-action-pref-map)

  (or (gethash state-action-pref-map board-id)
    (defhash state-action-pref-map
         board-id
         (get-default-action-preference board-id))))


(defun get-knowledge-alpha (knowledge-map board-id)
  (setq count
      (get-knowledge-count knowledge-map board-id))
  (/ 1 (+ 1 count)))

(defun get-knowledge-epsilon (knowledge-map board-id)
  (setq count
      (get-knowledge-count knowledge-map board-id))

  (setq interval (gethash knowledge-map _MIN_EPSILON_DECAY_INTERVAL_KEY))

  (setq min-epsilon (gethash knowledge-map _MIN_EPSILON_KEY))

  (setq max-epsilon
      (gethash knowledge-map _MAX_EPSILON_KEY))

  (setq R 
      (/ (* -1 interval)
       (Ln min-epsilon)))

  (max min-epsilon 
     (min max-epsilon
        (exp (/ (* -1 count) 
              R)))))

(defun incr-knowledge-count (knowledge-map board-id)

  (setq visit-count-map
      (gethash knowledge-map
             _STATE_VISIT_COUNT_KEY))

  (defhash visit-count-map
       board-id
       (+ 1 (get-knowledge-count knowledge-map board-id))))




(defun update-action-pref-list-with-eligibiity-trace (knowledge-map next-board-state-id reward )
  (multiple-bind (state-action-pref-map state-value-map prior-board-state-id prior-place-id discount update-delta)
           (list (gethash knowledge-map _STATE_ACTION_PREF_KEY)
               (gethash knowledge-map _VALUE_MAP_KEY)
               (gethash knowledge-map _LAST_INITIAL_BOARD_ID)
               (gethash knowledge-map _LAST_ACTION_PLACE_ID)
               (gethash knowledge-map _DISCOUNT_KEY)
               (gethash knowledge-map _ACTOR_UPDATE_DELTA_KEY)))

  (if (not prior-place-id)
    (return F))

  (setq alpha
      (get-knowledge-alpha knowledge-map next-board-state-id))

  (setq visit-count
      (get-knowledge-count knowledge-map next-board-state-id))

  (setq prior-action-list
      (get-action-pref-list prior-board-state-id state-action-pref-map))

  (setq prior-state-value-existed-p
      (gethash state-value-map prior-board-state-id))


  (setq prior-state-value
      (or prior-state-value-existed-p
          (defhash state-value-map
                 prior-board-state-id
                 0)))

  (setq next-state-value-exists-p
      (gethash state-value-map next-board-state-id))

  (setq next-state-value
      (or next-state-value-exists-p
          (defhash state-value-map
                 next-board-state-id
                 0)))

  (setq error
      (- (+ reward
          next-state-value)
         prior-state-value))


  (if _RESET_ELIGILITY_TRACE_P
    (set-knowledge-eligibility-trace knowledge-map next-board-state-id 1)
    (increment-knowledge-eligibility-trace knowledge-map next-board-state-id 1))  
  
  ; update Value functions
  (setq visited-state-map
      (gethash knowledge-map _STATES_VISITED_MAP_KEY))

  (defhash visited-state-map next-board-state-id 1)

  (setq eligibility-trace 
      (get-knowledge-eligibility-trace knowledge-map next-board-state-id))

  (for visited-state-id
     (get-hash-keys visited-state-map)
     knowledge-map
     (progn
      (setq prior-state-value
          (or (gethash state-value-map
                   visited-state-id)
              (defhash state-value-map visited-state-id 0)))

      (setq prior-trace (get-knowledge-eligibility-trace knowledge-map visited-state-id))
      (defhash state-value-map
           visited-state-id
           (+ prior-state-value
            (* error
                 alpha
                 prior-trace)))


      (if (< prior-trace _MIN_ELIGIBILITY_TRACE)
        (progn
          (remhash visited-state-map visited-state-id)
          (set-knowledge-eligibility-trace knowledge-map visited-state-id 0)))))

  (multiply-knowledge-eligibility-trace knowledge-map
                      (* discount
                       (gethash knowledge-map _LAMBDA_KEY)))



  (println "Visit count: " visit-count ".  Transistion: (" prior-board-state-id " -> " next-board-state-id ") with alpha: " alpha " and which "
       (if (and next-state-value-exists-p prior-state-value-existed-p) 
         " has been seen before"
         " has not been seen before.")
       (if next-state-value-exists-p " Current state has been seen before. " " ")
       "Expectation error is: " error
       " eligibility trace " eligibility-trace)

  (if (not (= reward 0))
    (println "higher reward: " 
         reward  
         " and error: " 
         error 
         " old action pref is " 
         (nth prior-action-list prior-place-id) 
         " new pref is "
         (+ (nth prior-action-list prior-place-id)
          (* update-delta error)) )
    (if (and (not (= 0 prior-state-value))
         (not (= 0 error)))
      (println "knowledge propagation: " prior-state-value)))

  (set-nth prior-action-list
       prior-place-id
       (+ (nth prior-action-list prior-place-id)
        (* update-delta error)))

  (defhash state-action-pref-map
       prior-board-state-id
       prior-action-list))

(defun update-action-pref-list (knowledge-map next-board-state-id reward )
  
  (multiple-bind (state-action-pref-map state-value-map prior-board-state-id prior-place-id discount update-delta)
           (list (gethash knowledge-map _STATE_ACTION_PREF_KEY)
               (gethash knowledge-map _VALUE_MAP_KEY)
               (gethash knowledge-map _LAST_INITIAL_BOARD_ID)
               (gethash knowledge-map _LAST_ACTION_PLACE_ID)
               (gethash knowledge-map _DISCOUNT_KEY)
               (gethash knowledge-map _ACTOR_UPDATE_DELTA_KEY)))

  (if (not prior-place-id)
    (return F))

  (setq alpha
      (get-knowledge-alpha knowledge-map next-board-state-id))

  (setq visit-count
      (get-knowledge-count knowledge-map next-board-state-id))

  (setq prior-action-list
      (get-action-pref-list prior-board-state-id state-action-pref-map))



  (setq prior-state-value-existed-p
      (gethash state-value-map prior-board-state-id))


  (setq prior-state-value
      (or prior-state-value-existed-p
          (defhash state-value-map
                 prior-board-state-id
                 0)))

  (setq next-state-value-exists-p
      (gethash state-value-map next-board-state-id))

  (setq next-state-value
      (or next-state-value-exists-p
          (defhash state-value-map
                 next-board-state-id
                 0)))

  (setq error
      (- (+ reward
          next-state-value)
         prior-state-value))


  (println "Visit count: " visit-count ".  Transistion: (" prior-board-state-id " -> " next-board-state-id ") with alpha: " alpha " and which "
       (if (and next-state-value-exists-p prior-state-value-existed-p) 
         " has been seen before"
         " has not been seen before.")
       (if next-state-value-exists-p " Current state has been seen before. " " ")
       "Expectation error is: " error)

  (if (not (= reward 0))
    (println "higher reward: " 
         reward  
         " and error: " 
         error 
         " old action pref is " 
         (nth prior-action-list prior-place-id) 
         " new pref is "
         (+ (nth prior-action-list prior-place-id)
          (* update-delta error)) )
    (if (and (not (= 0 prior-state-value))
         (not (= 0 error)))
      (println "knowledge propagation: " prior-state-value)))

  (set-nth prior-action-list
       prior-place-id
       (+ (nth prior-action-list prior-place-id)
        (* update-delta error)))

  (defhash state-action-pref-map
       prior-board-state-id
       prior-action-list)

  (defhash state-value-map
       prior-board-state-id
       (+ prior-state-value
        (* error
           alpha))))


(defun get-knowledge-epsilon-action (knowledge-map board-id)
  (get-epsilon-action (get-action-pref-list board-id 
                        (gethash knowledge-map _STATE_ACTION_PREF_KEY))
              (get-knowledge-epsilon knowledge-map board-id)))


(defun make-knowledge-map ()
  (multiple-bind (  discount update-delta state-value-map state-action-pref-map q-value-map last-place-id last-board-id)
           (list  DEFAULT_DISCOUNT DEFAULT_UPDATE_DELTA (make-int-hashtable) (make-int-hashtable) (make-int-hashtable) F F))


  (setq knowledge-map
      (make-string-hashtable (list (list _QVALUE_MAP_KEY q-value-map)
                     
                     (list _STATE_ACTION_ELIGILIBILTY_TRACE_KEY (make-int-hashtable))
                     (list _STATES_VISITED_MAP_KEY (make-int-hashtable))
                     (list _ACTOR_UPDATE_DELTA_KEY update-delta)
                     (list _DISCOUNT_KEY discount)
                     (list _STATE_VISIT_COUNT_KEY (make-int-hashtable))
                     (list _STATE_ACTION_PREF_KEY state-action-pref-map)
                     (list _VALUE_MAP_KEY state-value-map)
                     (list _LAMBDA_KEY _DEFAULT_LAMBDA)
                     (list _MIN_EPSILON_KEY _DEFAULT_MIN_EPSILON)
                     (list _MAX_EPSILON_KEY _DEFAULT_MAX_EPSILON)
                     (list _MIN_EPSILON_DECAY_INTERVAL_KEY _MIN_EPSILON_DECAY_INTERVAL)
                     (list _LAST_ACTION_PLACE_ID last-place-id)
                     (list _LAST_INITIAL_BOARD_ID last-board-id)))))


(defun reset-eligibility-trace (knowledge-map)
  (defhash knowledge-map _STATE_ACTION_ELIGILIBILTY_TRACE_KEY (make-int-hashtable))
  (defhash knowledge-map _STATES_VISITED_MAP_KEY (make-int-hashtable))
  knowledge-map)


(defun reset-knowledge-map (knowledge-map)
  (defhash knowledge-map _LAST_ACTION_PLACE_ID F)
  (defhash knowledge-map _LAST_INITIAL_BOARD_ID F)
  (reset-eligibility-trace knowledge-map)
  knowledge-map)


(defun set-knowledge-alpha (knowledge-map alpha)
  (defhash knowledge-map _ALPHA_KEY alpha)
  knowledge-map)

(defun set-knowledge-epsilon (knowledge-map epsilon)
  (defhash knowledge-map _MIN_EPSILON_KEY epsilon)
  knowledge-map)

(defun get-next-player-mark (mark-id)
  (if (= X-MARK-ID mark-id)
    O-MARK-ID
    X-MARK-ID))


(defun update-board-state (board-state place-id mark-id)
  (set-nth board-state
       place-id
       (get-action-board-id (get-place-board-id place-id)
                  mark-id)))

(defun make-board-ttt-board (initial-mark-id x-knowledge-map o-knowledge-map)

  (make-string-hashtable (list (list _X_KNOWLEDGE_MAP_KEY 
                                     (or (and x-knowledge-map (reset-knowledge-map x-knowledge-map))
                                         (make-knowledge-map)))
                                 (list _O_KNOWLEDGE_MAP_KEY 
                                     (or (and o-knowledge-map (reset-knowledge-map o-knowledge-map))
                                         (make-knowledge-map)))
                                 (list _TTT_BOARD_STATE_KEY (fill-list 9))
                                 (list _X_PLAYER_FINISHED_KEY F)
                                 (list _O_PLAYER_FINISHED_KEY F)
                                 (list _PLAYER_MARK_KEY initial-mark-id))))

(defun get-ttt-board-x-knowledge (ttt-board-map)
  (gethash ttt-board-map _X_KNOWLEDGE_MAP_KEY))

(defun get-ttt-board-o-knowledge (ttt-board-map)
  (gethash ttt-board-map _O_KNOWLEDGE_MAP_KEY))

(defun get-ttt-board-state (ttt-board-map)
  (gethash ttt-board-map _TTT_BOARD_STATE_KEY))

(defun is-game-finished-p (ttt-board-map)
  (and (gethash ttt-board-map _X_PLAYER_FINISHED_KEY)
     (gethash ttt-board-map _O_PLAYER_FINISHED_KEY)))


(defun execute-x-player-move (ttt-board user-action-id learn-p)

  (if (and (gethash ttt-board _X_PLAYER_FINISHED_KEY)
       (gethash ttt-board _O_PLAYER_FINISHED_KEY))
    (return F))

  (if (gethash ttt-board _X_PLAYER_FINISHED_KEY)
    (progn
      (defhash ttt-board _PLAYER_MARK_KEY O-MARK-ID)
      (return O-MARK-ID)))

  (setq x-knowledge-map
      (gethash ttt-board _X_KNOWLEDGE_MAP_KEY))

  (setq board-state
      (gethash ttt-board _TTT_BOARD_STATE_KEY))

  (setq board-id
      (get-board-state-id board-state))

  ; Set previous state reward


  (setq reward 0)
  (setq finished-p F)

  (cond 
    ((draw-board-p board-state)
      (setq reward 0.5)
      (setq finished-p 1))
    ((o-player-has-won-p board-state)
      (setq reward -1)
      (setq finished-p 1))
    ((x-player-has-won-p board-state)
      (setq reward 1)
      (setq finished-p 1)))

  (if learn-p
      (if _USE_ELIGIBILITY_TRACE_P
          (update-action-pref-list-with-eligibiity-trace x-knowledge-map
                                   board-id
                                     reward)
          (update-action-pref-list x-knowledge-map
                       board-id
                         reward)))
  
  (if finished-p
    (progn
      (defhash ttt-board _X_PLAYER_FINISHED_KEY 1)
      (if (gethash ttt-board _O_PLAYER_FINISHED_KEY)
        (return F)
        (progn
          (defhash ttt-board _PLAYER_MARK_KEY O-MARK-ID)
          (return O-MARK-ID))
        )))


  ; get the action x player intends to make
  (setq selected-place-id
      (or user-action-id
        (get-knowledge-epsilon-action x-knowledge-map 
                        board-id)))

  (defhash x-knowledge-map
       _LAST_INITIAL_BOARD_ID
       board-id)

  (defhash x-knowledge-map
       _LAST_ACTION_PLACE_ID
       selected-place-id)

  ; execute this action

  (incr-knowledge-count x-knowledge-map board-id)

  (update-board-state board-state selected-place-id X-MARK-ID)

  (defhash ttt-board
       _TTT_BOARD_STATE_KEY
       board-state)

  (defhash ttt-board _PLAYER_MARK_KEY O-MARK-ID)

  O-MARK-ID)


(defun execute-o-player-move (ttt-board user-action-id learn-p)

  (if (and (gethash ttt-board _X_PLAYER_FINISHED_KEY)
       (gethash ttt-board _O_PLAYER_FINISHED_KEY))
    (return F))

  (if (gethash ttt-board _O_PLAYER_FINISHED_KEY)
    (progn
      (defhash ttt-board _PLAYER_MARK_KEY X-MARK-ID)
      (return X-MARK-ID)))

  (setq o-knowledge-map
      (gethash ttt-board _O_KNOWLEDGE_MAP_KEY))

  (setq board-state
      (gethash ttt-board _TTT_BOARD_STATE_KEY))

  (setq board-id
      (get-board-state-id board-state))

  (setq reward 0)
  (setq finished-p F)
  (cond 
    ((draw-board-p board-state)
      (setq reward 0.5)
      (setq finished-p 1))
    ((o-player-has-won-p board-state)
      (setq reward 1)
      (setq finished-p 1))
    ((x-player-has-won-p board-state)
      (setq reward -1)
      (setq finished-p 1)))


  (if learn-p
      (if _USE_ELIGIBILITY_TRACE_P
          (update-action-pref-list-with-eligibiity-trace o-knowledge-map
                                   board-id
                                     reward)
          (update-action-pref-list o-knowledge-map
                       board-id
                         reward)))
  

  (if finished-p
    (progn
      (defhash ttt-board _O_PLAYER_FINISHED_KEY 1)
      (if (gethash ttt-board _X_PLAYER_FINISHED_KEY)
        (return F)
        (progn
          (defhash ttt-board _PLAYER_MARK_KEY X-MARK-ID)
          (return X-MARK-ID)))))


  ; get the action x player intends to make
  (setq selected-place-id
      (or user-action-id
        (get-knowledge-epsilon-action o-knowledge-map 
                        board-id)))
  

  (defhash o-knowledge-map
       _LAST_INITIAL_BOARD_ID
       board-id)

  (defhash o-knowledge-map
       _LAST_ACTION_PLACE_ID
       selected-place-id)

  ; execute this action

  (update-board-state board-state selected-place-id O-MARK-ID)

  (incr-knowledge-count o-knowledge-map board-id)

  (defhash ttt-board
       _TTT_BOARD_STATE_KEY
       board-state)

  (defhash ttt-board _PLAYER_MARK_KEY X-MARK-ID)

  X-MARK-ID)


(defun print-mark-id (mark-id)
  (gethash mark-display-map mark-id))



(defun make-next-ai-ttt-move (ttt-board-map)
  (setq next-player-mark
      (gethash ttt-board-map _PLAYER_MARK_KEY))

  (if (= O-MARK-ID
       next-player-mark)
    (execute-o-player-move ttt-board-map)
    (execute-x-player-move ttt-board-map)))

(defun get-ttt-player (ttt-board-map)
  (setq i 0)
  (setq continue 1)
  (lambda (user-place-id user-mark)
    (if continue
      (if (and user-mark 
           user-place-id)
        (if (= (gethash ttt-board-map _PLAYER_MARK_KEY)
               user-mark)
          (progn
            (set continue
               (if (= O-MARK-ID
                  user-mark)
                 (execute-o-player-move ttt-board-map (get-action-id user-place-id user-mark))
                 (execute-x-player-move ttt-board-map (get-action-id user-place-id user-mark))))
            (setq board
                (get-ttt-board-state ttt-board-map))
            (print-board board))
          "NOT USER'S TURN")
        (progn
          (set continue
             (make-next-ai-ttt-move ttt-board-map))
          (setq board
              (get-ttt-board-state ttt-board-map))
          (print-board board)))
      "BOARD IS FINISHED")))


(defun wait (milli)
  (setq stop 
      (+ (time)
         milli))

  (unless (> (time)
         stop)))


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


; Keep this
(defun incr-player-score (reset)
    (set player-score
         (or (and reset 0)
             (+ 1 player-score)))

    (if player-score-text
        (set-text player-score-text
              (string (integer player-score)))))

; Keep this
(defun incr-ai-score (reset)
    (set ai-score
         (or (and reset 0)
             (+ 1 ai-score)))
    (if ai-score-text
        (set-text ai-score-text
                  (string (integer ai-score)))))


; Keep this
(defun incr-draw-count (reset)
    (set draw-count
         (or (and reset 0)
             (+ 1 draw-count)))
    
    (if draw-score-text
        (set-text draw-score-text
              (string (integer draw-count)))))





; ** Modify this to initialize the AI ttt-board-map, perhaps loading from prior data

(setq ttt-board-map F)

(setq x-knowledge-map F)

(setq o-knowledge-map F)

(setq _X_KNOWLEDGE_DATA_KEY
     "X-KNOWLEDGE")

(setq _O_KNOWLEDGE_DATA_KEY
     "O-KNOWLEDGE")

(setq _CURRENT_AI_KEY "default-agent")

(setq board-text-map
      (make-int-hashtable))

(defun update-grid ()
    (for i
         3
         F
         (for j
              3
              F
              (set-text (gethash board-text-map
                                 (get-place-id i j))
                        (print-mark-id (get-mark-id-from-board-place (get-ttt-board-state ttt-board-map)
                                                                     (get-place-id i j)))))))


(defun draw-new-game-board ()
    (set finished F)
    (update-grid))

(defun save-current-ai-knowledge ()
  (set-data-value _O_KNOWLEDGE_DATA_KEY o-knowledge-map _CURRENT_AI_KEY)
  (set-data-value _X_KNOWLEDGE_DATA_KEY x-knowledge-map _CURRENT_AI_KEY))


(defun load-ai-knowledge ()
  (if (check-data-exists _X_KNOWLEDGE_DATA_KEY _CURRENT_AI_KEY)
        (set x-knowledge-map
             (get-data-value _X_KNOWLEDGE_DATA_KEY _CURRENT_AI_KEY))
        (set x-knowledge-map
             (make-knowledge-map)))

  (if (check-data-exists _O_KNOWLEDGE_DATA_KEY _CURRENT_AI_KEY)
        (set o-knowledge-map
             (get-data-value _O_KNOWLEDGE_DATA_KEY _CURRENT_AI_KEY))
        (set o-knowledge-map
             (make-knowledge-map))))


; ._._._. Updated ._._._.
(defun initialize ()
    (load-ai-knowledge)

    (draw-new-game-board)
    (incr-player-score 1)
    (incr-ai-score 1)
    (incr-draw-count 1))


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
                                                  "0")))))

(defmacro udpate-solution-map (solution-map mark)
    `(or (and (empty type-map)
              (defhash* ,solution-map i 1))
         (and (= 1 (length type-map))
              (setq score (gethash type-map ,mark))
              (defhash* ,solution-map i (integer (+ 1 score))))
         ,solution-map))

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
(setq _LEARN_FROM_USER_P F)
    
(setq ai-mark X-MARK-ID)

(setq player-mark O-MARK-ID)

(defun set-player-mark (mark-id)
    (set player-mark
         mark-id)

    (set ai-mark
         (get-next-player-mark mark-id)))

(setq start-player-mark F)

(defun set-start-player-mark (mark-id)
  (set start-player-mark mark-id)
  (set ttt-board-map
       (make-board-ttt-board mark-id x-knowledge-map o-knowledge-map)))



(setq mark-text-size 17)

(for i
     3
     board-text-map
     (for j
          3
          F
          (defhash board-text-map
                   (get-place-id i j)
                   (text " "
                        :width "match_parent"
                        :height "wrap_content"
                        :background-color "white"
                        :text-align "center"
                        :parent-align "center"
                        :padding 20
                        :text-size mark-text-size
                        :on-click (set-user-move (get-place-id i j))
                         ))))


(initialize)


; Replace this with something like execute-x-ply
(defun do-ai-move ()
    (if (= (gethash ttt-board-map _PLAYER_MARK_KEY) 
           ai-mark)
        (progn

          (if (= O-MARK-ID
                 ai-mark)
              (execute-o-player-move ttt-board-map)
              (execute-x-player-move ttt-board-map))
          (update-grid)
          (cond 
            ((draw-board-p (get-ttt-board-state ttt-board-map)) 
              (incr-draw-count) 
              (set finished 1) 
              (save-current-ai-knowledge))
            ( (o-player-has-won-p (get-ttt-board-state ttt-board-map)) 
              (if (= ai-mark O-MARK-ID)
                  (incr-player-score)
                  (incr-ai-score))
              (set finished 1)
              (save-current-ai-knowledge))
            ( (x-player-has-won-p (get-ttt-board-state ttt-board-map)) 
              (if (= ai-mark X-MARK-ID)
                  (incr-player-score)
                  (incr-ai-score))
              (set finished 1)
              (save-current-ai-knowledge))))))


; 
(defun set-user-move (place-id)
    
    (if (= (gethash ttt-board-map _PLAYER_MARK_KEY) 
           player-mark)
        (progn

          (if (= O-MARK-ID
                  player-mark)
              (execute-o-player-move ttt-board-map (get-action-id place-id player-mark) _LEARN_FROM_USER_P)
              (execute-x-player-move ttt-board-map (get-action-id place-id player-mark)) _LEARN_FROM_USER_P)
          (update-grid)
          (cond 
            ((draw-board-p (get-ttt-board-state ttt-board-map)) 
              (incr-draw-count) 
              (set finished 1)
              (save-current-ai-knowledge))
            ( (o-player-has-won-p (get-ttt-board-state ttt-board-map)) 
              (if (= player-mark O-MARK-ID)
                  (incr-player-score)
                  (incr-ai-score))
              (set finished 1)
              (save-current-ai-knowledge))
            ( (x-player-has-won-p (get-ttt-board-state ttt-board-map)) 
              (if (= player-mark X-MARK-ID)
                  (incr-player-score)
                  (incr-ai-score))
              (set finished 1)
              (save-current-ai-knowledge))))))


; Modify to remove coordinate-string
; <><><> updated
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
                       (get-place-id i j))))


; Update this to use place-ids
(defun create-grid ()
      (mapcar i
              (0 1 2)
              (horizontal-layout :width "match_parent"
                                 :height "wrap_content"
                                 (mapcar j
                                         (0 1 2)
                                         (create-grid-text i
                                                           j)))))






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
                                                           :on-click "(set-player-mark X-MARK-ID)")
                                         (radio-button "O"
                                                       :padding-left radio-padding
                                                       :on-click "(set-player-mark O-MARK-ID)"
                                                       :margin-left 13))
                 (text "Who starts:"
                       :parent-align "left"
                       :margin-top 5)
                 (horizontal-radio-group :width "match_parent"
                                         :height "wrap_content"
                                         (radio-button "Player" :on-click "(set-start-player-mark player-mark)" :padding-left radio-padding)
                                         (radio-button "AI"
                                                       :padding-left radio-padding
                                                       :on-click "(set-start-player-mark ai-mark)"
                                                       :margin-left 13))
                 (set board-container
                       (vertical-layout :width "match_parent"
                                        :height "wrap_content"
                                        (create-grid )))
                 
                 (shadow-button "New Game"
                         :parent-align "left"
                         :on-click "(draw-new-game-board) (if (and start-player-mark (= ai-mark start-player-mark)) (do-ai-move))"))
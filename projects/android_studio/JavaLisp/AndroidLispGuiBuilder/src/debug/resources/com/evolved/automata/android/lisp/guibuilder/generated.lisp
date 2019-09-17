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
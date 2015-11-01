(defmacro catch (exp ...)
    (setq signal (unique-id))

    `(signal-block (,signal)
        ((,signal) (try ,exp))
        (("RUNTIME_ERROR" exception) ,@...)))





(defun configure-robot ()
	(comment
		(configure-sonar current-nxt 0)
		(configure-switch current-nxt 2)
		(configure-switch current-nxt 3))

	(funcall (get-on-motor-port-map-lambda current-nxt "left motor" 0))
	(funcall (get-on-motor-port-map-lambda current-nxt "right motor" 2))

	(funcall (get-on-sensor-port-map-lambda current-nxt "sonar sensor" 0))
	(funcall (get-on-sensor-port-map-lambda current-nxt "left touch" 2))
	(funcall (get-on-sensor-port-map-lambda current-nxt "right touch" 3)))

(setq stop F)

(defun start-simple-forward ()
	(evaluate-background (catch 
							(progn

								(set stop F)
								(move-forward current-nxt 10)
								(unless (or stop
											(get-touch current-nxt
	                                     			   "left touch")
	                          				
											(get-touch current-nxt
	                                          		   "right touch")
	                           
	                						(< (setq current-distance
	                                    			 (get-sonar-value current-nxt
	                                                         		  "sonar sensor"))
	                						   (integer (get-text port-edit))))
									(evaluate-foreground (set-text distance-text
																   (string current-distance))))
								(stop-moving current-nxt))
							(set stop 1) (log "robot" exception) (stop-moving current-nxt))))


(defun stop-motion ()
	(set stop 1))

(defun force-stop ()
	(set-motor-power current-nxt
                    0 
                    0)
	(set-motor-power current-nxt
                    2 
                    0))


(defun get-selected-sensor ()
	(set-text port-edit
			  (string (get-sonar-value current-nxt
			  						   "sonar sensor"))))


(vertical-layout :width "match_parent"
				 :height "match_parent"
				 :background-color "white"
				 :padding 15
				 (button "configure ports"
				 		 :on-click (configure-robot))
				 (button "get sensor value"
				 		 :on-click (get-selected-sensor))

				 (setq port-edit
				 	   (edit "20"))

				 (button "test simple forward"
				 		 :width "wrap_content"
				 		 :height "wrap_content"
				 		 
				 		 :padding 10
				 		 :text-color "#F8E71C"
				 		 :text-style "bold"
				 		 :on-click (start-simple-forward))
				 (button "force-stop"
				 		 :on-click (force-stop))

				 (setq distance-text
				 	   (text "distance"))
				 (button "stop"
				 		 :on-click (stop-motion))
				 
				 )



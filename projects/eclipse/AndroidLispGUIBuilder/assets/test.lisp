(setq animated-text
	  (text :width "wrap_content"
	 	    :height "wrap_content"
	 	    "Thanos"
	 	    :padding 10
	 	    :background (create-shadow-background)
	 	    ))

(setq bounce-height 50)
(setq stop 1)
(setq up-milli 1000)

(vertical-layout :width "match_parent"
				 :height "match_parent"
				 :background-color "white"
				 

				 (button "test animation"
				 		 :width "wrap_content"
				 		 :height "wrap_content"
				 		 :on-click (progn
				 		 			  (set *stop F)
				 		 			  
				 		 			  (setq fract 0)
				 		 			  (setq start-time 
				 		 			  	    (time))
				 		 			  
			 		 			  	  (signal-block ("UP")
			 		 			  	  	  (("UP") (unless *stop
			 		 			  	  	  			  (setq fract 
				 		 			  	  	  	  			(/ (- (time) start-time)
				 		 			  	  	  	  			   *up-milli))
				 		 			  	  	  		  (if (< fract 1)
				 		 			  	  	  		  	  (progn
				 		 			  	  	  		  	  	(log "bounce" (* fract *bounce-height))
				 		 			  	  	  		  	  	(update-parameters animated-text
				 		 			  	  					 	 			 :margin-bottom (* fract *bounce-height)
				 		 			  	  					 	 			 1))
				 		 			  	  	  		  	  (progn
				 		 			  	  	  		  	  	 (setq stop-time 
				 		 			  	    					   (+ (time) *up-milli))
				 		 			  	  	  		  	  	 (signal "DOWN")))))
			 		 			  	  	  (("DOWN") (unless *stop
			 		 			  	  	  				(setq fract
			 		 			  	  	  					  (/ (- stop-time (time))
			 		 			  	  	  					  	 *up-milli))
			 		 			  	  	  				(if (> fract 0)
			 		 			  	  	  					(progn
			 		 			  	  	  						(log "bounce" (* fract *bounce-height))
			 		 			  	  	  						(update-parameters animated-text
			 		 			  	  	  									   :margin-bottom (* fract *bounce-height)
			 		 			  	  	  									   1))
			 		 			  	  	  					(progn
			 		 			  	  	  						(setq start-time
			 		 			  	  	  							  (time))
			 		 			  	  	  						(signal "UP"))))))))

				 (horizontal-layout :width "match_parent"
				 					:height 200
				 					:child-align "bottom"
				 					(solid :width 30
									 	   :height 20
									 	   :background (create-border)
									 	   :margin-bottom bounce-height)
				 					animated-text)
				 (button "stop-animation"
				 		 :on-click (set *stop 1)))

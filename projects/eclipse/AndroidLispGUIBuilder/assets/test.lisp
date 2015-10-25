(setq animated-text
	  (text :width "wrap_content"
	 	    :height "wrap_content"
	 	    "Thanos"
	 	    :padding 10
	 	    :text-color "white"
	 	    :background (create-shadow-background :shadow-angle -45
	 	    									  :shadow-width 8
	 	    									  :foreground-color "green"
	 	    									  :shadow-color "#8F000000")
	 	    ))

(setq bounce-height 50)
(setq stop 1)
(setq up-milli 1000)
(setq border-width 3)

(setq toggle F)

(defun configure-selected-tab (text selected)
  (update-parameters text
                     :background (if selected
                                    (create-border :foreground-color "white"
                                                :border-color "green"
                                                :top-width 2
                                                :left-width 2
                                                :right-width 2
                                                :bottom-width 2)
                                    (create-shadow-background :shadow-width 4
                                                       :shadow-color "#D8D8D8"
                                                       :foreground-color "white"
                                                       :shadow-angle -45)) 
                     1))


(defun toggle-background (view)
	(update-parameters view 
					   :background (if toggle 
					   				  (create-border :foreground-color "white"
                                                :border-color "green"
                                                :top-width 2
                                                :left-width 2
                                                :right-width 2
                                                :bottom-width 2)
					   				  (create-shadow-background :shadow-width 4
                                                       :shadow-color "#D8D8D8"
                                                       :foreground-color "white"
                                                       :shadow-angle -45))
					   1))





(vertical-layout :width "match_parent"
				 :height "match_parent"
				 :background-color "white"
				 

				 (button "test animation"
				 		 :width "wrap_content"
				 		 :height "wrap_content"
				 		 :padding 10
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
									 	   :background (create-border :foreground-color "white"
									 	   							  :border-color "#05346A"
									 	   							  :top-width border-width
									 	   							  :bottom-width border-width
									 	   							  :left-width border-width
									 	   							  :right-width border-width)
									 	   :margin-bottom bounce-height)
				 					animated-text)
				 (setq test 
				 	   (button "stop-animation"
				 		 	  :padding 10
				 		 	  :on-click (set *stop 1)))
				 (button "toggle back"
				 		 :on-click (progn
				 		 			  (toggle-background *test)
				 		 			  (set *toggle (not *toggle)))))



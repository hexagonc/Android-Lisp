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


(defun onNewSpeech (speech)
	(set-text asr-text-view
			  speech))

(setq asr-text-view
	  (text :width "match_parent"
	  		:height "wrap_content"))

(vertical-layout :width "match_parent"
				 :height "match_parent"
				 (text tts_availability
				 	   :text-color tts_color
				 	   :text-style "bold")
				 (text asr_availability
				 	   :text-color asr_color
				 	   :text-style "bold")


				 (set-enabled (button "start listening"
				 		 			  :on-click (if (and *ASR_AVAILABLE_P
			 		 			  						 (not (already-listening-p)))
			 		 			  					(start-speech-recognition)))
				 		 	  ASR_AVAILABLE_P)
				 (text "Transcription:"
				 	   :text-style "bold")
				 asr-text-view
				 (setq speech-edit
				 	   (edit :width "match_parent"
				 	   		 :height "wrap_content"))
				 (set-enabled (button "speak message"
				 					  :on-click (if (and *SPEECH_AVAILABLE_P
				 					  					 (not (already-speaking-p))
				 					  					 (> (length (trim (setq s
				 					  					 	   			  		(get-text *speech-edit))))
				 					  					 	0))
				 					  				(tts s)))
				 			 SPEECH_AVAILABLE_P))
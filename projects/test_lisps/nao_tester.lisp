(global
	(setq  MEMORY_SERVICE_NAME "ALMemory")
    (setq  BATTERY_SERVICE_NAME  "ALBattery")
    (setq  TOUCH_SERVICE_NAME  "ALTouch")
    (setq  SONAR_SERVICE_NAME  "ALSonar")
    (setq  MOTION_SERVICE_NAME  "ALMotion")
    (setq  POSTURE_SERVICE_NAME  "ALRobotPosture")
    (setq  VISION_SERVICE_NAME "ALVideoDevice")
    (setq  TTS_SERVICE_NAME "ALTextToSpeech")

	(setq nao-manager
		  (create-nao-manager))




	(connect-to-nao nao-manager 
  				     "hilbert.local"
  				     5000)

	(call-qi-function nao-manager
					  BATTERY_SERVICE_NAME
					  "getBatteryCharge")

	(call-qi-function nao-manager
					  TTS_SERVICE_NAME
					  "say"
					  "Hello, world!")

	(call-qi-function nao-manager
					  TTS_SERVICE_NAME
					  "getLanguage"
					  )

	(call-qi-function nao-manager
					  TTS_SERVICE_NAME
					  "getAvailableVoices")

	(call-qi-function nao-manager
					  TTS_SERVICE_NAME
					  "getVolume")

						 
	
	(call-qi-function nao-manager
					  BATTERY_SERVICE_NAME
					  "getBatteryCharge")

	; Wake up
	(call-qi-function nao-manager
					  MOTION_SERVICE_NAME
					  "wakeUp")

	(call-qi-function nao-manager
					  MOTION_SERVICE_NAME
					  "robotIsWakeUp")
	(call-qi-function nao-manager
					  MOTION_SERVICE_NAME
					  "moveInit")

	(call-qi-function nao-manager
					  MOTION_SERVICE_NAME
					  "rest")

	

	(call-qi-function nao-manager
					  TOUCH_SERVICE_NAME
					  "getSensorList")

	(call-qi-function nao-manager
					  SONAR_SERVICE_NAME
					  "subscribe"
					  "me")
	(call-qi-function nao-manager
					  SONAR_SERVICE_NAME
					  "unsubscribe"
					  "me")

	(call-qi-function nao-manager
					  SONAR_SERVICE_NAME
					  "getMemoryKeyList")

	(setq sonar-data-left-key
		  "Device/SubDeviceList/US/Left/Sensor/Value")

	(setq sonar-data-right-key
		  "Device/SubDeviceList/US/Right/Sensor/Value")
	
	(call-qi-function nao-manager
					  MEMORY_SERVICE_NAME
					  "getData"
					  sonar-data-left-key)

	(call-qi-function nao-manager
					  MEMORY_SERVICE_NAME
					  "getData"
					  sonar-data-right-key)

	(for i
		 20
		 F
		 (progn
		 	(setq start (time))
		 	(multiple-bind (left right)
		 				   (list (call-qi-function nao-manager
												   MEMORY_SERVICE_NAME
												   "getData"
												   sonar-data-left-key)
		 				   	     (call-qi-function nao-manager
												   MEMORY_SERVICE_NAME
												   "getData"
												   sonar-data-right-key)))
		 	(setq latency
		 		  (- (time)
		 		  	 start))
		 	(log "timing test <><>" 
		 		  (concat i  
		 		  		  ") result: " 
		 		  		  (list left right)
		 			 	  " latency: "
		 			 	  latency 
		 			 	  " ms"))
		 	))


	(call-qi-function nao-manager
					  MOTION_SERVICE_NAME
					  "moveInit")

	(call-qi-function nao-manager
					  MOTION_SERVICE_NAME
					  "moveTo"
					  0.25
					  0.0
					  0.0)

	(call-qi-function nao-manager
					  MOTION_SERVICE_NAME
					  "stopMove")

	(progn
		(setq stop-time
			  (+ (time) 3000))
		(call-qi-function nao-manager
					  MOTION_SERVICE_NAME
					  "moveToward"
					  0.25
					  0.0
					  0.0)
		(unless (> (time) stop-time))
		(call-qi-function nao-manager
					  MOTION_SERVICE_NAME
					  "stopMove")

		)

	(log "foreg" "junk")
	

	; ++++++++++++++++++++++++++++++++++++++
	; Vision
	; ++++++++++++++++++++++++++++++++++++++
	(setq client_name "lisptester")

	(setq top-camera 0)

	(setq bottom-camera 1)

	(setq camera-resolution-map
		  (make-string-hashtable (list (list "40*30" 8)
		  							   (list "80*60" 7)
		  							   (list "160*120" 0)
		  							   (list "320*240" 1)
		  							   (list "640*480" 2)
		  							   (list "1280*960" 3))))


	(defun get-resolution-id (key)
		(gethash camera-resolution-map key))

	(setq color-space-id 11)

	(setq requested-fps 15)

	; Subscribe to camera
	(setq top-camera-client-id
		  (call-qi-function nao-manager
						  VISION_SERVICE_NAME
						  "subscribeCamera"
						  client_name
						  top-camera
						  (get-resolution-id "640*480")
						  color-space-id
						  requested-fps))

	(setq bottom-client-actual-id
		  (call-qi-function nao-manager
						  VISION_SERVICE_NAME
						  "subscribeCamera"
						  client_name
						  bottom-camera
						  (get-resolution-id "640*480")
						  color-space-id
						  requested-fps))

	

	; Unsubscribe

	(call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "unsubscribe"
					  bottom-client-actual-id)
	(call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "unsubscribe"
					  top-camera-client-id)


	; Testing
	(setq client-actual-id 
		  (last (call-qi-function nao-manager
		  						  VISION_SERVICE_NAME
		  						  "getSubscribers")))


	; Opens and initializes the video source device

	(call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "openCamera"
					  top-camera)

	; Starts the video capture of the specified video device
	(call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "startCamera"
					  top-camera)

	; Retrieves the latest image from the video source and send the data coming directly from the raw buffer 
	; as an ALValue through the network (no format conversion)

	(setq top-image-spec
		  (call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "getImageRemote"
					  top-camera-client-id))


	(multiple-bind (width height num-layers colorspace time-stamp-second time-stamp-micro image camera-id camera-left-fov camera-top-fov camera-right-fov camera-bottom-fov)
		top-image-spec)

	(setq image-string
		  (encode-byte-buffer-as-base64-string image))

	(set-data-value "nao-test-image-rgb-top" image-string "nao-tests")


	(get-data-value "nao-test-image-rgb-top" "nao-tests")
	; Stops the video capture of the specified video device
	(call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "stopCamera"
					  top-camera)

	; Closes the video source device, releasing resources
	(call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "closeCamera"
					  top-camera)


	; ********************************************************
	; Get bottom image
	; ********************************************************

	(call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "openCamera"
					  bottom-camera)

	; Starts the video capture of the specified video device
	(call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "startCamera"
					  bottom-camera)

	; Retrieves the latest image from the video source and send the data coming directly from the raw buffer 
	; as an ALValue through the network (no format conversion)

	(setq bottom-image-spec
		  (call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "getImageRemote"
					  bottom-client-actual-id))


	(multiple-bind (width height num-layers colorspace time-stamp-second time-stamp-micro image camera-id camera-left-fov camera-top-fov camera-right-fov camera-bottom-fov)
		bottom-image-spec)

	(setq image-string
		  (encode-byte-buffer-as-base64-string image))

	(set-data-value "nao-test-image-rgb-bottom" image-string "nao-tests")


	(get-data-value "nao-test-image-rgb-bottom" "nao-tests")
	; Stops the video capture of the specified video device
	(call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "stopCamera"
					  bottom-camera)

	; Closes the video source device, releasing resources
	(call-qi-function nao-manager
					  VISION_SERVICE_NAME
					  "closeCamera"
					  bottom-camera)
	


	; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	; 			Vision YUV JPEG Desktop functions
	; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+




	(setq top-output-file "/Users/Evolved8/Dropbox/algb_2/top-image.png")

	(setq bottom-output-file "/Users/Evolved8/Dropbox/algb_2/bottom-image.png")



	(setq height 480)

	(setq width 640)


	(setq top-png-buffer 
		  (decode-base64-string-to-byte-buffer (get-data-value "nao-test-image-raw-top" "nao-tests")))


	(save-YUV422-byte-buffer-to-png-file top-png-buffer height width top-output-file )
	
	(setq bottom-png-buffer 
		  (decode-base64-string-to-byte-buffer (get-data-value "nao-test-image-raw-bottom" "nao-tests")))

	(save-YUV422-byte-buffer-to-png-file top-png-buffer height width top-output-file )

	(save-YUV422-byte-buffer-to-png-file bottom-png-buffer height width bottom-output-file)

	(save-rgb-byte-buffer-to-png-file (decode-base64-string-to-byte-buffer (get-data-value "nao-test-image-rgb-top" "nao-tests")) height width "/Users/Evolved8/Dropbox/algb_2/top-rgb-image.png" )


	; o~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~o
	; Google Vision Tests
	; o~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~oOo~o

	(setq height 480)

	(setq width 640)


	(setq top-rgb-buffer 
		  (decode-base64-string-to-byte-buffer (get-data-value "nao-test-image-rgb-top" "nao-tests")))

	(setq bottom-rgb-buffer 
		  (decode-base64-string-to-byte-buffer (get-data-value "nao-test-image-rgb-bottom" "nao-tests")))


	(setq top-png-buffer 
		  (convert-rgb-byte-buffer-to-png-byte-buffer top-rgb-buffer height width))

	(setq bottom-png-buffer
		  (convert-rgb-byte-buffer-to-png-byte-buffer bottom-rgb-buffer height width))

	(setq api-key "use-yours")
	(setq google-label-result
		  (get-google-vision-png-result top-png-buffer "LABEL_DETECTION" 3 api-key))

	(setq google-text-result
		  (get-google-vision-png-result top-png-buffer "TEXT_DETECTION" 3 api-key))

	)


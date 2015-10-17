(vertical-layout :width "match_parent"
				 :height "match_parent"
				 :background-color "white"
				 
				 (solid :width 30
				 		:height 20
				 		:background (create-border)
				 		:parent-align "center"
				 		)
				 (text :width "wrap_content"
				 	   :height "wrap_content"
				 	   "Thanos"
				 	   :padding 10
				 	   :background (create-shadow-background)
				 	   :margin-top 20
				 	   )
				 (solid :background (create-shadow-background)
				 		:width 100
				 		:height 30)

				 )

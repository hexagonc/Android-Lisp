; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
; Android Lisp GUI Builder Basic Tutorial
; In this tutorial, I will introduce the basic user interface functions
; and command syntax that you will need in order to understand the more
; substantial samples and examples.  This tutorial will be given as
; comments in a test source file, such as this.  All code uses a lisp-like
; syntax, so in keeping with that, line comments are delimited by starting a
; line with a semi-colon, ";".  Multi-line commented code is delimited by the
; "(comment ...)" function.

; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
; Using this tutorial
; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

; This source file is divided into 12 lessons.  These lessons are contained
; in "progn" function blocks, which can be easily navigated using the sibling
; navigation buttons which look like, "|<" or "|>".  In order to use the code
; navigation buttons, set the code page to "read-only" mode. (Click the button labeled
; as "Read-only: false".  Clicking this toggles the page to read only mode and the
; button should say: "Read-only: true")  Once the page is in read-only mode,
; you can use the touch screen to move the cursor near the source code that you
; want to evaluate.  Press the parent select button, "^", which highlights the
; innermost parent object to where the cursor currently is.

; For example, consider the following function, which computes 13 + 5:

(+ 13 25)

; Put the cursor anywhere between the parenthesis.  Press the "^" button
; until the entire expression, "(+ 13 5)", is highlighted.  Having done this
; press the "EVAL" button, which should display "18.0" in the results.  You can
; navigate between adjacent expressions that are at the same level using the
; "|<" and "|>" navigation buttons.  For, example consider the following block
; of expressions:

(progn
    (+ 13 5)
    (* 13 5))

; If you put the cursor in the "(+ 13 5)" expression and press "^" to select
; the whole expression, you can navigate between "(+ 13 5)" and "(* 13 5)" using
; |<" and "|>".  If you want to move up further to the entire "progn" expression,
; press the "^" button again.  If you press "EVAL" now, all expressions inside
; will be evaluated sequentially, in the order they appear.  Thus, the result
; will be "65.0".  "(+ 13 5)" was also evaluated but the result that displays in
; the result pane is the value of the last expression in the block, which was
; "(+ 13 5)".  If the entire block is selected, you can move down to the first
; child inside, by pressing the down button "⌵".

; Now that basic navigation is explained, we can begin the actual lessons.

(progn
    ; Lesson 1 - Basic user interfaces
    ; Evaluate the following block of code to show "hello world".

    (set-top-view (vertical-layout :width "match_parent"
                                   :height "match_parent"
                                   (text "Hello world")))

    ; After evaluating this, select "To Render Screen" from the main menu in order to view it.
    ; In order to switch back to the source code editor, select "To Code View" from the
    ; main menu, from the render view.

    ; Evaluating the "vertical-layout" function, creates a proxy object for an Android
    ; LinearLayout with orientation "vertical".  There are lisp functions for most of the basic
    ; Android widgets and most have function names that are suggestive of the Android class name
    ; being created.  Selecting, "To Render Screen" actually constructs the views from the
    ; proxies and attaches them to the view hierarchy.

    ; These view proxy objects can themselves be stored in variables, so evaluating the
    ; following block works as well:

    (progn
        (setq view-proxy
              (vertical-layout :width "match_parent"
                               :height "match_parent"
                               (text "Hello world")))
        (set-top-view view-proxy))

    ; The function 'set-top-view' defines the view proxy object that will be bound to the
    ; window when you select the "To Render Screen" menu object.

    ; All functions that return view proxy objects accept keyword parameters that are
    ; similiar to the attributes that would be used in layout resource xml files that
    ; are normally used to define Android views.  The background color of the parent
    ; LinearLayout (defined by the vertical-layout function) can be set with the "background-color"
    ; keyword:

    (progn
        (setq view-proxy
              (vertical-layout :width "match_parent"
                               :height "match_parent"
                               :background-color "red"
                               (text "Hello world")))
        (set-top-view view-proxy))

    ; Colors are specified as string parameters and any value that works with the
    ; Color.parseColor(..) method can be used with the view proxies, thus, we can also
    ; use colors, specified as hexadecimal strings, like "#DCF9FF".  Keyword parameters
    ; can be stored in strings instead of provided as literals, so the following code
    ; block can also be used to set the color of vertical LinearLayout:
    (progn
        (setq parent-color "#DCF9FF")
        (setq view-proxy
              (vertical-layout :width "match_parent"
                               :height "match_parent"
                               :background-color parent-color
                               (text "Hello world")))
        (set-top-view view-proxy))

    )


(progn
    ; Lesson 2 - Changing height and width
    ; By default, view proxies have height and width layout parameters set to "wrap_content",
    ; except for the view that is passed to "set-top-view", whose layout parameters are forced
    ; to be "match_parent".  To illustrate this difference evaluate the following block:

    (progn
        (setq parent-color "#DCF9FF")
        (setq child-color "red")
        (setq view-proxy
              (vertical-layout :background-color parent-color
                               (text :background-color child-color
                                     "Hello world")))
        (set-top-view view-proxy))

    ; The values of height and width keyword parameters depend on the parent of the view proxy
    ; that these parameters apply to, however, all view proxies support string height and width
    ; parameters of "match_parent" and "wrap_content".  Numeric height and width parameters can
    ; also be provided, in which case, they will be interpreted as device pixel values, as illustrated
    ; below:

    (progn
        (setq parent-color "#DCF9FF")
        (setq child-color "red")
        (setq view-proxy
              (vertical-layout :background-color parent-color
                               (text :background-color child-color
                                     :width 100
                                     :height 100
                                     "Hello world")))
        (set-top-view view-proxy))
    )

(progn
    ; Lesson 3 - Updating keywords of view proxies after construction
    ; The update-parameters function can be used the change most of the keywords
    ; that define the attributes of a view proxy after construction.  Start by
    ; evaluating the following expression:

    (progn
        (setq parent-color "#DCF9FF")
        (setq child-color "red")
        (setq view-proxy
              (vertical-layout :background-color parent-color
                               (text :background-color child-color
                                     "Hello world")))
        (set-top-view view-proxy))

    ; View this by switching to the render view.
    ; You can change the color of view-proxy by evaluating this function:

    (update-parameters view-proxy :background-color "green")

    ; Now switch back to the render view and notice that the parent view's
    ; color has been changed to green without having to call "set-top-view".
    ; What happens is that the view proxies contain a weak reference to the
    ; native Android view at the point that the view is attached to the window.
    ; When you call update-parameters on the view proxy, the proxy checks if
    ; the weak reference is still valid, and if so, it updates the color of the
    ; android view immediately.  If the weak reference is not valid, then it
    ; saves the updated color and the next time the view proxy reconstructs the
    ; native Android view, it uses the updated color instead of the original
    ; color that was defined when the view proxy was created.
    )

(progn
    ; Lesson 4 - Basic event handlers
    ; Nearly all view proxies support setting an on-click and on-long-click listener.
    ; These are defined by the :on-click and :on-long-click keywords.
    ; Evaluate the following block of code and switch to the Render view.  Click on the
    ; red background of the TextView and notice the changing colors of the parent view.
    (progn
        (setq parent-color-1 "#DCF9FF")
        (setq parent-color-2 "#FFDCDC")
        (setq parent-color parent-color-1)
        (setq child-color "red")
        (setq view-proxy
              (vertical-layout :background-color parent-color
                               (text :background-color child-color
                                     :width 100
                                     :height 100
                                     :on-click (if (equals parent-color parent-color-1)
                                                   (update-parameters view-proxy
                                                                      :background-color (setq parent-color parent-color-2))
                                                   (update-parameters view-proxy
                                                                      :background-color (setq parent-color parent-color-1)))
                                     "Hello world - click me!!")))
        (set-top-view view-proxy))

    ; Touching the red background of the "Hello World" TextView toggles the background color
    ; of the parent view, via the update-parameters function.  The reason this is able work
    ; is because the ":on-click" keyword argument is not evaluated immediately, at the point
    ; that the "text" view proxy function is evaluated.  Instead, the code after ":on-click"
    ; is saved as a raw lisp s-expression which is evaluated dynamically by the View.OnClickListener
    ; that is passed to the native Android TextView at the point of construction.  This allows
    ; the on-click listener to remain aware of the current state of the runtime environment,
    ; even as the actual value of the "parent-color" lexical variable changes across multiple
    ; calls to onClick of the listener.  This is different from the way most keyword parameters
    ; processed; most keyword parameters, such as ":height", ":width", and ":background-color"
    ; are evaluated one time at the point that the view proxy is created.

    )

(progn
    ; Lesson 5 - More about LinearLayouts
    ; There are two separate functions to create proxies for the native Android LinearLayout.
    ; These functions are "vertical-layout" and "horizontal-layout".  They function identically
    ; except for one creates a LinearLayout with layout_orientation = "vertical" and one with
    ;  layout_orientation = "horizontal".  The two separate functions are used simply to avoid
    ; having to always set the orientation and thus make the view proxy source a bit more
    ; readable.

    ; vertical-layout and horizontal-layout are special because they allow the use of percentage
    ; values for the height (in the case of vertical-layouts) or width (in the case of horizontal-layout).
    ; Evaluate the following example and observe the render view:

    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq parent-view
              (horizontal-layout :width _MATCH_PARENT
                                 :height _MATCH_PARENT
                                 (text :width "40%"
                                       :height _WRAP_CONTENT
                                       :background-color "red"
                                       "Hello")
                                 (text :width "60%"
                                       :height _WRAP_CONTENT
                                       :background-color "blue"
                                       "World")))
        (set-top-view parent-view))

    ; Below you will find the vertical equivalent of the view above

    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq parent-view
              (vertical-layout :width _MATCH_PARENT
                               :height _MATCH_PARENT
                               (text :width _WRAP_CONTENT
                                     :height "40%"
                                     :background-color "red"
                                     "Hello")
                               (text :width _WRAP_CONTENT
                                     :height "60%"
                                     :background-color "blue"
                                     "World")))
        (set-top-view parent-view))

    ; The children of vertical-layout or horizontal-layouts can use a mixture
    ; of dimensions specified with percentages and "wrap_content" and explicit
    ; device pixel values.  The way this works is that the dimensions of the
    ; child views with explicit pixel values and "wrap_content" are first positioned
    ; within the parent view, then, the child views with percentage dimensions are
    ; positioned proportionally amongst the remaining space within the parent.  In the
    ; following example, the width of the first child is set with "wrap_content", using
    ; the minimum amount of space required to display its contents, whereas the remaining
    ; children have widths set as percentages of the remaining space.
    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq parent-view
              (horizontal-layout :width _MATCH_PARENT
                                 :height _MATCH_PARENT
                                 (text :background-color "#483F86"
                                       "Step 1: "
                                       :text-color "white"
                                       :text-style "bold"
                                       :text-size 20)
                                 (text :width "50%"
                                       :height _WRAP_CONTENT
                                       :background-color "yellow"
                                       "Hello")
                                 (text :width "50%"
                                       :height _WRAP_CONTENT
                                       :background-color "blue"
                                       "World")))
        (set-top-view parent-view))

    ; Notice the additional formating keywords that are used in the first TextView.
    ; In general, keywords and non-keyword arguments to a function (in this case, "text")
    ; can be interspersed arbitrarily amongst themselves so long as the relative order of
    ; the non-keyword parameters respect the signature of the function definition.  However
    ; one should probably adopt a convention of either specifiying all keyword parameters
    ; before or after the non-keyword parameters for readility and to reduce the chance of
    ; errors.  Note that all keyword parameters are specified in pairs.  Any argument to
    ; a function that start with a colon, ":", is interpretted as a keyword parameter.  Then
    ; the immediately following argument is assumed to be the value assigned to this keyword.

    ; Here is another example, where the first and last children have explicit widths specified
    ; whereas the middle two children have percentage widths.
    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq parent-view
              (horizontal-layout :width _MATCH_PARENT
                                 :height _MATCH_PARENT
                                 (text :background-color "#483F86"
                                       "Start"
                                       :text-color "white"
                                       :text-style "bold"
                                       :text-size 20)
                                 (text :width "50%"
                                       :height _WRAP_CONTENT
                                       :background-color "yellow"
                                       "Hello")
                                 (text :width "50%"
                                       :height _WRAP_CONTENT
                                       :background-color "blue"
                                       "World")
                                 (text :background-color "#483F86"
                                       "Finish"
                                       :text-color "white"
                                       :text-style "bold"
                                       :text-size 20)
                                 ))
        (set-top-view parent-view))
    ; Note that it would be an error to use a percentage value for the width of a view
    ; if its parent isn't a horizontal-layout.  Similarly, it would be an error to
    ; use a percentage value for the height of a view if its parent isn't a vertical-layout.
    ; Thus, evaluating and trying to render the following code will result in an error:

    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq parent-view
              (horizontal-layout :width _MATCH_PARENT
                                 :height _MATCH_PARENT
                                 (text :height "10%"
                                       :background-color "#483F86"
                                       :text-color "white"
                                       :text-style "bold"
                                       :text-size 20
                                       "Start")
                                 (text :width "50%"
                                       :height _WRAP_CONTENT
                                       :background-color "yellow"
                                       "Hello")
                                 (text :width "50%"
                                       :height _WRAP_CONTENT
                                       :background-color "blue"
                                       "World")
                                 (text :background-color "#483F86"
                                       "Finish"
                                       :text-color "white"
                                       :text-style "bold"
                                       :text-size 20)
                                 ))
        (set-top-view parent-view))

    ; Note that the height of the first child is set to "10%" which is an
    ; invalid height for a child of a horizontal-layout, thus the system
    ; will show the error notification icon (which can be clicked to view the
    ; system log which contains the error details).



    )

(progn
    ; Lesson 6 - Buttons and alignment
    ; Although most views can be clicked and used as "buttons", there is a function
    ; which can used specifically to create native Android buttons.  The following
    ; example shows the use of buttons as well as modifying the alignment of a child
    ; view within its parent.  Evaluate the following code block and switch to the
    ; render view:

    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq alignment-list
              (list "left" "center" "right"))

        (setq alignment-id 0)

        (setq parent-view
              (vertical-layout (setq hello-text
                                     (text :width _WRAP_CONTENT
                                           :height _WRAP_CONTENT
                                           :background-color "yellow"
                                           :parent-align (nth alignment-list alignment-id)
                                           "Hello world"))
                               (button "Change alignment"
                                       :on-click (progn
                                                    (setq alignment-id
                                                          (mod (+ alignment-id 1)
                                                               3))
                                                    (update-parameters hello-text
                                                                       1
                                                                       :parent-align (nth alignment-list alignment-id))))))

        (set-top-view parent-view))

    ; There are a few interesting new features in this example.  The keyword parameter ":parent-align" is
    ; used to set of the alignment of a child within its parent (it sets the layout gravity of the native child
    ; Android view).  Likewise, some views that can have children, like LinearLayouts (created by vertical-layout
    ; and horizontal-layout), can accept the keyword ":child-align" (which sets the gravity of the native Android viewgroup).
    ; This keyword sets the default alignment of its children.

    ; In the example above, we are dynamically permuting the alignment of the child view, hello-text, within its parent.
    ; An important difference between the use of "update-parameters" in this example and prior examples is the presence of
    ; the number "1" after the "hello-text" argument.  This "1" is actually interpretted as a boolean parameter indicating
    ; that the system should request layout on the view contained in "hello-text" after updating its parameters.  This is
    ; a technicality of native Android and is required when the layout parameters of a view changes and we want the changes
    ; to be reflected immediately.  Some keyword parameters of a view proxy change layout parameters behind the scenes, such
    ; as height changes and alignment changes, whereas others, like background changes and visibility changes do not involve
    ; layout changes.  The updates that do not change the layout do not require the request layout parameter be true.

    ; This example also illustrates the use of boolean parameters and optional arguments.  The function "update-parameters"
    ; requires a combination of keyword and standard arguments, some of whom are optional.  The keyword parameters are all
    ; optional, whereas the standard arguments must appear in the correct order and with the correct types.  For
    ; "update-parameters", the first standard argument must be a view proxy and the second remaining argument is optional,
    ; which must have a boolean type.  With this lisp-like scripting language, boolean types are defined merely by the
    ; presence or absence of the "false" value, which is represented by the literal "F" token.  Any value that does not evaluate
    ; to "F" is considered "true".  Thus, the value "1" that was passed to "update-parameters" could have been replaced by
    ; nearly any other value and the behavior would have been the same since nearly all values are "truthy" by default and
    ; only "F" or expressions that evaluate to "F" are "falsy".

    ; Evaluate and render the next view to see the vertical equivalent to the same demonstration:

    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq alignment-list
              (list "top" "center" "bottom"))

        (setq alignment-id 0)

        (setq parent-view
              (horizontal-layout :child-align "center"
                                 (setq hello-text
                                       (text :width _WRAP_CONTENT
                                             :height _WRAP_CONTENT
                                             :background-color "yellow"
                                             :parent-align (nth alignment-list alignment-id)
                                             :margin-right 20
                                             "Hello world"))
                                 (button "Change alignment"
                                         :on-click (progn
                                                      (setq alignment-id
                                                            (mod (+ alignment-id 1)
                                                                 3))
                                                      (update-parameters hello-text
                                                                         1
                                                                         :parent-align (nth alignment-list alignment-id))))))

        (set-top-view parent-view))

    ; For vertical-layout and horizontal-layout, the alignment options for the children are perpendicular to the direction of the
    ; parent view.  Thus, for vertical-layout, your alignment options are "left" "center" "right" whereas the alignment options
    ; for horizontal-layout are "top" "center" "bottom".  In the following example, I introduce the function "relative" which creates a
    ; proxy for RelativeLayouts.  You have more flexible aligment options for this parent view because you can alignment to all corners and
    ; all sides of the parent view.  This is illustrated in the next example code:

    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq alignment-list
              (list "left|top" "top|center" "top|right" "right|center" "right|bottom" "center|bottom" "left|bottom" "left|center" "center_vertical|center_horizontal"))

        (setq alignment-id 0)

        (setq parent-view
              (vertical-layout (relative :width _MATCH_PARENT
                                         :height 150
                                         :background-color "yellow"
                                         (setq relative-child
                                               (text :width _WRAP_CONTENT
                                                     :height _WRAP_CONTENT
                                                     :parent-align (nth alignment-list alignment-id)
                                                     "Hello world")))
                               (button "Change alignment"
                                       :on-click (progn
                                                    (setq alignment-id
                                                          (mod (+ alignment-id 1)
                                                               (length alignment-list)))
                                                    (update-parameters relative-child
                                                                       1
                                                                       :parent-align (nth alignment-list alignment-id))))))

        (set-top-view parent-view))

    ; This example shows all the alignment options that are currently supported with the lisp view proxy
    ; for RelativeLayouts.  Unfortunately, layout rules that relate one child to another, such as:
    ; "toLeftOf" and "toRightOf" is not supported at the moment.

    )

(progn
    ; Lesson 7 - Backgrounds and images
    ; Backgrounds are not limited to a single color.  Instead, the Android Lisp GUI Builder allows you define
    ; backgrounds with borders, gradient colors and arbitrary drawable resources.  You can create the
    ; appearance of elevation to a view by setting shadow backgrounds.  In order to create backgrounds more
    ; complex than single colors, one must use the "create-background" function.  This function uses a number
    ; of custom keywords that don't relate to existing Android xml attributes so they are best explained with
    ; the following examples.  Evaluate and render the following code:

    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")
        (setq container-dimension 125)
        (setq container-color "#217827")

        (setq parent-view
              (scrollview
                    (vertical-layout :width _MATCH_PARENT
                                     :height _MATCH_PARENT
                                     (horizontal-layout :width _MATCH_PARENT
                                                        :height _WRAP_CONTENT
                                                        (relative :width container-dimension
                                                                  :height container-dimension
                                                                  :background-color container-color
                                                                  :margin-bottom 2
                                                                  :margin-right 10
                                                                  (text "Example 1"
                                                                        :text-color "white"
                                                                        :padding 10
                                                                        :parent-align "center_vertical|center_horizontal"
                                                                        :background (create-background :foreground-color "#782134"
                                                                                                       :border-color "#212178"
                                                                                                       :border-width 2)))
                                                        (text :width "100%"
                                                              :height _MATCH_PARENT
                                                              :text-align "top|left"
                                                              "Background with same border on all sides"))
                                     (horizontal-layout :width _MATCH_PARENT
                                                        :height _WRAP_CONTENT
                                                        (relative :width container-dimension
                                                                  :height container-dimension
                                                                  :background-color container-color
                                                                  :margin-bottom 2
                                                                  :margin-right 10
                                                                  (text "Example 2"
                                                                        :text-color "white"
                                                                        :padding 10
                                                                        :parent-align "center_vertical|center_horizontal"
                                                                        :background (create-background :foreground-color "#782134"
                                                                                                       :border-color "#212178"
                                                                                                       :border-top-width 2
                                                                                                       :border-right-width 2)))
                                                        (text :width "100%"
                                                              :height _MATCH_PARENT
                                                              :text-align "top|left"
                                                              "Background with different border widths on sides"))
                                     (horizontal-layout :width _MATCH_PARENT
                                                        :height _WRAP_CONTENT
                                                        (relative :width container-dimension
                                                                  :height container-dimension
                                                                  :background-color container-color
                                                                  :margin-bottom 2
                                                                  :margin-right 10
                                                                  (text "Gradient Example"
                                                                        :text-color "black"
                                                                        :text-style "bold"
                                                                        :padding 10
                                                                        :parent-align "center_vertical|center_horizontal"
                                                                        :background (create-background :foreground-start-color "#6C6C6D"
                                                                                                       :foreground-stop-color "#E1E1E1"
                                                                                                       :foreground-gradient-type "RIGHT_LEFT"
                                                                                                       :border-top-width 2
                                                                                                       :border-right-width 2)))
                                                        (text :width "100%"
                                                              :height _MATCH_PARENT
                                                              :text-align "top|left"
                                                              :textIsSelectable "true"
                                                              "Metalic looking background, using gradient background.  The gradient direction is taken from GradientDrawable.Orientation"))
                                     (horizontal-layout :width _MATCH_PARENT
                                                        :height _WRAP_CONTENT
                                                        (relative :width container-dimension
                                                                  :height container-dimension
                                                                  :background-color container-color
                                                                  :margin-bottom 2
                                                                  :margin-right 10
                                                                  (text "Rounded corners"
                                                                        :text-color "white"
                                                                        :parent-align "center_vertical|center_horizontal"
                                                                        :background (create-background :foreground-color "#782134"
                                                                                                       :corner-radius 5)))
                                                        (text :width "100%"
                                                              :height _MATCH_PARENT
                                                              :text-align "top|left"
                                                              :textIsSelectable "true"
                                                              "The corner radius is 5dp"))
                                     (horizontal-layout :width _MATCH_PARENT
                                                        :height _WRAP_CONTENT
                                                        (relative :width container-dimension
                                                                  :height container-dimension
                                                                  :background-color container-color
                                                                  :margin-bottom 2
                                                                  :margin-right 10
                                                                  (text "Shadows"
                                                                        :text-color "black"
                                                                        :padding 10
                                                                        :parent-align "center_vertical|center_horizontal"
                                                                        :background (create-shadow-background :foreground-color "#DCF9FF"
                                                                                                              :shadow-width 5
                                                                                                              :shadow-color "#901A1A1A"
                                                                                                              :shadow-angle -45
                                                                                                              )))
                                                        (text :width "100%"
                                                              :height _MATCH_PARENT
                                                              :text-align "top|left"
                                                              :textIsSelectable "true"
                                                              "The shadow width is 2dp, shadow color is #901A1A1A and shadown angle is -45 degrees"))
                                     (horizontal-layout :width _MATCH_PARENT
                                                        :height _WRAP_CONTENT
                                                        (relative :width container-dimension
                                                                  :height container-dimension
                                                                  :background-color container-color
                                                                  :margin-bottom 2
                                                                  :margin-right 10
                                                                  (text "X"
                                                                        :padding 10
                                                                        :width 50
                                                                        :height 50
                                                                        :text-color "black"
                                                                        :text-align "center_vertical|center_horizontal"
                                                                        :parent-align "center_vertical|center_horizontal"
                                                                        :background (create-background :foreground-color "#FBEDFB"
                                                                                                       :border-width 3
                                                                                                       :border-color "#02004C"
                                                                                                       :corner-radius 25)))
                                                        (text :width "100%"
                                                              :height _MATCH_PARENT
                                                              :text-align "top|left"
                                                              :textIsSelectable "true"
                                                              "Circular border.  The height and width should be the same and the corner radius should be 1/2 of the side lengths.  In this example, side length is 50 and corner radius is 25.  Border width is 3."))
                                     (horizontal-layout :width _MATCH_PARENT
                                                        :height _WRAP_CONTENT
                                                        (relative :width container-dimension
                                                                  :height container-dimension
                                                                  :background-color container-color
                                                                  :margin-bottom 2
                                                                  :margin-right 10
                                                                  (text "X"
                                                                        :width 50
                                                                        :height 50
                                                                        :text-color "black"
                                                                        :focusable-in-touch-mode "true"
                                                                        :focusable "true"
                                                                        :on-click ()
                                                                        :text-align "center_vertical|center_horizontal"
                                                                        :parent-align "center_vertical|center_horizontal"
                                                                        :background (create-background :foreground-color "#FBEDFB"
                                                                                                       :border-width 3
                                                                                                       :border-color "#02004C"
                                                                                                       :corner-radius 25
                                                                                                       :on-pressed-drawable (create-background :foreground-color "yellow"
                                                                                                                                               :border-width 3
                                                                                                                                               :border-color "black"
                                                                                                                                               :corner-radius 25))))
                                                        (text :width "100%"
                                                              :height _MATCH_PARENT
                                                              :text-align "top|left"
                                                              :textIsSelectable "true"
                                                              "Basic support for changing background depending on state.  Currently only on press and on-disabled is supported.")))))
        (set-top-view parent-view))

    ; This extensive example, in addition to showing all the basic features of the "create-background"
    ; function, also shows the use of the "scrollview" function.  This function, as the name suggests,
    ; creates a proxy for a native Android ScrollView.  This view, which normally only has a single child,
    ; has the property that if its child's height is less than its height, then the scrollview acts as
    ; if its isn't there at all. On the other hand, if the height of its child needs to be taller than
    ; the ScrollView, then the ScrollView allows the child's contents to be scrolled.  A standard
    ; pattern is to set the height of a ScrollView to something fixed, like match_parent or a fixed
    ; percentage value, or explicit device pixels and set its contents to a LinearLayout, whose height
    ; is set to wrap_content.

    ; The following demonstrates the use of an ImageView.  The "image" function creates a proxy to a
    ; ImageView Android view.  This view works similarly to a very basic view except that it has the
    ; keyword parameters "src" and "scaleType" which specify separate foreground content from the
    ; background content.  Explanation of scaleType is given at:
    ; 'https://developer.android.com/reference/android/widget/ImageView.ScaleType.html'

    ; One special capability of the Android Lisp GUI Builder that goes beyond native android is the
    ; ability to set the image source to a URL.  This capability is provided by the Universal Image
    ; Downloader library, https://github.com/nostra13/Android-Universal-Image-Loader

    (progn

        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq image-size 200)

        (setq parent-view
              (scrollview (vertical-layout :width _MATCH_PARENT
                                           :height _WRAP_CONTENT
                                           (vertical-layout :width _WRAP_CONTENT
                                                            :height _WRAP_CONTENT
                                                            :margin-bottom 2
                                                            (image :width image-size
                                                                   :height image-size
                                                                   :padding 3
                                                                   :src "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7e/Saturn_with_auroras.jpg/280px-Saturn_with_auroras.jpg"
                                                                   :scaleType "FIT_XY")
                                                            (horizontal-layout :width _MATCH_PARENT
                                                                               :height _WRAP_CONTENT
                                                                               (image :src (get-drawable-resource-id "ic_thumb_down_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")
                                                                               (image :src (get-drawable-resource-id "ic_thumb_up_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")
                                                                               (image :src (get-drawable-resource-id "ic_file_download_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")))
                                           (vertical-layout :width _WRAP_CONTENT
                                                            :height _WRAP_CONTENT
                                                            :margin-bottom 2
                                                            (image :width image-size
                                                                   :height image-size
                                                                   :src "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b6/Jupiter_gany.jpg/108px-Jupiter_gany.jpg"
                                                                   :scaleType "FIT_XY")
                                                            (horizontal-layout :width _MATCH_PARENT
                                                                               :height _WRAP_CONTENT
                                                                               (image :src (get-drawable-resource-id "ic_thumb_down_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")
                                                                               (image :src (get-drawable-resource-id "ic_thumb_up_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")
                                                                               (image :src (get-drawable-resource-id "ic_file_download_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")))
                                           (vertical-layout :width _WRAP_CONTENT
                                                            :height _WRAP_CONTENT
                                                            :margin-bottom 2
                                                            (image :width image-size
                                                                   :height image-size
                                                                   :src "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Jupiter_family.jpg/120px-Jupiter_family.jpg"
                                                                   :scaleType "FIT_XY")
                                                            (horizontal-layout :width _MATCH_PARENT
                                                                               :height _WRAP_CONTENT
                                                                               (image :src (get-drawable-resource-id "ic_thumb_down_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")
                                                                               (image :src (get-drawable-resource-id "ic_thumb_up_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")
                                                                               (image :src (get-drawable-resource-id "ic_file_download_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")))
                                           (vertical-layout :width _WRAP_CONTENT
                                                            :height _WRAP_CONTENT
                                                            :margin-bottom 2
                                                            (image :width image-size
                                                                   :height image-size
                                                                   :src "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3a/Young_male_chimp.png/120px-Young_male_chimp.png"
                                                                   :scaleType "FIT_XY")
                                                            (horizontal-layout :width _MATCH_PARENT
                                                                               :height _WRAP_CONTENT
                                                                               (image :src (get-drawable-resource-id "ic_thumb_down_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")
                                                                               (image :src (get-drawable-resource-id "ic_thumb_up_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")
                                                                               (image :src (get-drawable-resource-id "ic_file_download_black_24dp")
                                                                                      :scaleType "CENTER_INSIDE")))
                    )
              )
        )
        (set-top-view parent-view))
)

(progn
    ; Lesson 8 - Persisting data
    ; Each page defines its own lexical environment.  Thus, loading and evaluating the expressions on this
    ; tutorial on a different code page defines separate variables than the variables defined on another
    ; code page.  Normally, the data for a code page only exists while the Android Lisp GUI Builder app
    ; is loaded into memory.  Force closing this app or restarting the device loses the data.  To solve
    ; this problem, each page has a dedicated area for persistent lisp variable values, that are saved
    ; as key value pairs.  Ultimately, this data is saved in the built-in Android sqlite database as string
    ; data.  Evaluate and render the following code block, which should display the text with a red background.

    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq background-color-key "lesson-8-background-color")
        (setq default-background-color "red")

        (defun get-background-color ()
            (or (and (has-page-data background-color-key)
                     (get-page-data background-color-key))
                default-background-color))

        (setq parent-view
              (vertical-layout :background-color (get-background-color)
                               (text "Persistent data example")))
        (set-top-view parent-view))

    ; You can use the function, "set-page-data" to assign any serializable lisp object to a string key value.
    ; In general, you can serialize "basic" serializable objects or "composite" serializable objects.
    ; basic serializable objects = strings
    ;                              numbers
    ;                              the "false" value, F
    ;                              a non-closure lambda function
    ; composite serializable objects = lists of basic or composite serializable objects
    ;                                  string hashtables of basic or composite serializable objects
    ;                                  int hastables of basic or composite serializable objects.

    ; You can permanently update the background color used in the example above by calling the following
    ; expression:

    (set-page-data background-color-key "green")

    ; Now, re-evaluating the prior expression for creating the top view, and switching to the render screen,
    ; you should see a background of green.

    ; In order to test that the data is saved permanently, you have to save your changes to this page by
    ; selecting "Save Page" (if you recently added this page to a workspace, you should select "Save All", as
    ; well).  You can exit the app and force quit it to make sure the application is removed from memory.
    ; Upon restarting the app and reloading the page with this tutorial, you should be able to evaluate the
    ; "parent-view" from above and upon rendering it, the background should be green, having accessed the
    ; previously stored color string in the "background-color-key" page variable.  But, again, this variable
    ; value is only accessible from the page that you created it on.

    ; We can use the functions "get-data-value", "set-data-value", "delete-data-value" and "check-data-exists"
    ; to store data globally, independent of any particular page.  Syntatically, they work nearly the same as
    ; "set-page-data", "has-page-data", "get-page-data" and "delete-page-data".  The code below refactors the
    ; example above to use global data:

    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq background-color-key "global-background-color")
        (setq tutoral-context-key "tutorial-context")

        (setq default-background-color "yellow")

        (defun get-background-color ()
            (or (and (check-data-exists background-color-key tutoral-context-key)
                     (get-data-value background-color-key tutoral-context-key))
                default-background-color))

        (setq parent-view
              (vertical-layout :background-color (get-background-color)
                               (text "Global data example")))
        (set-top-view parent-view))

    ; Note that one difference between the page data functions, "set-page-data",
    ; "get-page-data", etc and the global data functions, "get-data-value",
    ; "set-data-value", etc is that the global data functions have a final optional
    ; parameter that specifies a context key.  The purpose of this context key is merely
    ; and additional layer of distinction between different stored values.  Its primary
    ; use is through the functions, "get-all-names" which returns all key names having
    ; a particular context key.  If you don't include the context key, a default one
    ; will be used.

    ; After evaluating and rendering the expression above, and noting that the
    ; background color is yellow, you can now evaluate the expression below.
    ; Evaluate the expression below, which changes the value of the background color.

    (set-data-value background-color-key "#85FDEE" tutoral-context-key)

    ; Note that you don't need to save the current page for the new data key,
    ; "background-color-key", to be set since this data is stored globally, independent
    ; of the current page. Now, you can load the tutorial on a different page or after
    ; restarting the app and upon rendering the code below, you will notice that the
    ; color of the background is set to the value you assigned to the key above.

    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq background-color-key "global-background-color")
        (setq tutoral-context-key "tutorial-context")

        (setq default-background-color "yellow")

        (defun get-background-color ()
            (or (and (check-data-exists background-color-key tutoral-context-key)
                     (get-data-value background-color-key tutoral-context-key))
                default-background-color))

        (setq parent-view
              (vertical-layout :background-color (get-background-color)
                               (text "Global data example")))
        (set-top-view parent-view))

)

(progn
    ; Lesson 9 - Dialogs
    ; Dialog can be used to create small popup windows that focus attention on a specific
    ; input or output modality.  In general, dialogs only require a view with content to
    ; display, usually an EditText widget, whose proxy is created by the "edit" function.

    (progn
        (setq _MATCH_PARENT "match_parent")
        (setq _WRAP_CONTENT "wrap_content")

        (setq default-icon-dimen 60)

        (setq name "")

        (setq background-color "white")

        (defun show-name-input-dialog ()
            (setq dialog-width 200)
            (dialog :dialog-title "Select a name"
                    :positive-text "Ok"
                    :on-positive (progn
                                    (set name
                                         (get-text name-edit))
                                    (set-text name-text name))
                    :cancel-text "Cancel"
                    (vertical-layout :width dialog-width
                                     :height _WRAP_CONTENT
                                     (setq name-edit
                                           (edit name
                                                 :text-size 20
                                                 :width _MATCH_PARENT
                                                 :height _WRAP_CONTENT
                                                 :singleline "true")))))

        (defun show-screen-color-dialog ()
            (setq dialog-width 200)
            (defun update-color ()
                (setq color-name
                      (get-text color-edit))
                (if (or (= (length color-name) 0)
                        (not (signal-block ("INITIAL")
                                            (("INITIAL") (try
                                                            (update-parameters parent-view
                                                                               :background-color color-name)))
                                            ((F) F))))
                    (show-long-toast "Invalid color - try \"red\"")
                    (progn
                        (set background-color
                             color-name)
                        (set-text color-text color-name)
                        (dismiss-dialog my-dialog))))


            (setq my-dialog
                  (dialog :dialog-title "Set Background Color"
                        (vertical-layout :width dialog-width
                                         :height _WRAP_CONTENT
                                         (setq color-edit
                                               (edit ""
                                                     :hint "hex color number or color name"
                                                     :text-size 20
                                                     :width _MATCH_PARENT
                                                     :height _WRAP_CONTENT
                                                     :singleline "true"))
                                         (horizontal-layout :width _MATCH_PARENT
                                                            :height _WRAP_CONTENT
                                                            (button "OK"
                                                                    :width "50%"
                                                                    :on-click (update-color))
                                                            (button "Cancel"
                                                                    :width "50%"
                                                                    :on-click (dismiss-dialog my-dialog)))))))

        (setq parent-view
              (vertical-layout :width _MATCH_PARENT
                               :height _MATCH_PARENT
                               :background-color background-color
                               :child-align "center_horizontal"
                               (text "Set background color and name"
                                     :text-style "bold")
                               (horizontal-layout :width _MATCH_PARENT
                                                  :height _WRAP_CONTENT
                                                  :child-align "center"
                                                  :margin-bottom 10
                                                  (text "Name: "
                                                        :text-style "bold|italic")
                                                  (setq name-text
                                                        (text name
                                                              :width "100%"
                                                              :height _WRAP_CONTENT
                                                              :singleline "true"))
                                                  (image :width default-icon-dimen
                                                         :height default-icon-dimen
                                                         :src (get-drawable-resource-id "ic_open_in_new_black_24dp")
                                                         :scaleType "CENTER_INSIDE"
                                                         :on-click (show-name-input-dialog)))
                               (horizontal-layout :width _MATCH_PARENT
                                                  :height _WRAP_CONTENT
                                                  :child-align "center"
                                                  (text "Background color: "
                                                        :text-style "bold")
                                                  (setq color-text
                                                        (text background-color
                                                              :width "100%"
                                                              :height _WRAP_CONTENT
                                                              :singleline "true"))
                                                  (image :width default-icon-dimen
                                                         :height default-icon-dimen
                                                         :src (get-drawable-resource-id "ic_open_in_new_black_24dp")
                                                         :scaleType "CENTER_INSIDE"
                                                         :on-click (show-screen-color-dialog)))))
        (set-top-view parent-view))


    )

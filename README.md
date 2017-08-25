# Android Lisp GUI Builder
## Introduction
This repository contains sample projects that demonstrate the versatility of a Lisp interpreter that I created for Java. It also contains some technical design UML diagrams that I will be updating (probably slowly) over time.  The main projects and installation instructions are described below.  I will also update the repository Wiki with technical information on the Lisp interpreter and a usage guide for the sample projects.
## Sample Projects Included
###	AndroidLispGUIBuilder 
This Android application project is the central showcase for the Lisp interpreter.  Adding more features, polish and sample programs to this project will be my main focus right now.  Eventually, I'd like this to be a self-contained Android application for teaching programming.  Currently, it has a code editor which also functions as a Lisp REPL.  From the code editor, one can save and load Lisp source files to device storage or to Dropbox if you have a developer app key.  The code editor is also used to build Android View hiearchies that can be added to a Fragment or ViewGroup so that one can define a full graphical user interface using Lisp.  Most View attributes that can be defined in xml can also be defined in Lisp, with functions standing in for xml elements, somewhat similiar to ReactJs.  In the code editor, one can create View hierarchies, set View attributes like backgrounds and text colors and even define event handlers all with Lisp.  Even if you just want to quickly mock up a user interface that will eventually be implemented in xml or constructed programmatically, the AndroidLispGUIBuilder can be used to put together a quick interactive wireframe of how a UI component will work.  

In line with my goal for AndroidLispGUIBuilder as a teaching tool, I also added some special Lisp functions that allow you to program a [Lego Mindstorms  NXT](https://shop.lego.com/en-US/LEGO-MINDSTORMS-NXT-2-0-8547) using bluetooth.  This way, one can augment a lesson on creating event-based graphical user interfaces with event-based robotic control.  I implemented basic Lisp wrapper functions for most of the NXT Bluetooth Direct Command API.  Eventually, I will add support for the [Mindstorms EV3](https://www.lego.com/en-us/mindstorms/products/mindstorms-ev3-31313).

### AndroidTools
This is an Android library project which contains the core api for the Android Lisp interpreter.  It can be used if you just want to build a View hierarchy using Lisp but don't care for the frontend provided by the AndroidLispGUIBuilder app.  In addition to the  main Lisp api, which is contained in the **com.evolved.automata.lisp** and **com.evolved.automata.android.lisp** packages, there are some utility classes which wrap standard Android functions and some custom Views that I have found useful.  Warning!  Other than the code specifically referenced by the "*.lisp" packages, most of the other classes are old, probably buggy and are subject to removal or changes with future versions of this repository so use them at your own risk!

### LispDesktopConsole
This is a barebones sample project that implements a simple REPL for the Lisp interpreter in Java.  If you want to get a general sense for how the interpreter works and how it can be used, I would look at the admittedly sparse unit tests that are defined in this project.  The core Lisp interpreter, which is referenced by this project, is compatible with a JRE 1.7 and above.  There's not much to say about the project itself.  If you just want a version of Lisp on Java that is easy to extend with your own built-in functions, then this is the project to start with.  I was able to extend the language pretty easily to control an Aldebaran NAO robot and the Sony AIBO by adding custom Lisp functions that wrapped the vendor supplied API.  Note that there are some significant differences between the version of Lisp here and Common Lisp.  Because much of my own personal use of the Lisp interpreter has been for robotics, I've added a lot of special functions that have no analog in Common Lisp.  Also, due to mistakes and forgetfulness, some of my functions are named differently than their Common Lisp equivalents.  Periodically, I will update the Wiki with technical information on the behavior of the Lisp interpreter as well as a function reference.  Ultimately, I want to be able to support a rich Lisp API on Java and for the desktop version of the Lisp interpreter, I want to eventually allow one to create simple Swing applications with Lisp.  

NOTE! There are classes in the package "com.evolved.automata.nn" package that reference native C functions for neural networks.  The native libraries are included in the path Android-Lisp/projects/android_studio/JavaLisp/LispDesktopConsole/libs/native/.  Currently, I only have precompiled libraries for MacOS but the source files for the native libraries are contained in /Users/Evolved8/ALGB2/Android-Lisp/projects/android_studio/JavaLisp/LispDesktopConsole/libs/source so it should be easy to recompile them for Windows or Linux systems.  Probably eventually I will build x86 libraries for Windows and Linux and add them to the MacOS libraries that are there.

### Dropbox Configuration
If you want your users to be able to load source files into the code editor of AndroidLispGUIBuilder from their Dropbox accounts, you'll need to sign up for and get an app key from Dropbox.  Your application will need at least "App Folder" level of permissions.  Once you have an app key, there are two string resource values, "enc_dropbox_app_key" and "manifest_key", that you have to set in the [dropbox_config.xml](https://github.com/hexagonc/Android-Lisp/blob/starting_v2/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/src/main/res/values/dropbox_config.xml).  The first string resource, "enc_dropbox_app_key" is the literal value of the Dropbox app key.  You should set "manifest_key" to the literal value of your app key prefixed by "db-".  This is used for the OAuth2 authentication provided by the Dropbox SDK.  The other Dropbox resource string, "dropbox_app_client_name", is the name of your application as defined in the Dropbox developer console.

## Program Behavior
### Basics
AndroidLispGUIBuilder is organized into "CodePages".  A CodePage is defined by the source code that you want to execute as well as a [lexical environment](http://www.lispworks.com/documentation/HyperSpec/Body/03_aac.htm) that is specific to that Page.  The lexical environment of a CodePage is nested in a global environment that is shared by all CodePages.  I am still tweaking the CodePage user interface but right now the design is optimized for evaluating and navigating blocks of Lisp code, called [S-expressions](https://en.wikipedia.org/wiki/S-expression).  Each time you move the cursor in the source code, the system automatically highlights the innermost parent s-expression.  The user interface makes it easy to navigate to the parent expression, first child expression, next or previous sibling s-expressions.  Thus, it should be fairly simple to drill down into any particular portion of the source code in order to evaluate the selected block within the lexical scope of the CodePage.  For example, if you type into a CodePage:
```
(+ 12 454)
(* 12 (/ 78 23))
```
If you put the cursor before the space after the "+" in _(+ 12 454)_ then the system highlights the whole expression, _(+ 12 454)_.  If you press the "Eval" button then the system will display 466.0 in the "Results" window.  Given _(+ 12 454)_ is selected, there is a button (![next button](https://raw.githubusercontent.com/hexagonc/Android-Lisp/starting_v2/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/src/main/res/drawable-xhdpi/ic_skip_next_black_24dp.png) or ![prev button](https://raw.githubusercontent.com/hexagonc/Android-Lisp/starting_v2/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/src/main/res/drawable-xhdpi/ic_skip_previous_black_24dp.png)) that you can use to move to its sibling expression _(* 12 (/ 78 23))_.  Having done this, you can evaluate this expression, yielding about 40.70.  Pressing ![down](https://raw.githubusercontent.com/hexagonc/Android-Lisp/starting_v2/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/src/main/res/drawable-xhdpi/ic_expand_more_black_24dp.png) moves to the first child in the list, which would be "*".  Having done this, you could press ![next button](https://raw.githubusercontent.com/hexagonc/Android-Lisp/starting_v2/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/src/main/res/drawable-xhdpi/ic_skip_next_black_24dp.png) twice, to navigate to the second sibling of "*", which would be expression _(/ 78 23)_.  You could evaluate this to yield about 3.39.  Pressing ![up](https://raw.githubusercontent.com/hexagonc/Android-Lisp/starting_v2/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/src/main/res/drawable-xhdpi/ic_expand_less_black_24dp.png) moves to the immediate parent list, which would be _(* 12 (/ 78 23))_. All the code in CodePage is contained in an implicit "progn" function so navigating "up" from a seemingly top level expression, like _(* 12 (/ 78 23))_, selects all the code on the CodePage.  If you press "Eval" now, all the code will be evaluated within the lexical scope of the CodePage in sequence, until an error is encountered.  This is demonstrated in the following example:

```
(setq x (+ 34 100))
(setq value (format "x is %1$s" (* x 10)))
```

Regardless of where you put the cursor initially, if you press ![up](https://raw.githubusercontent.com/hexagonc/Android-Lisp/starting_v2/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/src/main/res/drawable-xhdpi/ic_expand_less_black_24dp.png) enough times, you can select all the code on the page.  Pressing "Eval" evaluates _(setq x (+ 34 100))_, which binds the value of _(+ 34 100)_ to the variable _x_.  _(setq value (format "x is %1$s" (* x 10)))_ uses this variable to define another variable _value_.  These variables exist only on this CodePage.  If you navigate to another CodePage, you will not have access to the binding of any other CodePage.  If you want to share variables between pages, you have to bind variables in the global lexical environment, via the _global_ function.  For example, if you evaluate a CodePage with the source code:
```
(global
    (setq x 1000)
    (setq value (format "x is %1$s" (* x 10))))
```
then _x_ is a global variable that is visible to all CodePages.  However, variables defined on a CodePage will over-shadow global variables of the same name.  Note, however, that no matter how deeply nested the expression you evaluate on a CodePage, the lexical environment it is evaluated in is always the top level CodePage environment.  Thus, even if you have
```
(setq x 10)
(for x
     (1 2 3 4)
     (log-info "Done")
     (log-info (concat "Value: " x)))
```

If you evaluate this whole code list, _x_ will be bound to 10 in the CodePage environment, but _x_ in the "for" loop is bound to values in a child lexical environment, specific to the "for" loop.  If you select and evaluate the expression _(concat "Value: " x)_, the value returned will be 10, even though the last value bound to _x_ in the "for" loop was 4.

### Simple User-Interfaces
There are Lisp functions for creating most of the basic Android widget classes.  The overall paradigm is somewhat similiar to [ReactJS](https://facebook.github.io/react/) in that the user-interface is generated in a functional manner.  Instead of defining user-interfaces using xml, with the Android Lisp GUI Builder, the user interface is defined by nested function calls.  For example, an xml approach to building a simple colored block with a text label in the middle would be done something like this:
```
<LinearLayout android:orientation="vertical"
              android:layout_width="match_parent"
              android:layout_height="200dp"
              android:background="#0F0"
              >
    <TextView android:text = "Hello world"

        android:layout_width = "wrap_content"
        android:layout_height = "wrap_content"/>


</LinearLayout>
```
The Lisp approach to creating this same construction in the view hierarchy would be done with something like the following code:
```
(vertical-layout :width "match_parent"
                 :height 200
                 :background-color "green"
                 (text "Hello world"))
```
If you select this code and press "Eval", the function _vertical-layout_ is evaluated which results in the function _text_ also being evaluated.  Functions like _vertical-layout_ and _text_ return a datastructure that acts as a [proxy](https://github.com/hexagonc/Android-Lisp/blob/starting_v2/projects/android_studio/JavaLisp/AndroidTools/src/main/java/com/evolved/automata/android/lisp/views/ViewProxy.java) for an Android View.  Depending on the widget they represent, there are predefined Lisp keyword arguments that correspond to layout and View attributes.  So in this case, a vertical LinearLayout of height 200 dp (numeric values for length attributes are interpretted as in dp), width "match_parent" and background-color "green" contains a TextView widget.  The function _text_ returns an object which acts as a wrapper for a TextView and for this function, any string arguments are interpreted as the label for the TextView.  If you don't specify the width or height of a View, then the width and height both default to "wrap_content".  The key purpose of the Android Lisp GUI Builder is to allow you to create simple user interfaces using Lisp for quick prototypes or mockups and view them on your smartphone or tablet.  Evaluating the code above returns an instance of a [ViewProxy](https://github.com/hexagonc/Android-Lisp/blob/starting_v2/projects/android_studio/JavaLisp/AndroidTools/src/main/java/com/evolved/automata/android/lisp/views/ViewProxy.java).  In order to actually render the view on your phone, you have to save the ViewProxy value to a specially named variable in the lexical environment of the CodePage.  You should never have to modify this variable directly, since it is used by the system to find the top-level View or ViewGroup for the CodePage.  If it is set incorrectly, your view will not inflate correctly.  Instead, you use the function _(set-top-view ...)_ in order to define the top level view.  Thus, in order to actually view the "LinearLayout" view defined above, you would to evaluate:
```
(set-top-view (vertical-layout :width "match_parent"
                               :height 200
                               :background-color "green"
                               (text "Hello world")))
```
If this evaluates without error, you can then select "To Render Screen" from the main application menu.  This switches the CodePage from code editor mode to render mode.  In render mode, the app creates actual Android native Views corresponding to the specifications of the Lisp ViewProxy and attaches it to a Android Fragment associated with your CodePage.  Each CodePage has its own UI screen that corresponds to the ViewProxy that was set as the top view.  All variables and functions defined in the lexical environment for a CodePage exist for the duration of the app itself.  There are special functions, such as _get-page-data_, _set-page-data_ and _has-page-data_ to save and manage simple data types (such as numbers, strings, lists or hashtables of the same) to a persistent storage that is private to the page.

Where the Android Lisp GUI Builder differs from the xml way of building user interfaces is that ViewProxys can be saved to variables and constructed programmatically.  Here is a very simple example showing a simple bar graph:
```lisp
(setq height 300)

(setq bar-function
      '(* x x))

(setq bar-width 20)
(setq bar-color "black")
(setq inter-bar-margin 5)

(setq x-values
      (list -4 -3 -2 -1 0 1 2 3 4))

(setq max-function-value
      (apply 'max
             (mapcar x
                     x-values
                     (eval bar-function))))


(setq top-view
      (vertical-layout 
          :width "match_parent"
          :height height
          :padding 10
          (text :width "wrap_content"
                :height "wrap_content"
                :margin-bottom 10
                (concat "Value of graphing: " (string bar-function)))
          (horizontal-scrollview :width "match_parent"
                             :height "match_parent"
                             (horizontal-layout :width "wrap_content"
                                                :height "match_parent"
                                                :child-align "bottom"  
                                                (mapcar (x i)
                                                         x-values
                                                         (solid :background-color bar-color
                                                                :width bar-width
                                                                :margin-left (if (> i 0)
                                                                                 inter-bar-margin
                                                                                 0)
                                                                :height (* height
                                                                           (/ (eval bar-function)
                                                                              max-function-value))))))))
(set-top-view top-view)
```
After running AndroidLispGUIBuilder, you can type this code directly into the code editor (not recommended unless you have a good Android keyboard) or you can save it to a file on your computer and load that file on your smartphone if you configured the Android Lisp GUI Builder project to access your Dropbox account.  Eventually, I will add support for loading source code from other cloud storage venders or removable storage on your smartphone.  If using Dropbox, you can link your CodePage to a Dropbox file by setting the Page Properties from the main application menu.  From there, you can also set the name of your CodePage.


## Areas for Improvement
Basic user interface polish is an overall area for improvement with too many areas to mention here.  Below are the major issues that I hope to fix within the next few months.
* Add a predefined workspace with sample CodePages demonstrating much more complex user-interfaces than shown here.
* The Lisp s-expression highlighting algorithm needs to be cleaned up a bit.  Sometimes it doesn't select the right block of code, usually because the code in the code editor gets out of synch with the [data-structure](https://github.com/hexagonc/Android-Lisp/blob/starting_v2/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/src/main/java/com/evolved/automata/android/lisp/guibuilder/LispCodeEditorParseContext.java) that is used to model the code.  The workaround for incorrect code selection is to move the cursor by touching the screen.  Each time the cursor moves by touch, the system resynchs your cursor position in the source code with the internal model of the source code.
* Remove the ProgressDialog that appears when the app rebuilds the source code model.  This should be replaced by a hour-glass like status icon.  The problem with the ProgressDialog is that it can get stuck on if you switch to another CodePage while it is present.
* Synching source code up from the CodePage back to Dropbox is not supported at the moment.  This is very simple but I just haven't figured out the user interface design for allowing this.
* Need a way to clear all and selected source code from a CodePage
* Need nicer code editing options like copy, paste, cut, and undo
* Want to add support for controlling Lego Mindstorms EV3 from Lisp
* Need to be able to clear the application status log
* Add an application Settings screen.  Should be able to backup and restore all app data to some external file.
* Add more polish to the [File Chooser Dialog](https://github.com/hexagonc/Android-Lisp/blob/starting_v2/projects/android_studio/JavaLisp/AndroidLispGuiBuilder/src/main/java/com/evolved/automata/android/lisp/guibuilder/FileChooserDialog.java), especially allowing a custom title for the dialog (currently the title asks the user to select a file to load even when the actual purpose of the dialog was to allow the user to create the name of a brand new file to saved to).  Also, need to support a custom description of the ProgressDialog that can appear when accessing a directory takes time (such as it does for Dropbox navigation).
* The File Chooser Dialog code should probably be moved to the AndroidTools project anyway, since it is general enough to be of use to nearly any Android program that handles files.
* Prevent the user from loading binary files into the code editor
* Add support for landscape mode and changes in screen orientation.  Currently, the app only works in portrait mode.  This is to allow as much space as possible for the code editor when shown on a small smartphone screen.  For tablets, it makes sense to allow the user to run the app in landscape mode.  In order to do this, however, I'd have to change the policy around generating Views with Lisp.  The current policy is that the last Lisp expression evaluated in the code editor determines the View that is displayed in the "Render" tab.  Probably, users will have to return a list of views, one for portrait mode and one for landscape in order to make orientation changes work properly.

## License
All of this code is MIT licensed, which is described [here](license.txt).
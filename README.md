# Android Lisp GUI Builder
## Introduction
This repository contains sample projects that demonstrate the versatility of a Lisp interpreter that I created for Java. It also contains some technical design UML diagrams that I will be updating (probably slowly) over time.  The main projects and installation instructions are described below.  I will also update the repository Wiki with technical information on the Lisp interpreter and a usage guide for the sample projects.
## Sample Projects Included
###	AndroidLispGUIBuilder 
This Android application project is the central showcase for the Lisp interpreter.  Adding more features, polish and sample programs to this project will be my main focus right now.  Eventually, I'd like this to be a self-contained Android application for teaching programming.  Currently, it has a code editor which also functions as a Lisp REPL.  From the code editor, one can save and load Lisp source files to device storage or to Dropbox if you have a developer app key.  Because editing code is difficult with most Android soft keyboards, I added the ability to create and insert code templates into the editor.  The code editor is also used to build Android View hiearchies that can be added to a Fragment or ViewGroup so that one can define a full graphical user interface using Lisp.  Most View attributes that can be defined in xml can also be defined in Lisp, with functions standing in for xml elements.  In the code editor, opne can create View hierarchies, set View attributes like backgrounds and text colors and even define event handlers all with Lisp.  Even if you just want to quickly mock up a user interface that will eventually be implemented in xml or constructed programmatically, the AndroidLispGUIBuilder can be used to put together a quick interactive wireframe of how a UI component will work.  

In line with my goal for AndroidLispGUIBuilder as a teaching tool, I also added some special Lisp functions that allow you to program a Lego Mindstorms  NXT using bluetooth.  This way, one can augment a lesson on creating event-based graphical user interfaces with event-based robotic control.  There is a pre-defined code template called _lego mindstorms sample_ which shows how to create a graphical user interface for selecting an NXT from a set of paired bluetooth devices, connecting to that NXT, and controlling a Lego rover robot.  This code template contains a number of useful examples of using Lisp to define the event handlers for an Android Widget. There are other pre-defined code templates that further demonstrate what can be done with the Android Lisp GUI Builder.  Again, Lisp support for built-in Android functionality is a work in progress so there may be some features missing.

### AndroidTools
This is an Android library project which contains the core api for the Android Lisp interpreter.  It can be used if you just want to build a View hierarchy using Lisp but don't care for the frontend provided by the AndroidLispGUIBuilder app.  In addition to the  main Lisp api, which is contained in the **com.evolved.automata.lisp** and **com.evolved.automata.android.lisp** packages, there are some utility classes which wrap standard Android functions and some custom Views that I have found useful.  Warning!  Other than the code specifically referenced by the "*.lisp" packages, most of the other classes are old, probably buggy and are subject to removal or changes with future versions of this repository so use them at your own risk!

### DesktopLispTester
This is a barebones sample project that implements a simple REPL for the Lisp interpreter in Java.  If you want to get a general sense for how the interpreter works and how it can be used, I would look at the admittedly sparse unit tests that are defined in this project.  The core Lisp interpreter, which is referenced by this project, is compatible with a JRE 1.6 and above.  It is probably easy to back-port to JRE 1.5 as well although I haven't tested this.  The Eclipse project itself is configured for JRE 1.6.  There's not much to say about the project itself.  If you just want a version of Lisp on Java that is easy to extend with your own built-in functions, then this is the project to start with.  I was able to extend the language pretty easily to control an Aldebaran NAO robot and the Sony AIBO by adding custom Lisp functions that wrapped the vendor supplied SDKs.  Note that there are some significant differences between the version of Lisp here and Common Lisp.  Because much of my own personal use of the Lisp interpreter has been for robotics, I've added a lot of special functions that have no analog in Common Lisp.  Also, due to mistakes and forgetfulness, some of my functions are named differently than their Common Lisp equivalents.  Periodically, I will update the Wiki with technical information on the behavior of the Lisp interpreter as well as a function reference.  Ultimately, I want to be able to support a rich Lisp API on Java and for the desktop version of the Lisp interpreter, I want to eventually allow one to create simple Swing applications with Lisp.  

##Installing the Android Lisp GUI Builder App from Eclipse (Android Studio instructions to come later):
Clone the repository to a directory, **_X_**.  The Eclipse projects are stored in the directory **X/Android-Lisp/projects/eclipse**.  Start Eclipse with this as the workspace folder.  Then go to "File -> Import... -> General -> Existing projects into workspace".
Select **X/Android-Lisp/projects/eclipse** as the base folder and check all projects within.   The repository is organized so that the Eclipse projects can automatically find shared source folders; if you use a folder other than **X/Android-Lisp/projects/eclipse** for your workspace then you'll have to individually update the build paths to link the appropriate source folders.  These projects use Apache Ivy to install third party libraries from Maven Repository.  After installing (if necessary) the IvyDE Eclipse plugin from http://www.apache.org/dist/ant/ivyde/updatesite, right click the ivy.xml file in the package explorer for AndroidLispGUIBuilder and click "Add Ivy Library" from the bottom of the context menu.  If this opens a dialog screen for "IvyDE Managed Libraries", configure the Ivy libraries as follows:
* Go to the "Classpath" tab and check the "Enable project specific settings" checkbox. 
* Make sure that "retrieved artifacts" is checked under "Build the classpath with".  The retrieved pattern should be **libs/[artifact]-[revision].[ext]**.  This is because Android jar dependencies must be stored in the _libs_ folder under your project.  
* Below this, make sure that 'Types' is set to at least 'jar, bundle'.  
* Click "Finish".

Configure the Ivy libraries for the AndroidTools project similarly.  You'll also have to manually create a _src_ folder (such as **X/Android-Lisp/projects/eclipse/AndroidTools/src**) on the filesystem for the AndroidTools project  since git cannot retrieve an empty folder from a repository.  (The _src_ folder is empty because the main code for AndroidTools is linked from an external source, in the directory 
**X/Android-Lisp/common/src/main**.)  Due to a limitation of Android, (or Eclipse, not sure which is responsible), you can only attach sources for an Android library project from one source folder so I chose to base the attached sources on the external linked sources rather than any sources defined in the AndroidTools project itself.

The Android GUI Builder code should run on any Android device with SDK 14 (Ice Cream Sandwich) or greater.  However, if you are building in Eclipse with a JDK of 1.7 or greater than you'll need to set the Android project build target in Eclipse to SDK 19 or greater.  

## Dropbox Configuration
If you want your users to be able to load source files into the code editor of AndroidLispGUIBuilder from their Dropbox accounts, you'll need to sign up for and get an app Key from Dropbox.  Your application will need at least "App Folder" level of permissions.  Once you have an app key, you need to store an encrypted version of the app key in the [string.xml](https://github.com/hexagonc/Android-Lisp/blob/master/projects/eclipse/AndroidLispGUIBuilder/res/values/strings.xml) resource values for AndroidLispGUIBuilder.  This string resource is called _enc_dropbox_app_key_.  You'll also need to set the string resource value for _dropbox_app_client_name_ which represents your own unique name for the AndroidLispGUIBuilder application.  

The encryption that is used to protect the app key is not meant to be particularly robust security.  It is certainly useless if you are distributing code based on this repository but don't use Proguard or some other means of obfuscation.  What little security that is provided by this encryption is mostly due to the fact that the method itself is not expected to be used for this purpose (you might be amused upon learning what it is based off of!).  Basically, the app key is secured by a symmetric encryption algorithm, so that once you hardcode the shared secret in the file, [GuiBuilderConfiguration.java](https://github.com/hexagonc/Android-Lisp/blob/master/projects/eclipse/AndroidLispGUIBuilder/src/com/evolved/automata/android/lisp/guibuilder/GuiBuilderConfiguration.java), the same function that encrypts the plaintext app key can also decrypt the encrypted key.  If you aren't too bothered by the risk of someone decompiling your code and getting your app key, at a minimum, you should probably change the value of the field _GuiBuilderConfiguration.seed_ to some random value and mix up the letters that are used for the fields, _d1_, _d2_, _d3_ and _d4_.  Once you do that, you can get the encrypted value of your app key by first setting the resource string, _enc_dropbox_app_key_, to the unencrypted app key.  Then you run the app, AndroidLispGUIBuilder, in debug mode and inspect the return value from the method, _GuiBuilderConfiguration.encryptDecrypt()_.  This will provide you the encrypted app key.  Now, all that is left is to set the resource value _enc_dropbox_app_key_ to this.  Once the resource strings _enc_dropbox_app_key_ and _dropbox_app_client_name_ are defined, your users will be able to authenticate themselves to Dropbox and authorize your app to access their Dropbox files.  The option to load files from Dropbox will not be available on the code editor tab until you have set an app key.

##Program Behavior
AndroidLispGUIBuilder is organized as a simple tab activity.  The initial tab you'll see, labeled "Console", contains a code editor, REPL and code management controls for saving and loading code.  The most basic usage is to simply type in a Lisp expression in the code editor and press "Evaluate".  The result will appear in the bottom console window under "Console Output".  Multiple Lisp command expressions are executed sequentially although only the value of the last executed function will be printed to the "Console Output".  For example, if you type into the code editor:
```
(+ 12 454)
(* 12 (/ 78 23))
```
the result of pressing "Evaluate" will be the value of evaluating `(* 12 (/ 78 23))` (about 40.69).  The code editor represents the top level "environment", which is a space for variables and function definitions.  [Environments](https://github.com/hexagonc/Android-Lisp/blob/master/common/src/main/com/evolved/automata/lisp/Environment.java) can be thought of like a "stack frame" or a "lexical scope" and can be nested to define functions and closures.  So if you execute multiple expressions, some of which have side-effects, like:
```
(setq x (+ 34 100))
(setq value (format "x is %1$s" (* x 10)))
```
then _x_ and _value_ now exist as global variables.  These variables will persist in the top level "environment" even if you press "Clear", which merely erases the code editor.  If you clear the code editor and enter only _x_, and then press "Evaluate", the result will be 134.  This value will persist so long as the AndroidLispGUIBuilder application is running.  View hierarchies can be created with AndroidLispGUIBuilder and structured similarly to how you would construct them with Android xml.  For example, having evaluated the commands above, you could clear the code editor and enter:
```
(vertical-layout :width "match_parent"
                 :height 200
                 :background-color "green"
                 (text value))
```
Upon pressing "Evaluate", the function _vertical-layout_ is evaluated which results in the function _text_ also being evaluated.  Functions like _vertical-layout_ and _text_ return a datastructure that acts as a [proxy](https://github.com/hexagonc/Android-Lisp/blob/master/common/src/main/com/evolved/automata/android/lisp/views/ViewProxy.java) for an Android View.  Depending on the widget they represent, there are predefined Lisp keyword arguments that correspond to layout and View attributes.  So in this case, a vertical LinearLayout of height 200 dp (numeric values for length attributes are interpretted as in dp), width "match_parent" and background-color "green" contains a TextView widget.  The function _text_ returns an object which acts as a wrapper for a TextView and for this function, any string arguments are interpreted as the label for the TextView.  If you don't specify the width or height of a View, then the width and height both default to "wrap_content".  IMPORTANT!  **If the final value that is evaluated in the code editor returns a proxy for a View (an object that extends [ViewProxy](https://github.com/hexagonc/Android-Lisp/blob/master/common/src/main/com/evolved/automata/android/lisp/views/ViewProxy.java)), then the proxy can be attached to AndroidLispGUIBuilder's view hierarchy and rendered in the "Render" tab.**  This is done automatically by pressing the "Render" buttom.  If the final Lisp command evaluated in the code editor is not an object that wraps a View then pressing "Render" has no effect.

The overall purpose of AndroidLispGUIBuilder is to allow you to build simple user interfaces with the ease of inflating an Android layout xml file but with less verbosity and with the power of looping, variables, functions and conditionals.  Here is a very example of showing a simple bar graph:
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
                                                                        max-function-value)))))))
```
The code above represents multiple Lisp function commands that are executed sequentially.  In order to render the View defined by this, the last value evaluated in the code editor needs to wrap an Android View, so that it can be attached to the Window.  After running AndroidLispGUIBuilder, you can type this code directly into the code editor (not recommended unless you have a good Android keyboard) or you can save it to a file on your computer and move that file directly to the device application storage folder (located at __/Android/data/com.evolved.automata.lisp.guibuilder__) using the [Android File Transfer](https://www.android.com/filetransfer/) program.  If you configured Dropbox in your version of AndroidLispGUIBuilder, users would also be able to save the code above in their Dropbox folder, **/Apps/{name you gave the app when registering with Dropbox}**, and load the code into the code editor that way.  

Once it is in the code editor you can evaluate the code.  After evaluating the code, the output window, under "Console Output", will return a string representation of the last command evaluated, which should be something like: __com.evolved.automata.android.lisp.views.LinearLayoutViewProxy@####__.  If you now press "Render" the app will switch you to the "Render" application tab where you can see the widget on your screen.  This is just a simple example of combining imperative with a declarative programming style to specify a user interface dynamically.  Having rendered this view, you can switch back to the "Console" tab and modify the source code further, such as changing the bar colors and spacing.  Background colors can use the built-in string mnemonics like "red", "black" or can be string hexadecimal numbers, as described [here](http://developer.android.com/reference/android/graphics/Color.html#parseColor%28java.lang.String%29).  You can iterative the design as many times as you want until you get something you are satisfied with (or there is a crash).  The code above is a pretty good example of how easy it could be to mock up a quick user interface that you want to test using Lisp. 

There are several pre-defined code templates that demonstrate more sophisticated user interfaces that can be built.  Code templates are generally meant to represent reusable snippets that are put together to form a larger body of code.  However, being samples that demonstrate the capability of the Lisp interpreter and runtime environment, the pre-defined code templates are meant to be executed as standalone programs in the code editor (make sure "replace editor contents" is checked before inserting these code templates).  The current sample code templates include:
* A program which demonstrates how to build the user interface for a simple tic-tac-toe game using lisp functions
* A program that not only builds the ui but which incorporates a very stupid artificial intelligence for playing tic-tac-toe and keeping track of scores
* A program which demonstrates simple text to speech and speech to text
* A program that allows the user to connect to a Lego Mindstorms NXT robot, configure it and control it with bluetooth
* Two more test code templates that can be evaluated after the others and are really just demonstrations of basic user interface concepts in Lisp.  These will change depending on new features that I am testing at the time.  These may depend on global variables already being set so I don't recommend just running these unless you understand what they are doing, which is generally not very difficult.  They will probably disappear in later versions of the app.

Most of these sample templates require little additional explanation except for the code template, "robot control samples", which should be executed after you evaluate the code template "lego mindstorms sample", since the latter is used to configure the Lego Mindstorms NXTs.  

## Known Bugs and Areas for Improvement
Improvements and bugs are broken up by project.
### AndroidLispGUIBuilder
* There is a checkbox on the code editor page labeled, "reset environment".  It is intended to be a flag that clears all global variables after loading a code template or source file.  This button currently doesn't do anything at the moment.  I may move this somewhere else, most likely the overflow menu.
* Saving the code editor to Dropbox doesn't work quite right at the moment.  The Dropbox upload button only saves the code editor to Dropbox if the current contents was the most recently downloaded from Dropbox.  Thus, you can't easily save the code editor contents to a different Dropbox file (it is possible but very convoluted).
* The file management section of the "Console" tab takes way too much space.  I probably need to make this collapsible or move it somewhere else entirely.  The obvious candidate is the overflow menu but I like having file management, especially the code templates, easily accessible.
* Want to add support for controlling Lego Mindstorms EV3 from Lisp
* Add an application Settings screen, particularly so that the user can select whether they want the pre-defined sample code templates to appear in the code template select dialog.
* Improve better app state logging.  The [AppStateManager](https://github.com/hexagonc/Android-Lisp/blob/master/common/src/main/com/evolved/automata/android/AppStateManager.java) seemed like a good idea at the time but I haven't put it to good enough use yet.
* Improve general application logging support.  This is purely for diagnostics and is tied to the previous item about app state logging.  Basically, the plan is to expose some of the application logging as a Content Provider and create a separate Android application which can view the logs after a crash on the device itself without having to connect and debug the app with ADB.
* Improve basic Dropbox handling, especially:
  * Update to the more recent Dropbox client api when it comes out of beta.  Currently I'm using version 1 because version 2 appears to be in beta.
  * Improve the layout of the Dropbox authorization dialog.  There is a problem where the soft keyboard can obscure the password field in the authentication page.  This is partly a result of the decision to render the authentication page in a dialog instead of in a whole activity.  I may revisit that decision if this becomes too much of a problem.
  * Add better handling of version conflicts with Dropbox (which can happen when you try to upload a file to Dropbox that has changed since you last downloaded it)
  * Improve handling when Dropbox is unavailable, such as saving a cached file
* Add more polish to the File Chooser Dialog, especially allowing a custom title for the dialog (currently the title asks the user to select a file to load even when the actual purpose of the dialog was to allow the user to create the name of a brand new file to saved to).  Also, need to support a custom description of the ProgressDialog that can appear when accessing a directory takes time (such as it does for Dropbox navigation).
* The File Chooser Dialog code should probably be moved to the AndroidTools project anyway, since it is general enough to be of use to nearly any Android program that handles files.
* Prevent attempting to load binary files into the code editor
* Need to display a notification after resyncing the code editor text back to Dropbox
* Figure out a way of handling horizontal scrolling within the code editor better.  This is a priority since strong indentation is an essential aspect of code legibility with Lisp.
* Fix error (and sometimes crash) that occurs after loading files from local storage.
* Figure out a more flexible way of handling files stored on sdcards.  [This is partly the fault of Android](http://www.androidpolice.com/2014/02/17/external-blues-google-has-brought-big-changes-to-sd-cards-in-kitkat-and-even-samsung-may-be-implementing-them/).  There were reported issues with apps not being able to access sd cards with KitKat so the current implementation has a fairly conservative policy of only allowing access to the apps storage area, __/Android/data/com.evolved.automata.lisp.guibuilder__.  I'll have to update this functionality once I read the latest policy from Google about writing to sd cards.  I know that it has changed as of Android 5.0 but I don't know the details yet.
* Add support for landscape mode and changes in screen orientation.  Currently, the app only works in portrait mode.  This is to allow as much space as possible for the code editor when shown on a small smartphone screen.  For tablets, it makes sense to allow the user to run the app in landscape mode.  In order to do this, however, I'd have to change the policy around generating Views with Lisp.  The current policy is that the last Lisp expression evaluated in the code editor determines the View that is displayed in the "Render" tab.  Probably, users will have to return a list of views, one for portrait mode and one for landscape in order to make orientation changes work properly.


### AndroidTools
* Add Lisp functions for accessing an application's shared preferences
* Add a Lisp Sqlite api so that users can save program data using SQL
* Add Lisp support for ListView
* Improve the support for __layout_weight__ for the Lisp equivalents to LinearLayout.  Currently, the Lisp LinearLayout proxies don't properly (implementation is buggy and I haven't looked at it in months) support having a layout weight of 1, so that child Views are distributed evenly along a parent's length.  Instead, you have to specifically set the percentage of the LinearLayout's length to allocate for each child.
* Allow users to set ids and tags for Views created by Lisp
* Support more layout attributes for the RelayoutLayout Lisp wrapper, https://github.com/hexagonc/Android-Lisp/blob/master/common/src/main/com/evolved/automata/android/lisp/views/RelativeLayoutViewProxy.java
* Add a Lisp function for accessing Application resources

### DesktopLispTester
* Add more regression unit tests for the core Lisp api.  This will be needed before I can make some of the more fundamental improvements to the Lisp interpreter itself, such as adding argument caching for better performance.
* Add the ability to enable argument caching within a code block.  This should significantly improve performance for macros as well as any code within a loop.  On the other hand, running code will take up more memory and there may be edge cases where the value of a lambda function doesn't get updated within a block.
* Improve support for parallel execution of code
* Add more functions to the core api that seem like they could be of general use, such as more trigonometric functions and a few more string handling functions.
* Add desktop specific Lisp functions for creating Java Swing applications.  This will be an effort similar to the one I did to allow creating Android Views.


##Future
This repository represents an ongoing project that I will be constantly adding more features to.  Along with the issues mentioned above, the immediate need is to fix the worst bugs and continue refining the user interface for AndroidLispGUIBuilder.  I plan on continuing to update the sample code templates in the AndroidLispGUIBuilder, especially the tic-tac-toe program which has an embarrasingly stupid AI.  Improving the overall repository documentation is also on the agenda for the future.  Periodically check the wiki in case I start updating it with additional documentation.

As mentioned above, I will eventually port the projects to Android Studio, since I believe Android Studio is much better behaved in a lot of ways than Eclipse (being also the officially 
supported IDE for Android).  Also, I believe having the projects in the Gradle project structure is more portable than the Eclipse project structure.  

##License
All of this code is MIT licensed, which is described [here](license.txt).
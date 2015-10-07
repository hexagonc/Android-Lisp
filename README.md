# Android-Lisp
Contains experimental code for building dynamic user interfaces in Lisp on Android devices.

##Installation Instructions for Eclipse (Android Studio instructions to come later):
Clone the repository to a directory, X.  The Eclipse projects are stored in the directory X/Android-Lisp/projects/eclipse.  This folder
can be the root of your Eclipse workspace.  Start Eclipse with this path as your workspace.  Then go to "File -> Import... -> General -> Existing projects into workspace".
Select X/Android-Lisp/projects/eclipse as the base folder and check all projects within.  Some of these projects have dependencies on third party libraries
that can be obtained from Maven Repository via Ivy so you'll need the Ivy Eclipse plugin.  Add "http://www.apache.org/dist/ant/ivyde/updatesite" to your Eclipse 
update sites if necessary.  After installing the Ivy plugin, right click the ivy.xml file in the package explorer for AndroidLispGUIBuilder projects.  Near the bottom of the 
context menu, select "Add Ivy Library".  If this opens a dialog screen for "IvyDE Managed Libraries", go to the "Classpath" tab and check the 
"Enable project specific settings" checkbox.  Uncheck "resolve dependencies transitively" (we don't want the downstream dependencies for the libraries because
they are already provided by the Android framework).  Under "Build the classpath with", check the radio button for "retrieved artifacts".  Change the
"Retrieve pattern" to "libs/[artifact]-[revision].[ext].  This is because Android jar dependencies must be stored in the libs folder under your project.  Click "Finish".
Do the same for the AndroidLispGUIBuilder project.  If you want to run the desktop lisp code in the DesktopLispTester, then you'll have to add the
Ivy libraries here too, although they are not needed for the Android application.


You'll also have to manually create a src folder on the filesystem for the AndroidToolsAfter
project since git cannot retrieve an empty folder from a repository.  (The src folder is empty because the main code for AndroidTools is linked from an external source, in the directory 
X/Android-Lisp/common/src/main.)  Due to a limitation of Android, (or Eclipse, not sure which is responsible), you can only attach sources for an Android library project 
from one source folder so I choose to base the attached sources on the external linked sources rather than any sources defined in the AndroidTools project itself.

The Android GUI Builder code should run on any Android device with SDK 11 (Gingerbread) or greater.  However, if you are building in Eclipse with a JDK of 1.7 or greater
than you'll need to set the Android project build target in Eclipse to SDK 19 or greater.  

##Program Behavior
The Android Lisp GUI builder is organized as a simple tab activity.  The initial tab you'll see, labelled "Console", is the command console which allows you to load a number
 of sample lisp gui programs as well as define and save (to a very limited degree) your own programs.  In order to run these sample programs, you have to first execute the lisp code which generates a 
VieProxy. (A ViewProxy is a wrapper for an Android View.)  Then you need to press the "render" button which automatically switches you to the "render" tab where you can interact with the views that were created by the lisp.
There are currently three sample programs: (1) A program which demonstrates how to build a simple tic-tac-toe game using lisp functions, (2) a program that not only builds the ui but 
which incorporates a very stupid artificial intelligence for playing tic-tac-toe and keeping track of scores, (3) a dummy program for interfacing with a Lego Mindstorms NXT robot via bluetooth.  
This program is currently non-operable, but when finished, should demonstrate the ability to create the brains of a simple robot using an Android phone and lisp.  There's not a lot of comments in the code
right now but it should be pretty easy to make small tweaks to the UI.  If you already understand Android gui development using conventional xml, then you should recognize many of the 
keyword attributes that are used to do such things as set the background-color of a view or viewgroup and set the size of text.  The gui lisp functions are based on the Android xml elements of similar names.  
Currently, only linear layouts and textviews are well supported, although there is limited support for relative layouts, dialogs, buttons, toasts, and a few other widgets.  If you just want to 
play around with non-gui lisp code, you can execute any instruction on the "Console" tab by pressing "Evaluate".  The last state of the code edit view is automatically saved to the 
"Previous" option in the spinner at the top.    


##Future
This repository represents an ongoing project that I will be constantly adding more features to.  The immediate improvements will be to test the bluetooth connectivity code for the 
Mindstorms NXT and add some Android unit tests.  I will probably also add support in lisp for more Android widgets and more layout parameters for the existing widgets (especially the 
relative-layout).  I'll also eventually add user selectable AI for the tic-tac-toe agent to demonstrate different problem solving techniques in AI.  For example, there might be a tic-tac-toe 
agent that initially doesn't know how to play the game but which learns via reinforcement learning, or there might be an AI player that always plays super defensively at the cost of winning.

As mentioned above, I will eventually port the projects to Android Studio, since I believe Android Studio is much better behaved in a lot of ways than Eclipse (being also the officially 
supported IDE for Android).  Also, I believe having the projects in the Gradle project structure is more portable than the Eclipse project structure.  

##License
All of this code is MIT licensed, which is described [here](license.txt).
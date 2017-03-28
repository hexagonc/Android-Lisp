# Build the shared library for LispDesktopConsole.  This file is currently configured to generate a dynamic library for
# OSX.  Modify as needed for Linux or Windows.  Remove -Wall and -g for production builds

CFLAGS=-Wall -g -std=c99
#Configure this for your system.  On Windows, this is %JAVA_HOME%/include
JNI_INCLUDE_PATH_BASE=/Library/Java/JavaVirtualMachines/jdk1.7.0_75.jdk/Contents/Home/include

#Configure this for your system.  On Windows, this is %JAVA_HOME%/include/win32
JNI_INCLUDE_PATH_EXTRA=$(JNI_INCLUDE_PATH_BASE)/darwin

# Configure this for your system
CC=clang

objs = tools.o neural_library.o com_evolved_automata_nn_NativeTools.o
app_objs = tools.o neural_library.o test_network.o

# Change  this to libneural_tools.so for Linux or neural_tools.dll for Windows
library_target=libneural_tools.dylib

# Change this to an exe file for windows
app_target=test_network

# Update this for Linux or Windows, replacing "-dynamicLib" flag for something like "-shared" for gcc and removing "-lm" for Windows
$(library_target): $(objs)
	$(CC) -dynamiclib -o libneural_tools.dylib $(objs) -lm

test_app: $(app_objs)
	$(CC) -o $(app_target) $(app_objs)

com_evolved_automata_nn_NativeTools.o:
	$(CC) $(CFLAGS) -c com_evolved_automata_nn_NativeTools.c -I$(JNI_INCLUDE_PATH_BASE) -I$(JNI_INCLUDE_PATH_EXTRA)

test_network.o:
	$(CC) $(CFLAGS) -c test_network.c

tools.o: tools.h
	$(CC) $(CFLAGS) -c tools.c
neural_library.o: neural_library.c neural_library.h tools.h
	$(CC) $(CFLAGS) -c neural_library.c 
clean:
	rm -f neural_library.o
	rm -f tools.o
	rm -f com_evolved_automata_nn_NativeTools.o
	rm -f libneural_tools.dylib
	rm -f test_network
	rm -f libneural_tools.jnilib
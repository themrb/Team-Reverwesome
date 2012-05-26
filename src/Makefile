# COMP3130 MAKEFILE
# Stephen Gould <stephen.gould@anu.edu.au>

FILES_H = board.h comms.h
FILES_CPP = board.cpp comms.cpp

FILES_OBJ = $(FILES_CPP:.cpp=.o)

CCC = g++

all: client

client: ${FILES_OBJ} client.o
	$ gnatgcc -c agent.adb
	$ gnatgcc -c boards.adb
	$ gnatgcc -c gametree.adb
	$ gnatgcc -c minmax.adb
	$ gnatgcc -c configure.ads
	$ gnatgcc -c exceptions.adb
	$ gnatgcc -c temporaldifference.adb
	$ gnatgcc -c features.adb
	$ gnatbind -n agent
	$ gnatgcc -c b~agent.adb
	$ gnatlink -L/usr/lib/gcc/x86_64-linux-gnu/4.4.3 -lstdc++ -o client client.o ${FILES_OBJ} agent.ali --LINK=c++
	$ rm -f b~*

%.o : %.cpp
	${CCC} -c $< -o $@

clean:
	rm -f client server
	rm -f *.o
	rm -f *.ali
	rm -f b~*

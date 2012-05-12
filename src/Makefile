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
	$ gnatbind -n agent
	$ gnatbind -n boards
	$ gnatbind -n gametree
	$ gnatbind -n minmax
	$ gnatbind -n configure
	$ gnatbind -n exceptions
	$ gnatgcc -c b~agent.adb
	$ gnatgcc -c b~boards.adb
	$ gnatgcc -c b~gametree.adb
	$ gnatgcc -c b~minmax.adb
	$ gnatgcc -c b~exceptions.adb
	$ gnatlink -L/usr/lib/gcc/x86_64-linux-gnu/4.4.3 -lstdc++ -o client client.o ${FILES_OBJ} agent.ali --link=c++

%.o : %.cpp
	${CCC} -c $< -o $@

clean:
	rm -f client server
	rm -f *.o

# COMP3130 MAKEFILE
# Stephen Gould <stephen.gould@anu.edu.au>

FILES_H = board.h comms.h
FILES_CPP = board.cpp comms.cpp

FILES_OBJ = $(FILES_CPP:.cpp=.o)

CCC = g++

all: client

client: ${FILES_OBJ} client.o
	$ gnatgcc -c agent.adb
	$ gnatbind -n agent
	$ gnatgcc -c b~agent.adb
	$ gnatlink -L/usr/lib/gcc/x86_64-linux-gnu/4.4.3 -lstdc++ -o client client.o ${FILES_OBJ} agent.ali --link=c++

%.o : %.cpp
	${CCC} -c $< -o $@

clean:
	rm -f client server
	rm -f *.o

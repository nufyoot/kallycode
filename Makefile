all: main.o
	g++ -std=c++0x obj/main.o -o kallyc -O2

main.o: src/main.cpp
	mkdir -p obj/
	g++ -std=c++0x -c src/main.cpp -o obj/main.o -O2
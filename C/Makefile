CC = gcc -g
CFLAGS = -Iinclude -fopenmp 
CLINK = -lm -lsqlite3
OPT =

main.exe: main.o km.o
	$(CC) main.o km.o -o main.exe $(CLINK)

main.o: main.c
	$(CC) $(CFLAGS) $(OPT) -c main.c -o main.o

km.o: src/km.c
	$(CC) $(OPT) -c src/km.c -o km.o

clean:
	rm *.o *.exe

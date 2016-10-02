CC = gcc
CFLAGS = -Iinclude
OPT = -lm

main.exe: main.o km.o
	$(CC) main.o km.o -o main.exe $(OPT)

main.o: main.c
	$(CC) $(CFLAGS) -c main.c -o main.o

km.o: src/km.c
	$(CC) -c src/km.c -o km.o

clean:
	rm *.o *.exe

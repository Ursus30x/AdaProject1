main: clean

	gnatmake src/main.adb

clean:
	rm -f main
	rm -f main.ali
	rm -f main.o


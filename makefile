main: clean

	gnatmake src/simulation.adb

clean:
	rm -f simulation
	rm -f simulation.ali
	rm -f simulation.o


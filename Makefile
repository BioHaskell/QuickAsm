#!/usr/bin/make

SRC=*.hs Score/*.hs Util/*.hs

tags: $(SRC)
	hothasktags $(SRC) > tags

clean:
	rm -f `find . -iname '*.o'`
	rm -f `find . -iname '*.p_o'`
	rm -f `find . -iname '*.hi'`
	rm -f `find . -iname '*.p_hi'`


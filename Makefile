#!/usr/bin/make

SRC=*.hs Score/*.hs Util/*.hs

tags: $(SRC)
	hothasktags $(SRC) > tags


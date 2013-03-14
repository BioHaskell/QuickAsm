#!/usr/bin/make

SRC=*.hs Score/*.hs Util/*.hs

tags:
	hothasktags $(SRC) > TAGS


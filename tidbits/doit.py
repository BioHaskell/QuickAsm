#!/usr/bin/python

def tcast(arg):
  try:
    if "." not in arg:
      return int  (arg)
    else:
      return float(arg)
  except ValueError:
    return arg

results = []

with open("header.txt", "r") as input:
  for line in input:
    results.append(tuple([tcast(col)
	                  for col
                          in line.split(" ")
			  if col.strip() != ""]))

print str(results).replace("'", '"')

#!/bin/bash

cabal build
echo
for i in aoc??; do 
	echo "Day ${i#aoc}"
	echo =======
	echo -n "Part 1 answer: "
	cabal run -v0 "$i" part1 <"$i/input"
	echo -n "Part 2 answer: "
	cabal run -v0 "$i" part2 <"$i/input"
	grep "Your puzzle answer was" "$i/problem"
	echo
done

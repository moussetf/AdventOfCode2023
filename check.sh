#!/bin/bash

cabal build
echo

for i in aoc??; do
	echo -n "Day ${i#aoc}..."
	actual=$(grep "Your puzzle answer was" "$i/problem" | sed -e 's/Your puzzle answer was \([0-9]*\)./\1/')
	if [[ -f "$i/Main.hs" ]]; then
		produced=$(cabal run -v0 "$i" <"$i/input")
	elif [[ -f "$i/main.bqn" ]]; then
		produced=$(bqn "$i/main.bqn" input)
	fi
	if [[ $produced = $actual ]]; then
		echo " OK"
	else
		echo " FAIL"
	fi
done

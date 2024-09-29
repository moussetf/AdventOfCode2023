# Circular buffer for BFS
Clear‿Push‿Pop‿Size←{
	n←10_000_000 ⋄ i‿j←0‿0 ⋄ buf←n⥊@
		Clear⇐{𝕤 ⋄ j↩i}
	Push⇐{buf 𝕩˙⌾(i⊸⊑)↩ ⋄ i↩n|i+1 ⋄ "Full buffer"!i>j ⋄ i-j}
	Pop⇐{i=j?𝕩; r←j⊑buf ⋄ j↩n|j+1 ⋄ r}
	Size⇐{𝕤⋄n|i-j}
}

# Compute distances from the 'S'
Dists←{
	Dists map:
		Clear @
		h‿w←≢map
		i‿j←⊑(⥊𝕩='S')/∾(↕w)⋈˜¨⚇1‿0↕h
		dist←∞¨map ⋄ Push 0‿i‿j
		Next←{
			m 𝕊 d‿i‿j:
				∞≠i‿j⊑dist? Pop @;
			m 𝕊 d‿i‿j:
				dist d˙⌾(i‿j⊸⊑)↩
				c←{∧´(0‿0≤𝕩)∧(𝕩<h‿w)}¨⊸/ (i‿j)+⚇1‿1⟨¯1‿0,1‿0,0‿¯1,0‿1⟩
				c↩c/˜((∞=c⊑dist)∧(c⊑m))
				Push¨ ((d+1)⊸∾)¨ c
				Pop @
		}
		(map≠'#') Next •_while_ (@⊸≢) 0‿i‿j
		dist
}

input←>•FLines ⊑⎊"sample" •args

# Part 1
Part1←{max Part1 map: +´⥊ {(0=2|max+𝕩)∧(𝕩≤max)}¨ Dists map}
•Show 64 Part1 input

# Part 2
# This only works because there are no obstacles on the line/column with the S
# symbol in the real input (unlike in the sample).
Quad←{
	max Quad map:
		h‿w←≢map
		si‿sj←⊑(⥊map='S')/∾(↕w)⋈˜¨⚇1‿0↕h
		tile←'S'˙⌾(0‿0⊸⊑) '.'˙⌾⊑˘ {𝔽⌾⍉ 𝔽 𝕩} (sj⌽↕w)⊸⊏˘ (si⌽↕h)⊸⊏ map
		local←¯1↓¯1↓˘ Dists (∾⟜'.'˘ {𝔽⌾⍉ 𝔽 𝕩} tile)
		evenodd←0‿1 =⚇0‿∞ 2|local
		nevenodd←(+´⥊)¨evenodd
		maxlocal←⌈´(≠⟜∞)⊸/⥊local
		+´ {
			max ≥ maxlocal+h×𝕩?
				(𝕩+1)×(2|max+𝕩)⊑nevenodd;
			(𝕩+1) × +´⥊((2|max+𝕩)⊑evenodd)×max≥local+h×𝕩
		}¨ ↕⌈max÷h
}
Part2←{max Part2 map: ((2|1+max)-4×⌈(max+1)÷2) + +´ (max⊸Quad)¨ ⊢‿⌽‿(⌽˘)‿(⌽˘∘⌽) {𝕎 𝕩}⚇0‿∞ input}
•Show 26501365 Part2 input

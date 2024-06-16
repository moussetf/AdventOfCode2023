!(1≡≠) •args

input←>•FLines ⊑•args

graph‿digraph←{
	F←»∾¨«∾¨»˘∾¨«˘ # NSWE shifts
	indices←(<˘4‿2⊸⥊)¨F↕≢input
	cliffs←¬"v^><"⊸=¨ input
	a ← input {¬⊑𝕨∊".><^v"?4⥊0; ¬𝕩∊"# "}¨ (F input) # Select all neighbours
	b ← input {¬⊑𝕨∊".><^v"?4⥊0; (𝕩='.')∨(𝕩="^v<>")}¨ (F input) # Select only allowed neighbours
	⟨a/¨indices, b/¨indices⟩
}

nodes←(⟨⟩⊸≢¨)⊸(/○⥊)⟜(↕≢) graph
start←⊑(0=⊑)¨⊸/ nodes
end←⊑((¯1+≠graph)=⊑)¨⊸/ nodes

# Generic DFS
_dfs_←{
	graph F _dfs_ Init start:
		v←1¨graph ⋄ data←Init¨↕≢graph
		{ v 0˙⌾(𝕩⊸⊑)↩
		nbs←𝕩⊑graph
		𝕊¨ (⊑⟜v)⊸/nbs 
		𝕩 ⊑ data (𝕩 F nbs⊑data)˙⌾(𝕩⊸⊑)↩
 } start
}

•Show digraph { n 𝕊 ds:  { n≡end?0; 1+⌈´ds } } _dfs_ ¯∞ start

# For part 2, create a simplfied graph by contracting the bare paths
sgraph‿sdist←{
	g←⟨⟩¨graph ⋄ d←⟨⟩¨graph
	digraph {
		n 𝕊 data: data (≢¨⟜¯∞)⊸/↩ ⋄  {
			2≡≠n⊑graph? (+⟜1)⌾⊑⊑data;
			nbs←1⊸⊑¨data
			dists←⊑¨data
			g (∾⟜nbs)⌾(n⊸⊑)↩
			d (∾⟜dists)⌾(n⊸⊑)↩
			{𝕊 l‿m:
				g (∾⟜(<n))⌾(m⊸⊑)↩
				d (∾⟜(<l))⌾(m⊸⊑)↩ }¨ data
			1‿n
		 }
	} _dfs_ ¯∞ start
	g‿d
}

•Show {
	v←1¨input
	best←¯∞
	0 {
		l Search n:
			n≡start? best l⊸⌈↩;
		l Search n:
			v 0˙⌾(n⊸⊑)↩
			cds←n⊑sgraph
			dist←n⊑sdist
			mask←cds⊑v
			(l+mask/dist) Search¨ mask/cds
			v 1˙⌾(n⊸⊑)↩
	} end
	best
}

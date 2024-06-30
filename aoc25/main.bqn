!(1≡≠) •args

Split←(≢⟜⟨⟩)¨⊸/ ((1-˜(1++`)∧¬)∊˜)⊔⊢
UnionFind←{
	𝕊 init: {
		repr←•HashMap˜ init
		Find⇐{ 𝕩≢repr.Get 𝕩? 𝕩⊸repr.Set⊸⊢ 𝕊 repr.Get 𝕩; 𝕩 }
		Union⇐repr.Set○Find
		Reprs⇐repr.Values
	}
}

input←(" :"⊸Split)¨ •FLines ⊑⎊"sample" •args
edges←∾((<⊑)⋈¨1⊸↓)¨ input
vertices←⍷∾input

# Do the (unboosted) random contraction thing
{
	𝕤
	Union‿Find‿Reprs←UnionFind vertices
	(≠vertices) {
		𝕨 𝕊 2: 2;
		u‿v 𝕊 n:
			ru←Find u ⋄ rv←Find v
			{ ru≢rv? ru Union rv ⋄ n-1; 𝕤⋄n } @
	}´ (•rand.Deal≠)⊸⊏ edges
	cut←≠ (⊑≢1⊸⊑)¨⊸/ Find¨¨ edges
	{ cut≡3? 𝕤 ⋄ •Show  ×´≠¨⊐⊸⊔ Reprs@ ⋄ 0; 1 } @
} •_while_ ⊢ 1

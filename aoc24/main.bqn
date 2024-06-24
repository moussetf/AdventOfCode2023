!(1≡≠) •args
Split←(≢⟜⟨⟩)¨⊸/ ((1-˜(1++`)∧¬)∊˜)⊔⊢
ParseInt←(1+¯2×('-'=⊑)) × (+´(10⋆(↕≠))×(⌽(-⟜'0')))∘((≠⟜'-')⊸/)
input←> ParseInt¨∘(", @"⊸Split)¨ •FLines ⊑•args
pos←3↑˘input
vel←3↓˘input

# LU decomposition
Elim←{
	𝕨 𝕊 ⟨p,l,u⟩: 0≡𝕨‿𝕨⊑u? ⟨p,l∾˘⎊⊢(1˙⌾(𝕨⊸⊑)0˙˘u),u⟩;
	𝕨 𝕊 ⟨p,l,u⟩: v←(𝕨↓𝕨⊑˘u)÷𝕨‿𝕨⊑u ⋄ ⟨p,l∾˘⎊⊢(𝕨⥊0)∾v,u-((𝕨⥊0)∾»«v)×⌜𝕨⊏u⟩
}
Perm←{
	𝕨 𝕊 ⟨p,l,u⟩: 0≡𝕨‿𝕨⊑u?
		q←(↕𝕨)∾𝕨+⍒| 𝕨↓𝕨 ⊑˘u
		m←q⊏u
		⟨q⊏p,q⊏⎊⊢l,m⟩;
	𝕩
}
LU←{ ⟨↕≠𝕩,⟨⟩,𝕩⟩ (⊣ Elim Perm)´ ⌽↕≠𝕩 }

# Solve a non-singular linear system
Sub←{
	b 𝕊 idx‿l:
		(0⥊˜≠b) {
			i 𝕊 𝕩: "Singular matrix"!0≢(i‿i)⊑l ⋄ (((i⊑b)-+´𝕩×i⊏l)÷((i‿i)⊑l))˙⌾(i⊸⊑) 𝕩
		}´ idx
}
Solve←{ 𝕨 𝕊 𝕩: p‿l‿u←LU 𝕩 ⋄ b←p⊏𝕨 ⋄ (b Sub (⌽↕≠𝕨)‿l) Sub (↕≠𝕨)‿u }

•Show {
	Rot90←1‿¯1×⌽
	Crossing←{ p‿v 𝕊 q‿w: vt←Rot90 v ⋄ wt←Rot90 w ⋄ (+˝˘ (p×vt)≍q×wt) Solve⎊∞‿∞ vt≍wt }
	IsFuture←{ p‿v 𝕊 q: ∧´ 0≤(q-p)÷v }
	In←{⟨min,max⟩ 𝕊 𝕩: ∧´(min<𝕩)∧(𝕩≤max) }
	ta←("sample"≡⊑•args)◶⟨200000000000000‿400000000000000,7‿27⟩ @
	hs2d←(2↑˘pos)⋈˘(2↑˘vel)
	cs←Crossing⌜˜<˘ hs2d
	ds←(⍉∧⊢) hs2d⊸(((<⊣)IsFuture¨⊢)˘)cs
	2 ÷˜≠ ta⊸In¨⊸/ ds/○⥊cs
}

•Show {
	# The idea is that three hailstones can fully determine the solution. The
	# appropriate system of quadratic equations for the 9-element vector p∾v∾t
	# can (sometimes) be solved using Newton's method. This is repeatedly
	# attempted with random initial conditions and random triples of hailstones
	# until a solution is found.
	eps←0.0000001
	maxsteps←100
	mat←3‿9⥊1‿0‿0‿1‿0‿0‿1‿0‿0‿0
	+´3↑⊑ {
		𝕤 ⋄ s←3⊸•rand.Subset ≠input ⋄ p3←s⊏pos ⋄ v3←s⊏vel ⋄ start←•rand.Range¨ 9⥊1000
		F←{ 𝕊 vec: p‿v‿t←<˘3‿3⥊vec ⋄ ⥊ (t×(3‿3⥊v)-v3) + (3‿3⥊p)-p3 }
		DF←{ 𝕊 vec: p‿v‿t←<˘3‿3⥊vec ⋄ ⍉mat∾(mat×⎉1‿1 3/t)∾(3‿9⥊12⊸↑⌾⍉(3‿3⥊v)-v3) }
		i←0
		x←⌊0.5 + { i 1⊸+↩ ⋄ 𝕩 - (F 𝕩) Solve⎊(9⥊0) DF 𝕩 } •_while_ { (i<maxsteps) ∧ eps<⌈´|F 𝕩} start
		x ⋈ ⌈´|F x
	} •_while_ {eps<1⊑𝕩} @‿∞
}

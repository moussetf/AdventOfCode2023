brick←{
	# Bricks are vectors (x0 y0 z0 x1 y1 z1 idx)
	Idx⇐6⊸⊑
	XMin⇐(0⊸⊑)⌊(3⊸⊑) ⋄ YMin⇐(1⊸⊑)⌊(4⊸⊑) ⋄ ZMin⇐(2⊸⊑)⌊(5⊸⊑)
	XMax⇐(0⊸⊑)⌈(3⊸⊑) ⋄ YMax⇐(1⊸⊑)⌈(4⊸⊑) ⋄ ZMax⇐(2⊸⊑)⌈(5⊸⊑)
	Height⇐1+ZMax-ZMin
	# The projection to the xy plane will be important
	Footprint⇐0‿1⊸⊏ {⥊(<𝕨)+↕𝕩-𝕨-1} 3‿4⊸⊏
}

# Parse input
Split←((1-˜((1++`)∧¬))∘∊˜)⊔⊢
ParseInt←+´(10⋆(↕≠))×(⌽(-⟜'0'))
bricks←∾¨⟜(↕≠) (ParseInt¨ 6⊸↑∘",~"⊸Split)¨ •FLines ⊑⎊"sample" •args

# Build a matrix with m(ij)=1 iff brick i supports brick j
mX←1 + ⌈´brick.XMax¨ bricks
mY←1 + ⌈´brick.YMax¨ bricks
supportMat ← 2⊑ ⟨mX‿mY⥊0,mX‿mY⥊¯1,(≠bricks)‿(≠bricks)⥊0⟩ {
	b 𝕊 ⟨heights,highest,supportMat⟩:
		idx←brick.Idx b ⋄ fp←brick.Footprint b
		z←fp⊸⊑ heights ⋄ zmax←⌈´z
		supp←⋈⟜idx¨ (¯1⊸≠⊸/) ¯1+(z=zmax)∧(1+fp⊸⊑highest)
		⟨ (zmax + brick.Height b)¨⌾(fp⊸⊑) heights ⋄ idx¨⌾(fp⊸⊑) highest ⋄ 1¨⌾(supp⊸⊑)supportMat ⟩
}´(⍒brick.ZMin¨)⊸⊏ bricks

# A brick is a disintegrable iff every brick it supports is also supported by
# least one other brick, that is, twice the row in the matrix is at most the
# sum of the all the rows:
•Show +´(2⊸×)⊸(∧´∘≤⎉1‿∞)⟜(+˝) supportMat
# The "super" of a set of blocks is the set of blocks supported wholly
# on blocks in this set.
Super←(<˘⍉supportMat)(∧´≤)¨<
NumFalling ← (¯1+(+´)) ∘ (1⊸⊑) ∘ ({
		𝕊 ⟨·, set⟩: new ← (∨˝ set/supportMat) ∧ Super set ⋄ ⟨∨´new>set, set∨new⟩
		} •_while_ ⊑) ∘ {⟨1,𝕩=↕≠bricks⟩}
•Show +´ NumFalling¨ ↕≠bricks

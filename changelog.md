# v.1.0.2p

- No change to the brief or interface but I updatd pack.zip to make
`outputConfig` in Game.hs a bit more robust to variations in answers
for Task 1.

# v1.0.2

- Task 4 (b), bullet two said that the north room should have "a door to
the east requiring orange and yellow keys" this should be "a door to
the east requiring blue and yellow keys".

# v1.0.1p

- No change to the brief, but a slight change to pack.zip in JavaScript.hs:
the `compileExpr` function now puts parentheses around compiled BinOps, e.g.
`compileExpr (BinOp "+" (Num 1) (Num 2)) = "(1 + 2)"`. This should not
affect any solutions, but is slightly nicer for experimentation.

# v1.0.1

- Added a little more explanation on the Room representation on p.1
  and in Task 1 (a).

# v1.0.0

- Initial version
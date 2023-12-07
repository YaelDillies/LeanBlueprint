def x := 1

-- some other comment

-- some comment

def a := false

def b := -1

  theorem flt : ∀ a b c n : Nat, n > 2 → a^n + b^n ≠ c^n := by sorry

theorem flt3 : ∀ a b c : Nat, a^3 + b^3 ≠c^3 := by
  intro a b c
  apply flt
  decide
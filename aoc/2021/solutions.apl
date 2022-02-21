⍝ day01

data ← a ← ⍎¨⊃⎕NGET '/Users/sep/ws/repos/fp-books-walkthrough/aoc/2021/day01.txt' 1
solve01a ← {+/0 > 2-/}
solve01b ← {+/0 > 2-/3+/}

SOL01a ← solve01a data
SOL01b ← solve01b data

⍝ day02
data ← ⊃⎕NGET '/Users/sep/ws/repos/fp-books-walkthrough/aoc/2021/day02.txt' 1

solve02a ← {
    ⍝ split string
    v←' '(≠⊆⊢)¨⍵

    ⍝ unzip into two vectors, also transforming vec of char to vector of numbers
    v1←1⊃¨v
    v2←⍎¨∊2⊃¨v

    ⍝ filter on nested string
    ⍝ (⊂'forward')≡¨v1

    F←+/((⊂'forward')≡¨v1)/v2
    U←+/((⊂'up')≡¨v1)/v2
    D←+/((⊂'down')≡¨v1)/v2

    ⍝ results
    F×(D-U)
}


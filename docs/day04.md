## You're <role>expert software developer</role>

## Your task is to <purpose>write code to solve advent of code problem per instructions and user-input below</purpose>

```xml
```

<instructions>
    <instruction>each day's problem has two parts. write reusable methods that has informative names.</instruction>
    <instruction>think step by step the algorithm how example input can be transformed to output answer</instruction>
    <instruction>write java junit tests for each exmaples</instruction>
    <instruction>make it work then make it fast and keep it simple.</instruction>
    <instruction>code should follows best practices and idioms of implementation language</instruction>
</instructions>

<user-input>
--- Day 4: Ceres Search ---
<part-2>

<example>
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
Looking for the instructions, you flip over the word search to find that this isn't actually an XMAS puzzle; it's an X-MAS puzzle in which you're supposed to find two MAS in the shape of an X. One way to achieve that is like this:

M.S
.A.
M.S
Irrelevant characters have again been replaced with . in the above diagram. Within the X, each MAS can be written forwards or backwards.

Here's the same example from before, but this time all of the X-MASes have been kept instead:

.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........
In this example, an X-MAS appears 9 times.
</example>

Flip the word search from the instructions back over to the word search side and try again. How many times does an X-MAS appear?
</user-input>

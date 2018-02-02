# Pre-install

You need to install ml-ulex (or ml-lex?)

# Run

$ sml -m sources.cm

# Overview

The tiger.lex has implemented the token rules defined in the tokens.sig. The order of token rules are different from the ones in the tokens.sig, but we make sure there is no ambiguity.

We have four states in total: INITIAL, COMMENT, STRING and STRESCAPE.

All whitespaces at INITIAL state will be ignored. ALL newlines at INITIAL or COMMENT states will be ignored as well.

We give comment the highest priority. The next is symbols, after that is reserved identifiers, then ints, strings, normal identifers. All characters that cannot be captured by these rules will be treated as illegal character and reported to the terminal.

# Tokens & Rules
## String (Maybe we can earn one extra credit on string builder?)

Two rules for the string need to be stated:

1. The definition of printable characters: we define [\032-\255]|\t as printable characters, which means space, tab are also included in the printable characters. These characters are normal characters that can appear in the string.
2. The definition of unprintable characters: Only unprintable characters can appear in the /f\_\_f/ pattern, so we define [\000-\032] as unprintable characters.The pattern includes space, newline, tab and formfeed.

One mechanism of the string builder needs to be stated:

1. To improve the efficiency of building the string, we maintained a list of string chunks during lexicalizing the string. So instead of concatenating the string one by one, we join them at the end. The time complexity is O(n) instead of O(n^2). The implementation details are listed at the beginning of the tiger.lex file. stringEmpBuffer is to clear the string buffer and record the beginnig position of the string. stringAppBuffer is to append the string chunck to the beginning of the string buffer. stringBldBuffer is to build the string from the string buffer.

2. Also we add a detection to the unclosed string. 

## Comment
According to the specification, nested comments are supported in tiger. A `commentLevel` variable is maintained for recognizing the closed comment blocks.
When encountering a "/\*" in `INITIAL` state, the state transits to `COMMENT` and the `commentLevel` is incremented by 1. In `COMMENT` state, `commentLevel` is incremented by 1 when encountering a "/\*" and decremented by 1 when encountering a "\*/". If `commentLevel` becomes `0` in `COMMENT` state, the state transits back to `INITIAL` state.

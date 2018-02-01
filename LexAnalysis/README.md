# Pre-install

You need to install ml-ulex (or ml-lex?)

# Run

$ sml -m sources.cm

# Overview

The tiger.lex has implemented the token rules defined in the tokens.sig. The order of token rules are different from the ones in the tokens.sig, but we make sure there is no ambiguity.

We have four states in total: INITIAL, COMMENT, STRING and STRESCAPE.

All whitespaces at INITIAL state will be ignored. ALL newlines at INITIAL or COMMENT states will be ignored as well.

We give comment the highest priority. The next is symbols, after that is reserved identifiers, then ints, strings, normal identifers. All characters that cannot be captured by these rules will be treated as illegal character and reported to the terminal.



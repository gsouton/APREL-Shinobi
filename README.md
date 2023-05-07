# APREL-Shinobi
Advanced Programming, University of Copenhagen KU.

# APREL: A regular expression library
This is a library to parse regular expression implemented in Haskell.
Regex should be written as `/.../` (slashes are not part of the regex)
For example: `/ab*/` matches the string `"abbb"` but not `"baab"`.

## Regex
This library supports

- [ ] **Characters:** match only the given character in the subject string.
Reserved characters `('|' or '.')` must be escaped with a preceding backslash
(`'/\|/'` to match a literal `"|"`) and some special characters cans also be 
written as espace sequences such as `/\n/` for matching a newline character.

- [ ] **Character class:** Matches any character from a finite set
(decimal digits or whitespace). Character classes are written between square 
brackets, possibly with ranges (e.g.`/[A-Za-z0-9_]/` matches any alphanumeric
character or underscore)
Classes may also be complemented e.g. `/[^0-9\-]/` matches any character
except digits and minus.
Finally the special regex `/./` matches any character.

- [ ] **Sequencing:** The empty regex `//` matches only the empty string `""`.
The regex `/r1r2/` matches a string if and only if the string can be split into
two parts such that `/r1/` matches the part and `/r2/` matches the second.
Since r1 and r2 can themselves be sequences in general any number of regexes 
may appear one after another. For example the regex `/[a-z]=[0-9]/` matches
any lowercase letter followed by 'equals' and a digit such as `"x=7"`

- [ ] **Alternation:** The regex `/r1|r2/` matches a string if either `/r1/`
or `/r2/` (or both) match it. For example `/a|bb/` matches only 
`"a"` and `"bb"`.

- [ ] **Grouping:** The regex `/(r)/` matches exactly the same string as 
`/r/` but overrides the normal precedences and associativities of the regex
operators. For example the regex `/ab|c/` matches `"ab"` and `"c"`, whereas
`/a(b|c)/` matches `"ab"` and `"ac"`.

- [ ] **Repetition:** The regex `/r{n}/`, where n is a natural number, 
matches a string if the string can be split into `n` parts, each of which is
matched by `/r/`. The regex thus behaves essentialy the same as a length-n
sequence of r's. For example `/(a|bb){3}/` matches `"aaa", "abba", "bbbbbb"`,
etc. but not `"baa"` or `"baab"`. 

    The more general form of `/r{n,m}/` where
    `0 <= n <= m` matches between n and m consecutive occurences of r. 
    For example `/(a|b){3,5}/` matches `"aaa"`, `"baaa"` and `"ababa"` 
    but not `"aa"` or `"bababa"`. If the number m is omitted there is no upper 
    limit, for example `/a{7,}/` matches any string of 7 or more a's.
    The conventional regex forms `/r?/`, `/r*/` and `/r+/` can be seen as 
    syntactic sugar, for respectively `/r{0,1}/, /r{0,}/` and `/r{1,}/`.

- [ ] **Capture:** The regex `/(#r)/` matches the same as just `/(r)/` but
additionally adds the matched string to the end of a growing list of capture 
resulst, which may be referrence by later regexes in a sequences.
(see **Backreferences**).

    ***Caution:*** Unlike most PCRE-style syntax `APREL` uses a sperate notation 
    for capture group, instead of making ordinary parenthetization also 
    implicitly capture the grouped contents. In `APREL` `/(#r)/` corresponds
    to `/(r)/` in common regex syntaxes. While APREL's `/(r)/` corresponds to 
    an explicitly non capturing group generally written as `/(?:r)/`.

    Capture may occur inside complex regular expression but their scope may 
    be limited in some contexts. Specifically, any captures performed in the
    branches of a `|`-alternation or in the body of a repetition `{n, m}`
    after the fixed first n occurences are discared as soon as the whole branch
    or occurences are discardeed as soon as the whole branch or occurence has 
    succesfully matched, because they would otherwise make the numbering
    of all subsequent captures unpredictable. For example in 
    `/(#a)(b|(#cc))\2` or `/(#a){1,2}\2` the numbered backreference `/\2/`
    will be undefined (and hence fail to match).
    On the other hand it is still possible to make backreferences to captures
    completed prior to the alternation or repetition so that e.g. 
    `/(#a*)(b|c)*\1/` will match `"aacbbaa"`.
    Also local backreferences may occur in the same alternation branch or
    repetition iteration as the capture so that `/(#c)(a|(#b)\2)*\1 will 
    match `"cabbabbaac"`.(But after the repetition `\2` will again be 
    undefined.)

    Nested captures are also allowed with the captured strings conceptually 
    added to the list as the matching of each nested regex is completed.
    For example, `/(#ab(#c*)d)e/` matches `"abccde"` with \1 bound to 
    `"cc"` and \2 to `"abccd"`.

- [ ] **Backreferences:** The regex `/\n/`, where n is a natural number matches
the n'th (one base numbering) string in the capture list. For example,
`/(#.)(#.*)\2\1/` matches `"abcdbcda"` (with `\1` as `"a"` and `\2` as 
`"bcd"`) or `"aa"` (again with `\1` as `"a"` but `\2` as `""`), but not 
`"aba"` or `"abccba"`. If there are fewer than n captured strings 
before the backreference (of if n = 0) the regex does not match anything.

- [ ] **Conjunction:** The regex `/r1&r2/` matches a string if both `/r1/`
and `/r2/` match it. For example `/(aa)*&(aaa)*` matches any sequences of 
n a's whre n is a multiple of 6 or 5 (to verify)?
Any captures performed by `/r1/` are available in `/r2/` which may add 
additional captures. For example `/((#.*)ab)&(a\1(#.*))/` matches `"aaab"`
binding `\1` to `"aa"` and `\2` to `"b"`.

- [ ] **Negation:** The regex `/r!/` matches a string preciely when `/r/`
does not match it. For example `/(ab*)!/` matches the strings `"", "c", "ba"
or "aba"` but not `"a" or "abb"`.
Any captures performed inside `/r/` will inherently be unvailable after `/r!/`,
because if `/r/` succeeds, then `/r!/`fails, and if `/r/` fails then 
any tentative to captures if performed anre undone as usual before `/r!/` 
can succeed. In particular, `/r!!/` matches exactly the same string as 
just `/r/` (though perhaps less efficiently) but without addidn any capture 
to the list: `/(#ab)\1/` matches `"abab"` but `/(#ab)!!\1/` does not.

    ***Caution:*** Do not confuse regex negation with character-class complementation
    For example `/[^a]/ matches all single-character strings except `"a"` 
    whereas `/a!/` also matches e.g. `""` or `"aa"` On the other hand,
    `/(a!)&./ matches exactly the same string as `/[^a]/`(but again probably
    less efficiently).

APREL regexes are more powerful than formal regular expression.

# Shinobi: An alternative to Ninja.

This is about Shinobi an alternative to the build system Ninja.
Shinobi makes it possible to start an opration to a keikaku (meaning plan)
that describes what tasks need to be done to complete the operation.

An operation can succeed with a result or can fail with a result. That is, 
the result of an operation will always be a tuple where the first component
is one of the atoms `success` or `failure`

## API

`SubKeikaku` is always a `keikaku` and `SubKeikakus` is a list of `keikakus`.

- `prepare()` for start a Shinobi server.
Returns `{ok, Shinobi}` on success, or `{error, Reason}` if some error
occured.

- `register_command(Shinobi, Cmd, Fun)` is for registering the function 
`Fun` as the command `Cmd`, an atom,  at the Shinobi server. It is assumed 
that it is safe to terminate a process running Fun at any point and tat there
are no limits on how many concurrent instances we can run.

- `operation(Shinobi, Keikaku)` for starting an opration according the keikaku
`Keikaku`.
Returns `{ok, OperationId}` on success or `{error, Reason}` if some error
occrured.



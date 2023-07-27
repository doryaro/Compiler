# Scheme Compiler

Compiler from Scheme to Assembly X86_64 - written in Ocaml and Assembly X86_64\
The compiler support all Scheme syntax and more (e.g. interpolated strings)

## Testing the Reader 

The correct way to unit test your work is to create a separate testing file that loads your code (i.e. `reader.ml`) and 
applies tests to it. To encourage you to test your code correctly and quickly we're including the testing framework that 
will be used in grading your work (under the `tests` directory).

The way this testing framework works is by taking all the files in `tests/cases`, running them through your reader, 
converting the results back into s-expressions and using Scheme's `equal?` function to compare the original input with 
your output.

To run these tests you simply open a terminal in your project's root dir and run `tests/test_cases.sh` and you should 
see the result for each tested file. So for a fresh repo you should see:
```
$ cat tests/cases/boolean.scm
#t #f

$ tests/test_cases.sh 
--------------------boolean.scm--------------------:
Exception: X_not_yet_implemented.
FAIL: 
```

And after your first implement Booleans incorrectly (for the sake of demonstration) you might see something like:
```
$ tests/test_cases.sh 
--------------------boolean.scm--------------------:
FAIL: `(tests/cases/boolean.scm: ,(equal? '(#t #f) '(#t #\f)))
```

And finally, after implementing some parsers (correctly) and adding a test of your own called `my_test.ml` to 
`tests/cases`, you will see:
```
# cat tests/cases/my_test.scm
#t #\t 1 abc

$ tests/test_cases.sh 
--------------------boolean.scm--------------------:
PASS: (tests/cases/my_test.scm: #t)
--------------------my_test.scm--------------------:
PASS: (tests/cases/my_test.scm: #t)
```

## Testing Tag-Parser
The entire testing setup can be found in `./tests/test_tag_parser.ml` - cases and testing code.

When you first run `utop ./tests/test_tag_parser.ml` you should get the following output:
```
$ utop tests/test_tag_parser.ml 
Exception: Syntax Error message: Sexpr structure not recognized for sexpr: #t: Boolean
Exception: Syntax Error message: Sexpr structure not recognized for sexpr: #\a: Char
.
.
.
Exception: Syntax Error message: Sexpr structure not recognized for sexpr: `(#\a b ,c ,@d 'e (f)): QQ-list
Exception: Syntax Error message: Sexpr structure not recognized for sexpr: `#(a ,b ,@c "d"): QQ-vector
```
Each line represents a test-case. The first part of the line is the result (currently all tests result in a thrown
exception), the second part is the name of the test (e.g. "Boolean" or "QQ-vector" above)

The abovce exceptions mean your tag parser failed to match the S-expression input to a known Scheme expression. Of 
course as you implement parts of your parser, these test will pass, and eventually, your output should be:
```
PASS: Boolean
PASS: Char
.
.
.
PASS: QQ-list
PASS: QQ-vector
```
Add new cases to the `cases` list in `test_tag_parser.ml` to extend your testing setup. 

## Testing Semantic-Analyser
setup can be found in `tests/test_semantic_analyser.ml`.

When you first run  `utop ./tests/test_semantic_analyser.ml` you should get the following output:
```
$ utop ./tests/test_semantic_analyser.ml 
Exception: Syntax not yet implemented: Expr-lexical annotation
Exception: Syntax not yet implemented: Expr'-TP annotation
Exception: Syntax not yet implemented: Expr'-box

```

The test cases for the Semantic analyser are divided into two types. Tests that take as input an `expr` type (to test
the lexical addressing stage), and tests that take as input an `expr'` type (to test the other two stages). You can
see the name of the test is prefixed with the test type ("Expr"/"Expr'").

Output you should see is:
```
$ utop ./tests/test_semantic_analyser.ml 
PASS: Expr-lexical annotation
PASS: Expr'-TP annotation
PASS: Expr'-box
```

### Testing Extended Syntax
Since the compiler supports an extended syntax that isn't part of Scheme (e.g. interpolated strings), our test setup
supports taking an "expected output" file. This "expected output" should contain Scheme code equivalent to the input 
file but which does not use the extended syntax.

Assuming you want to test `extended.scm` which contains some of our extended syntax. Include `extended.expected` file
in the same directory as `extended.scm`, and the test procedure will use that `.expected` file to define the expected
outout.

#### Test Script
The same testing procedure is implemented in `tests/test_compiler.sh`, except it defines `testfile` using a argument.
To run the `foo` test case described above you should execute:
``` 
  tr -d \r tests/test_compiler.sh
  tests/test_compiler.sh foo
```

Note that first command (the one starting with `tr`). That will fix any cases where a Windows OS has ruined the test
script by changing the end of line from UNIX style (ASCII 10) to Windows style (ASCII 10 followed by ASCII 13). It's not
a part of the test, just a workaround for that destructive behavior for Git on Windows.

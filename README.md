
## Description

Given a sequence of numbers like `0, 1, 2, 3, 4, 4, 4, 4, 5, 6, 7, 8, 11, 12`, detect that
there are 3 extra `4`'s and that `9, 10` are missing.

## Screenshot

![Console Screenshot](https://raw.github.com/taylorjg/seqdiffhs/master/Images/Screenshot.png)

## Property Tests

I have a property test for the `diffs` function:

```haskell
prop_diffs :: PropTestData -> Bool
prop_diffs ptd = diffs (toList ptd) == allDiffs ptd
```

I have custom instances of the `Arbitrary` class for `PropTestData` and `Diff`.

```
$ cabal test --show-details=always
Preprocessing test suite 'SeqDiff_Property_Tests' for SeqDiff-0.1.0.0...
Running 1 test suites...
Test suite SeqDiff_Property_Tests: RUNNING...
Passed:
SequenceLength: 23; DuplicateDiffs: []; MissingDiffs: [(10, 4)]
Passed:
SequenceLength: 56; DuplicateDiffs: [(6, 3),(17, 4),(35, 3)]; MissingDiffs: [(11, 3),(28, 4)]
Passed:
SequenceLength: 4; DuplicateDiffs: []; MissingDiffs: []
Passed:
SequenceLength: 47; DuplicateDiffs: []; MissingDiffs: [(11, 2),(18, 4),(23, 2)]
Passed:
SequenceLength: 61; DuplicateDiffs: [(29, 4),(35, 5),(37, 5)]; MissingDiffs: [(17, 5)]
Passed:
```

```
$ cabal test --show-details=always
Preprocessing test suite 'SeqDiff_Property_Tests' for SeqDiff-0.1.0.0...
[4 of 4] Compiling Main             ( test\PropertyTests.hs, dist\build\SeqDiff_Property_Tests\SeqDiff_Property_Tests-tmp\Main.o )
Linking dist\build\SeqDiff_Property_Tests\SeqDiff_Property_Tests.exe ...
Running 1 test suites...
Test suite SeqDiff_Property_Tests: RUNNING...
*** Failed! Falsifiable (after 26 tests and 9 shrinks):
SequenceLength: 84; DuplicateDiffs: []; MissingDiffs: [(80, 2),(82, 1)]
Test suite SeqDiff_Property_Tests: FAIL
Test suite logged to: dist\test\SeqDiff-0.1.0.0-SeqDiff_Property_Tests.log
0 of 1 test suites (0 of 1 test cases) passed.
```

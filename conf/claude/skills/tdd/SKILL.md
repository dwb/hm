---
name: test-driven-development
description: Test-driven development. Use when asked to use TDD explicitly, or any other times when asked write a failing test before implementation, or similar.
---

# Test-driven Development

The point of test-driven development is to write a test that initially fails, to prove that a feature or bug exists in the first place, and to provide a sort of executable specification of the feature or bug. Then we implement the feature or bug fix. Then, without any changes to the test code, we re-run it and, if we have implemented correctly, it should pass.


## Instructions

1. First, fully understand the feature or bug. If there is any lack of clarity, ask questions about it until you have enough information to write a test, or set of tests. You also need to be sure where to write the test.
2. Write the failing test that proves the feature is missing or the bug present. The test must FAIL RED. It MUST NOT be marked failing in code, or skipped. It is good, desired, and indeed the whole point of the exercise for the test to clearly fail. Also DO NOT write any comments in the test indicating that it is meant to fail. By the time it is committed, it isn't meant to fail! No modifications should be necessary at all to the test code before committing. User should explicitly approve the test.
3. Run the test and see that it fails. If it passes, STOP.
4. Implement the feature or bug fix that the test proves is missing. Implement only as much as the test can prove. It may be acceptable to make small accompanying fixes that other processes can prove are correct (for example, type-level code), but tend towards a small amount of code that only fixes what we are testing.
5. Re-run the test we wrote. If it passes, SUCCCESS. If it fails, iterate on implementation until it does. If you keep iterating without success, STOP - there may be an issue with the test, or a deeper problem. Be sure to report back to the user with your progress.

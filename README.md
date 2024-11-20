## APL Concurrent Programming Extensions

This project is an extension of the APL (AP Language) to incorporate advanced features for concurrent and nondeterministic programming. The extensions are implemented across a series of tasks with the goal of providing both simulated and true concurrency within the APL ecosystem.

---

## Features and Tasks

### 1. **Tuples (Task A)**
- Implemented syntax for tuples `(x, y, z)` and projections `x.i` to extract tuple elements.
- Evaluation adheres to left-to-right order with error handling for invalid projections or non-tuple values.

### 2. **Loops (Task B)**
- **For-loops**: Sequential iteration with a bounded range.
- **While-loops**: Iteration based on a boolean condition.
- Introduced stepping (`StepOp`) for effects within loops and functions.

### 3. **Concurrent Operators (Task C)**
- `&&` for concurrent evaluation returning a pair of results.
- `||` for concurrent evaluation returning the result of the first completed expression.
- Both operations extend the APL grammar and are implemented for sequential and concurrent contexts.

### 4. **Simulated Concurrency (Task D)**
- Implemented a simulated concurrent interpreter using stepping (`StepOp`).
- Enabled interleaved execution of computations with support for resource contention using `KvGetOp` and `KvPutOp`.

### 5. **Key-Value Database (Task E)**
- Developed a thread-safe key-value database (`KVDB`) to manage shared state in concurrent settings.
- Supports blocking `get` operations until the requested key is available.

### 6. **True Concurrency (Task F)**
- Built a truly concurrent interpreter using Haskell threads and the `SPC` job scheduler.
- Handled nondeterministic behaviors and implemented true concurrency for `BothOfOp` and `OneOfOp`.

---

## Setup and Usage

### Requirements
- Haskell and Cabal environment.
- Project dependencies are listed in the provided `exam.cabal` file.

### Compilation
To compile and run tests:
```bash
cabal test
```

### Directory Structure
- `src/APL`: Core language implementation files, including parser, evaluator, and interpreters.
- `src/KVDB`: Key-value database implementation.
- `src/SPC`: Simplified job scheduler.

---

## Testing
The project includes a comprehensive suite of tests for each task, covering:
1. Syntax and semantics for new constructs (e.g., tuples, loops).
2. Evaluation of concurrency operators in pure, simulated, and true concurrency modes.
3. Edge cases, such as deadlocks and infinite loops, to validate robust behavior.

To execute the test suite:
```bash
cabal run tests
```

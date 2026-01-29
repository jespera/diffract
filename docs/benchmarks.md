# Testing and Benchmarks

## Testing

Tests use the [Alcotest](https://github.com/mirage/alcotest) framework:

```bash
# Run all tests
dune test

# Run with verbose output
dune build @runtest --force
```

Test coverage:
- **Node module**: Parsing, traversal, text extraction
- **Diff module**: Change detection, node comparison, flattening
- **Antiunify module**: Annotation generation, pattern extraction, cross-pattern unification

## Benchmarks

Performance benchmarks are provided for the pattern matching functionality.

### Running Benchmarks

```bash
# Run pattern matching benchmarks (outputs results to console and saves JSON)
dune exec benchmarks/bench_match.exe

# Run index-based matching benchmarks
dune exec benchmarks/bench_index.exe

# Compare last two benchmark runs
dune exec benchmarks/bench_compare.exe

# Show benchmark history table
dune exec benchmarks/bench_compare.exe -- --history

# Show trends with sparklines
dune exec benchmarks/bench_compare.exe -- --trends

# Generate gnuplot data files
dune exec benchmarks/bench_compare.exe -- --gnuplot
```

### Benchmark Suite

The benchmark suite measures pattern matching performance:

| Benchmark | Description |
|-----------|-------------|
| `simple_pattern` | Simple metavar matching (e.g., `$obj.$method($arg)`) |
| `sequence_N_children` | Sequence metavar (`$body*`) with N children (N=2,5,10,20) |
| `nested_seq_N_children` | Nested patterns with sequence metavar (N=2,5,10,20) |

### Index Benchmark (`bench_index.exe`)

Compares traversal-based vs index-based matching:

| Test | Description |
|------|-------------|
| Multi-Pattern Matching | 3 patterns against sources of varying sizes |
| Scaling with Patterns | Fixed source, 1-3 patterns (shows speedup growth) |
| Selective Patterns | Rare vs common node types |
| Index Reuse | Query times with pre-built index |

Example output:
```
Source Size           Traversal      Indexed    Speedup  Matches
-----------------------------------------------------------------
14 nodes                 0.42 ms       0.16 ms      2.62x       10
45 nodes                 1.22 ms       0.44 ms      2.75x       35
100 nodes                2.49 ms       0.92 ms      2.69x       80
```

### Scaling Test

A separate scaling test measures how performance grows with input size:

```bash
# Run scaling analysis
dune exec benchmarks/bench_scaling.exe
```

Example output:
```
Children     Sequence (ms)     Nested (ms)      Ratio
-------------------------------------------------------
n=10                  0.40 ms         0.46 ms       1.00x
n=20                  0.76 ms         1.00 ms       1.90x
n=50                  1.75 ms         2.21 ms       2.30x
n=100                 3.37 ms         4.33 ms       1.93x
n=200                 7.16 ms        13.48 ms       2.12x
```

The sequence matching algorithm uses O(n) precomputation of cumulative texts,
resulting in approximately linear scaling with the number of children.

### Output

**Console output:**
```
Benchmark                                   ns/iter
-------------------------------------------------------
pattern_matching/simple_pattern           117528.97
pattern_matching/sequence_02_children     107053.02
...
```

**JSON results:** Each run saves a timestamped JSON file to `benchmarks/results/`:
```json
{
  "timestamp": 1769497438.0,
  "git_commit": "abc1234",
  "benchmarks": [
    { "name": "pattern_matching/simple_pattern", "mean_ns": 117528.97 },
    ...
  ]
}
```

**Comparison output (`--trends`):**
```
Benchmark Trends (last 3 runs)

Benchmark                           First          Latest  Trend
----------------------------------------------------------------
simple_pattern                    107.94 us       117.53 us  ▁▅█ +8.9%
sequence_02_children               97.54 us       107.05 us  ▁▇█ +9.7%
```

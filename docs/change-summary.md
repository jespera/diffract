# Generating change summaries

## Motivating example

There are cases where a particular changeset (Pull-request, Merge-request, ...)
contains several changes that are similar in as-of-yet defined sense. This can
occur when the codebase is updated to reflect an API change of an external
library, a rename refactoring, or similar systematic changes.

Whatever the cause, a systematic change can be very tedious to review when it's
a large change across many places in the codebase because the canoncial way to
denote the changes is by a "standard" diff where every change is explicitly
enumerated.

Instead, we could consider using a more compact representation of that changes
(such as the ones `diffract` allows) and instead of 1000's of lines of

```
- f(x, y)
+ g(x)
```

We could have just one denotation:

```
@@
metavar $X : single
metavar $Y : single
@@

- f($X, $Y)
+ g($X)
```

And maybe we'd also include all the locations/files affected.

The more abstract denotation of the change makes it clear which parts are not
important for the change and which parts are. In this example `f` and `g` are
important, but the actual values to the functions are not.

### Tricky cases

- When not all changes are "the same"
- ...


## Existing approaches / ideas

TODO[ja]

## Meaning of "same change"

### Tree edit distance metric

Assume `apply(p1, t1) = t3`, and `d(t1, t2) + d(t2, t3) = d(t1, t3)`.
Then one can say that `p1` is "part of" the change of `t1` to `t3` because the
edits needed after applying `p1` are less than the edits from `t1` to `t3`.

I think it's important that the edit-dist is a *metric* and in particular the triangle inequality is important:

- `d(t1, t3) <= d(t1, t2) + d(t2, t3)`

But the the symmetry and non-negative properties are also important:

- `d(t1, t2) = d(t2, t1)`
- `d(t1, t2) >= 0` and `d(t1, t2) = 0` iff `t1 = t2`

### Commutativity 

- `p1` commutes with `p2` for `(t1, t3)`
  - still need to ensure `p1` and `p2` makes sense for `(t1, t3)`

### Decomposition of change

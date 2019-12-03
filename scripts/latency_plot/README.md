A script to generate plots and compare sampled measurements, optionally across
several categories; for instance comparing latency between master and a
development branch, at several load levels.

## Usage:

    $ plot.py <path_to_data_dir> <y_data_label>

The first argument should be a path to a directory that contains 1 or more CSV
files, which should be named like, e.g.

    my_special_branch.1000_rps.csv

where the two dot-separated sections correspond to `<variant_name>.<group_name>.csv`.
If you aren't comparing the same variant across multiple categories (e.g. at
several different requests-per-second), you can use `_` in place of
`<group_name>`. 

See:

    $ plot.py --help

### Data file format

Each file is a list of values that will be parsed as doubles, one for each
line. Garbage will be ignored and reported.

The Y scale on the graph will automatically display generic human-readable
units (G, M, K, m, u, n), so that e.g. 0.001 will be displayed around the "1 m"
scale (which might correspond to 1-millisecond, say; it's up to you to keep
track of actual units).

### Interpreting plots

The plots contain:
- thin hatches marking percentiles; e.g. a blue line at 120 means that 99% of samples were less than 120
- a randomly jittered scatter plot; these aid in viewing the distribution, and
  individual outliers. Samples from the second half of each dataset (i.e. those
  that were taken later in time) will appear in red; this is to help draw
  attention to any simple drift that is happening (i.e. when the sample value
  is highly correlated with time).
- a violin plot, which is like a sideways histogram; where it is widest
  indicates the largest concentration of samples.

### Example

This directory comes with some example data, which you can use to generate a plot, like:

    $ ./plot.py example_data latency_sec Heap_size RPS

# Notes and TODOs

- Be mindful that higher percentiles come with a large error; if samples are
  thin around the Xth percentile it's best to ignore it. We should improve this
  in the code, possibly by removing percentiles when few samples are above the
  threshold (see commented experiment).
- Consider a log plot maybe, but never filter any outliers
- It would be nice if we had a table of values for median, max, 95th pctl etc.

See the code for more TODOs.

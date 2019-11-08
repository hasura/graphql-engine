A script to generate plots and compare sampled measurements, optionally across
several categories; for instance comparing latency between master and a
development branch, at several load levels.

## Usage:

    $ plot.py <path_to_data_dir> <y_data_label> <param_label> <categories_label>

The first argument should be a path to a directory that contains 1 or more CSV
files, which should be named like, e.g.

    my_special_branch.1000_rps.csv

where the two dot-separated sections correspond to `<param_label>` and
`<categories_label>` respectively. If you aren't comparing across several
categories, the file can have an underdash or something meaningless.

### Data file format

Each file is a list of values that will be parsed as doubles, one for each
line. Garbage will be ignored and reported.

The Y scale on the graph will automatically display generic human-readable
units (G, M, K, m, u, n), so that e.g. 0.001 will be displayed around the "1 m"
scale (which might correspond to 1-millisecond, say; it's up to you to keep
track of actual units).

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

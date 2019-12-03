#!/usr/bin/env  python

# Avoid tkinter dependency (we can only save files)
import matplotlib
matplotlib.use('agg')

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt 
import matplotlib.patches as mpatches 
import matplotlib.ticker as ticker
import glob
import os
import sys 
import argparse


# Parse script arguments:
parser = argparse.ArgumentParser(description='Plot latency samples across categories. See README.md')
parser.add_argument('--log', action='store_true',
                    help='Use a log scale for the Y axis')
parser.add_argument('--baseline-variant',
                    help='Variant to use as the baseline for comparing improvements/regressions. '+
                         'The name should correspond to some <variant>.foo.csv in the data file names'
                    )
parser.add_argument('data_dir_path',
                    help='The path to the directory containing CSV files')
parser.add_argument('y_label', help='Label for the Y axis (i.e. description of data points)')
args = parser.parse_args()

# These correspond to the dot-separated parts of the sample filenames found in
# data_dir_path. These labels are aort of arbitrary and appear in the
# dataframes and graphs and tables.
x_label = 'Variant'
category_label = 'Group'

# Can be used to scale number of Y axis tics, while maintaining nice intervals:
approx_target_y_tics = 20


def prettify(ss):
    if type(ss) == type("string"):
        return ss.replace("_", " ")
    else:
        return list(map(lambda s: s.replace("_", " "), ss))

# Human friendly Y labels.
# Copy-pasta: https://stackoverflow.com/a/40573071 
def y_fmt(y, pos):
    decades = [1e9, 1e6, 1e3, 1e0, 1e-3, 1e-6, 1e-9 ]
    suffix  = ["G", "M", "k", "" , "m" , "u", "n"  ]
    if y == 0:
        return str(0)
    for i, d in enumerate(decades):
        if np.abs(y) >=d:
            val = y/float(d)
            signf = len(str(val).split(".")[1])
            if signf == 0:
                return '{val:d} {suffix}'.format(val=int(val), suffix=suffix[i])
            else:
                if signf == 1:
                    if str(val).split(".")[1] == "0":
                       return '{val:d} {suffix}'.format(val=int(round(val)), suffix=suffix[i]) 
                tx = "{"+"val:.{signf}f".format(signf = signf) +"} {suffix}"
                return tx.format(val=val, suffix=suffix[i])

                #return y
    return y

if not os.path.isdir(args.data_dir_path):
    sys.exit("can't find directory at path: "+ args.data_dir_path)


data_files = glob.glob(args.data_dir_path + '/*.csv')
if len(data_files) == 0:
    sys.exit("No .csv files found in " + args.data_dir_path)

data = []
# We use these to try to visualize drift using scatter points
data_first_half = []
data_second_half = []

error_cnt = 0
for path in data_files:
    dim1, dim2, _ = prettify(os.path.basename(path).split('.'))
    halfway = sum(1 for _ in open(path)) / 2
    with open(path) as f:
        for idx, val in enumerate(f):
            try:
                def ap(d):
                    d.append({args.y_label: float(val)
                             ,x_label: dim1
                             ,category_label: dim2
                             })
                ap(data)
                if idx > halfway:
                    ap(data_second_half)
                else:
                    ap(data_first_half)
            except: 
                error_cnt = error_cnt + 1

# TODO better stats and reporting:
if error_cnt > 0:
    print('Ignored bad samples: ' + str(error_cnt))

df = pd.DataFrame(data)
# I couldn't easily figure out how to derive these from 'df' so we build them separately above:
df_first_half = pd.DataFrame(data_first_half)
df_second_half = pd.DataFrame(data_second_half)


sns.set_style("whitegrid")
plt.figure(figsize=(18,10))

# Plot raw points lightly under everything
# NOTE: this is pretty slow:
def sub_stripplot(d, c, a):
    ax = sns.stripplot(x=category_label, y=args.y_label, hue=x_label, data=d, 
                       palette=sns.color_palette([c]), alpha=a,
                       jitter=0.40,
                       size=2.2,
                       dodge=True) 
    plt.setp(ax.collections, zorder=-1000) 

# Try to make simple drift apparent by coloring samples from the first and
# second halves of the run differently. This doesn't quite work as we'd like;
# the zorder of each point should be randomly interleaved.
sub_stripplot(df_second_half, "#FF4444", '0.5')
sub_stripplot(df_first_half,  "#AAAAAA", '0.9')
# TODO I tried to filter and restyle the outliers to make them more prominent,
# but failed. I think they are visible enough above though, with the styling
# tweaks I did.

# Plot percentiles using boxplot whisker caps:
percentiles = [(99.9, 'red'), (99, 'blue'), (95, 'green')] 
# TODO consider doing something like this in order to validate whether
#      percentiles are meaningful (if we have enough sample data). This results
#      in one boxplot being randomly drawn in the wrong place for some reason...
# df_copy = df.copy()
# df_1 = df_copy.sample(frac=0.50, random_state=99)
# df_2 = df_copy.drop(df_1.index)
# for df_x in [df_1, df_2, df]:
#     for pctl, color in percentiles:
#     ...
for pctl, color in percentiles:
    ax = sns.boxplot(x=category_label, y=args.y_label, hue=x_label, data=df, 
           showfliers=False, 
           showbox=False, 
           # Showing bottom percentiles just seemed to add visual noise:
           whis=[0, pctl],
           capprops={'color': color, 'linewidth': 1},
           # hide all but cap:
           whiskerprops={'linewidth': 0}
           )
    # Keep boxplots from adding nonsense to legend:
    handles, _ = ax.get_legend_handles_labels() 
    for h in handles:
        h.remove()


# This will get overwritten; add back below:
pctl_legend = plt.legend(title='Percentile markers', loc='upper left', 
                         handles=[mpatches.Patch(color=c, label=str(pctl)+"th") for pctl, c in percentiles]+
                                 [mpatches.Patch(color="black", label='median')]
                        )

# TODO use matplotlib's violin plots for decent log space support?
#   https://github.com/mwaskom/seaborn/issues/1257 
#   or: https://github.com/ciortanmadalina/modality_tests/blob/master/violinboxplot_hybrid_axes.ipynb 
# See: https://seaborn.pydata.org/generated/seaborn.violinplot.html 
sns.violinplot(x=category_label, y=args.y_label, hue=x_label, data=df, palette="Set1", 
               scale_hue=True,
               # All violins get the same area (number of samples may differ):
               scale="area", 
               # More granular violins:
               bw=.02, 
               # This seems to wreck things:
               # width=1.5,
               linewidth=0,
               # inner="quartile"
               )

# Add back percentile legend:
ax.add_artist(pctl_legend)

if args.log:
    ax.set(yscale="log")
    ax.yaxis.set_minor_formatter(ticker.FuncFormatter(y_fmt)) 
    ax.grid(b=True, which='minor', color='#888888', linewidth=0.2)
    ax.grid(b=True, which='major', color='#BBBBBB', linewidth=0.8)
else:
    # Get approx number of desired Y tics, while maintining nice (factor of 10) intervals:
    ax.yaxis.set_major_locator(ticker.MaxNLocator(approx_target_y_tics)) 
# human-readable Y axis labels:
ax.yaxis.set_major_formatter(ticker.FuncFormatter(y_fmt)) 

if not args.log:
    plt.ylim(0, None)


#### Print useful tables and statistics:
medians_df = df.groupby([category_label, x_label]).median()
mins_df    = df.groupby([category_label, x_label]).min()
print("")
print("======================== Medians per variant ========================")
print(medians_df)
print("")
medians_grid_df = medians_df.pivot_table(index=category_label, columns=x_label, values=args.y_label)
mins_grid_df    =    mins_df.pivot_table(index=category_label, columns=x_label, values=args.y_label)


#### Generate HTML table reports 
def html_tablify(pivoted_df, caption):
    styles = [
        dict(selector="tr, td, caption", props=[("color", "#777"), ("line-height", "1.6"), ("font-family", "Helvetica, Arial, sans-serif")]),
        dict(selector="th" , props=[("background-color", "#e0e0e0"), ("color", "#000"), ("text-align", "left")]),
        dict(selector="td" , props=[("border-width", "0 0 1px 0"), ("border-bottom", "1px solid #cbcbcb")]),
        dict(selector="caption", props=[("caption-side", "top"), ("padding", "20px"), ("font-size", "130%")])
    ]
    def highlight_pct_change(s):
        if args.baseline_variant:
            baseline = s[args.baseline_variant.replace("_", " ")]
            if pd.isnull(baseline):
                baseline = s.max()
        else:
            baseline = s.max()
        def color(v):
            if baseline == 0:
                pct_chg = 0
            else:
                pct_chg = (v - baseline) / baseline 

            # insignificant
            if pd.isnull(v):
                return ''
            elif abs(pct_chg) < 0.02:
                return ''
            # negative (faster/good)
            elif pct_chg > 0:
                if abs(pct_chg) < 0.05:
                    return 'background-color: #da080826'
                if abs(pct_chg) < 0.20:
                    return 'background-color: #da080870'
                if abs(pct_chg) < 0.50:
                    return 'background-color: #da0808; color: white'
                else:
                    return 'background-color: #a00101; color: white'
            # positive (slower/bad)
            else:
                if abs(pct_chg) < 0.05:
                    return 'background-color: #08da0826'
                if abs(pct_chg) < 0.20:
                    return 'background-color: #08da0870'
                if abs(pct_chg) < 0.50:
                    return 'background-color: #08da08; color: white'
                else:
                    return 'background-color: #01a001; color: white'

        return [color(v) for v in s]

    return (pivoted_df.style
             .set_table_styles(styles) 
             .set_caption(caption)
             # Hide NaNs:
             .applymap(lambda x: 'color: white' if pd.isnull(x) else '') 
             .apply(highlight_pct_change, axis=1)
             .render())

table_path = args.data_dir_path + '/table.html'
with open(table_path,'w') as f:
    f.write(html_tablify(medians_grid_df, "Medians"))
    f.write(html_tablify(mins_grid_df,    "Minimums"))
    f.close()
    print("Saved data table to '%s'" % table_path)
    pass


#### Save the plot to a file:
# NOTE: won't work without python3-tk:
# plt.show()
# SVG also works, but unsurprisingly performance is terrible when we add 'stripplot' above:
plot_path = args.data_dir_path + "/plot.png"
plt.savefig(plot_path, format="png") 
print("Saved graph to '%s'" % plot_path)


###############################################################################
#
# Here (IYI) is a draft of a solution using plot.ly. It generates an
# interactive graph which is nice but I couldn't get it to:
#   - not perform terribly when plotting 10Ks of data points (scattergl would help)
#   - show percentiles other than quartiles from box plot
#   - in general get a tight layout that made comparisons easy

# # Additional imports needed
# import plotly.graph_objects as go 
# import plotly.express as px
#
# # Mostly all copy-pasta from https://plot.ly/python/violin/ 
# #   There's lots of great stuff in the demos on that page, e.g. the "Ridgeline
# # Plot" would be a great way to visualize performance at different concurrency
# # levels.
# if len(params) == 2:
#     dim1_label, dim2_label = params
#     fig = px.violin(df, y=y_label, x=dim2_label, color=dim1_label
#                     , points="all"
#                     , box=True  
#                     # , hover_data=df.columns
#                     , hover_name=y_label
#                     )
# else: # one param
#     dim_label = params
#     fig = px.violin(df, y=y_label, x=dim_label
#                     , points="all"
#                     , box=True  
#                     # , hover_data=df.columns
#                     , hover_name=y_label
#                     )
# fig.update_traces(meanline_visible=True)
# # TODO if < 10000 samples do size 2  ??
# fig.update_traces(marker=dict(size=1.5))
# fig.show()


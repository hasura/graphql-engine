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
parser.add_argument('data_dir_path',
                    help='The path to the directory containing CSV files')
parser.add_argument('y_label', help='Label for the Y axis (i.e. description of data points)')
# The remaining arguments correspond to the dot-separated parts of the sample
# filenames found in data_dir_path.
parser.add_argument('x_label', help='See README.md')
parser.add_argument('category_label', help='See README.md')
args = parser.parse_args()


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

error_cnt = 0
for path in data_files:
    dim1, dim2, _ = prettify(os.path.basename(path).split('.'))
    with open(path) as f:
        for val in f:
            try:
                data.append({args.y_label: float(val)
                            ,args.x_label: dim1
                            ,args.category_label: dim2
                            })
            except: 
                error_cnt = error_cnt + 1

# TODO better stats and reporting:
if error_cnt > 0:
    print('Ignored bad samples: ' + str(error_cnt))

df = pd.DataFrame(data)


sns.set_style("whitegrid")
plt.figure(figsize=(18,10))

# Plot raw points lightly under everything
# NOTE: this is pretty slow:
ax = sns.stripplot(x=args.category_label, y=args.y_label, hue=args.x_label, data=df, palette=sns.color_palette(["#AAAAAA"]), 
                   jitter=0.40,
                   size=2.2,
                   dodge=True) 
plt.setp(ax.collections, zorder=-1000) 
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
    ax = sns.boxplot(x=args.category_label, y=args.y_label, hue=args.x_label, data=df, 
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

# TODO add median value labels (this has been really frustrating)

# This will get overwritten; add back below:
pctl_legend = plt.legend(title='Percentile markers', loc='upper left', 
                         handles=[mpatches.Patch(color=c, label=str(pctl)+"th") for pctl, c in percentiles]+
                                 [mpatches.Patch(color="black", label='median')]
                        )

# See: https://seaborn.pydata.org/generated/seaborn.violinplot.html 
sns.violinplot(x=args.category_label, y=args.y_label, hue=args.x_label, data=df, palette="Set1", 
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

# Get approx number of desired Y tics, while maintining nice (factor of 10)
# intervals:
ax.yaxis.set_major_locator(ticker.MaxNLocator(approx_target_y_tics)) 
# human-readable Y axis labels:
ax.yaxis.set_major_formatter(ticker.FuncFormatter(y_fmt)) 

if not args.log:
    plt.ylim(0, None)



# NOTE: won't work without python3-tk:
# plt.show()
# SVG also works, but unsurprisingly performance is terrible when we add 'stripplot' above:
plt.savefig("plot.png", format="png") 
print("Saved graph to 'plot.png'")

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


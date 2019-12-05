#!/usr/bin/env python3

# Avoid tkinter dependency
import matplotlib
matplotlib.use('agg')
import dash
from plotly.tools import mpl_to_plotly
from dash.dependencies import Input, Output, State
import dash_core_components as dcc
import dash_bootstrap_components as dbc
import dash_html_components as html
from urllib.request import urlopen
import pandas as pd
import json

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.ticker as ticker
import seaborn as sns
from io import BytesIO
import sys
import argparse
import base64

def as_pairs(l, pair_size=2):
    if len(l) < pair_size:
        return [l]
    else:
        return [l[0:pair_size]] + as_pairs(l[pair_size:], pair_size)


def seaborn_violin_plot_data(bench_results, scenarios, y_label, x_label, category_label):
    data = []
    for snro in scenarios:
        def allowed_value(x):
            for k in snro:
                if x[k] != snro[k]:
                    return False
            return True
        req_results = [x for x in bench_results if allowed_value(x)]
        ver_info = snro['version'] or snro['docker_image'].split(':')[1]
        if req_results:
            latencies = urlopen(req_results[0]['latencies_uri']).readlines()
            for x in latencies:
                val_ms = float(x.decode())/1000.0
                data.append({
                    y_label: val_ms,
                    x_label: ver_info,
                    category_label: snro['requests_per_sec']
                })
    return pd.DataFrame(data)


def seaborn_violin_plot_figure(df, y_label, x_label, category_label):
    sns.set_style("whitegrid")
    plt.figure(figsize=(14,6))

    # All points latency plot
    ax = sns.stripplot(
        x=x_label, y=y_label, hue=category_label,
        data=df, palette=sns.color_palette(["#AAAAAA"]),
        jitter=0.40, size=2.2, dodge=True)
    plt.setp(ax.collections, zorder=-1000)

    # Plot percentiles using boxplot whisker caps:
    percentiles = [(99.9, 'red'), (99, 'blue'), (95, 'green')]
    for pctl, color in percentiles:
        ax = sns.boxplot(
            x=x_label, y=y_label, hue=category_label, data=df,
            showfliers=False, showbox=False,
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
    pctl_legend = plt.legend(
        title='Percentile markers', loc='upper left',
        handles=[
            mpatches.Patch(color=c, label=str(pctl)+"th")
            for pctl, c in percentiles
        ] +
        [mpatches.Patch(color="black", label='median')]
    )

    # See: https://seaborn.pydata.org/generated/seaborn.violinplot.html
    sns.violinplot(
        x=x_label, y=y_label, hue=category_label, data=df, palette="Set1",
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

    approx_target_y_tics = 20

    ax.yaxis.set_major_locator(ticker.MaxNLocator(approx_target_y_tics))

    ax.yaxis.set_major_formatter(ticker.FuncFormatter(y_fmt))

    plt.ylim(0, None)
    out_img = BytesIO()
    plt.savefig(out_img, format='png', bbox_inches='tight', dpi=100)
    out_img.seek(0)
    encoded = base64.b64encode(out_img.read()).decode('ascii').replace('\n','')
    plt.close()
    return "data:image/png;base64,{}".format(encoded)

def get_seaborn_hdr_histogram_figure(df):
    sns.set_style("whitegrid")
    fig, ax = plt.subplots(figsize=(14,6))
    sns.lineplot(x='percentile', y='latency', hue='version', style='requests/sec',
                 data=df, ax=ax)
    ax.grid()
    ax.set(
        xlabel='Percentile',
        ylabel='Latency (ms)'
    )
    ax.set_xscale('logit')
    ticks = [0.25, 0.5, 0.9, 0.99, 0.999, 0.9999, 0.99999]
    plt.xticks(ticks)
    majors = ['25%', '50%', '90%', '99%' , '99.9%', '99.99%']
    ax.xaxis.set_major_formatter(ticker.FixedFormatter(majors))
    ax.xaxis.set_minor_formatter(ticker.NullFormatter())
    out_img = BytesIO()
    plt.savefig(out_img, format='png', bbox_inches='tight', dpi=100)
    out_img.seek(0)
    encoded = base64.b64encode(out_img.read()).decode('ascii').replace('\n','')
    plt.close()
    return "data:image/png;base64,{}".format(encoded)


# TODO use logit x-axis scale for hdr histogram
def get_seaborn_hdr_histogram_data(bench_results, scenarios):
    data = []
    for snro in scenarios:
        def allowed_value(x):
            for k in snro:
                if x[k] != snro[k]:
                    return False
            return True
        req_results = [x for x in bench_results if allowed_value(x)]
        ver_info = snro['version'] or snro['docker_image'].split(':')[1]
        if req_results:
            histogram = req_results[0]['latency_histogram']
            for e in histogram:
                data.append({
                    'latency': float(e['latency']),
                    'percentile': float(e['percentile']),
                    'requests/sec': snro['requests_per_sec'],
                    'version': ver_info
                })
    return pd.DataFrame(data)


# Human friendly Y labels.
# Copy-pasta: https://stackoverflow.com/a/40573071
def y_fmt(y, pos):
    decades = [1e9, 1e6, 1e3, 1e0, 1e-3, 1e-6, 1e-9 ]
    suffix  = ["G", "M", "k", "" , "m" , "u", "n"]
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
    return y

def get_violin_plot_data(bench_results, scenarios):
    x_vals = []
    y_vals = []
    for snro in scenarios:
        def allowed_value(x):
            for k in snro:
                if x[k] != snro[k]:
                    return False
            return True
        req_results = [x for x in bench_results if allowed_value(x)]
        ver_info = snro['version'] or snro['docker_image'].split(':')[1]
        if req_results:
            latencies = urlopen(req_results[0]['latencies_uri']).readlines()
            for x in latencies:
                val_ms = float(x.decode())/1000.0
                y_vals.append(val_ms)
                x_vals.append(ver_info)
    return [{
        'type': 'violin',
        'x': x_vals,
        'y': y_vals,
        'points': False
    }]

def get_histogram_data(bench_results, scenarios):
    data = []
    for snro in scenarios:
        def allowed_value(x):
            for k in snro:
                if x[k] != snro[k]:
                    return False
            return True
        req_results = [x for x in bench_results if allowed_value(x)]
        ver_info = snro['version'] or snro['docker_image'].split(':')[1]
        if req_results:
            histogram = req_results[0]['latency_histogram']
            dataRow = {
                'x' : [1/(1.0-float(x['percentile'])) for x in histogram if float(x['percentile']) < 1],
                'y' : [float(y['latency']) for y in histogram],
                'type': 'line',
                'name': ver_info
            }
            data.append(dataRow)
    return data

def run_dash_server(bench_results):

    bench_results.sort(key=lambda x : x['version'] or x['docker_image'])

    app = dash.Dash(external_stylesheets=[dbc.themes.BOOTSTRAP])

    uniq_queries = list( set( [ x['query_name'] for x in bench_results ] ) )
    children = []


    rows = []
    plot_types = ['histogram','violin plot']
    plots_filters_1 = [
        dbc.Col([
            html.Label('Plot type'),
            dcc.Dropdown(
                id='plot-type',
                options=[{'label':q, 'value': q} for q in plot_types],
                value= plot_types[0]
            )
        ]),
        dbc.Col([
            html.Label('Query'),
            dcc.Dropdown(
                id='query-name',
                options=[{'label':q, 'value': q} for q in uniq_queries],
                value= uniq_queries[0]
            )
        ]),
        dbc.Col([
            html.Label('Requests/sec'),
            dcc.Dropdown(
                id='rps',
                multi=True
            )
        ])
    ]
    rows.append(dbc.Row(plots_filters_1))

    plots_filter_2 = [
        dbc.Col([
            html.Label('version(s)/docker_image(s)'),
            dcc.Dropdown(
                id='ver',
                multi=True
            )
        ])
    ]
    rows.append(dbc.Row(plots_filter_2))



    graph = dbc.Col(
        html.Div([html.Img(id = 'graph', src = '')], id='plot_div')
    )
    rows.append(dbc.Row(graph))

    children.append(html.Div(dbc.Container(rows)))

    app.layout = html.Div(children=children)

    @app.callback(
        Output('ver', 'options'),
        [ Input('query-name', 'value'), Input('rps', 'value') ]
    )
    def updateVerOptions(query_name, rps_list):
        relvnt_q = [ x for x in bench_results if x['query_name'] ==  query_name and x['requests_per_sec'] in rps_list ]
        uniq_vers = list(set([
            (x['version'], x['docker_image'])
            for x in relvnt_q
        ]))
        return [
            {
                'label': x[0] or x[1],
                'value': json.dumps({
                    'version': x[0],

                    'docker_image': x[1]
                    })
            }
            for x in uniq_vers
        ]

    @app.callback(
        Output('ver', 'value'),
        [ Input('ver', 'options') ],
        [ State('ver', 'value') ]
    )
    def updateVerValue(options, vers):
        new_vers = []
        allowed_vers = [ o['value'] for o in options]
        default_vers = allowed_vers[:min(2, len(allowed_vers))]
        if not vers:
            return default_vers
        for ver in vers:
            if ver in allowed_vers:
              new_vers.append(ver)
        if new_vers:
            return new_vers
        else:
            return default_vers


    @app.callback(
        Output('rps', 'options'),
        [ Input('query-name', 'value') ]
    )
    def updateRPSOptions(query_name):
        relvnt_q = [ x for x in bench_results if x['query_name'] ==  query_name ]
        rps = list( set( [x['requests_per_sec'] for x in relvnt_q ] ) )
        return [
            {
                'label':  str(x),
                'value': x
            } for x in sorted(rps)
        ]

    @app.callback(
        Output('rps', 'value'),
        [ Input('rps', 'options') ],
        [ State('rps', 'value') ]
    )
    def updateRPSValue(options, rps_list):
        new_rps_list = []
        allowed_rps_list = [ o['value'] for o in options]
        default_rps_list = allowed_rps_list[:min(2, len(allowed_rps_list))]
        if not rps_list:
            return default_rps_list
        if rps not in rps_list:
            if rps in allowed_rps_list:
                new_rps_list.append(rps)
        if new_rps_list:
            return new_rps_list
        else:
            return default_rps_list


    def get_seaborn_histogram_figure(scenarios):
        df = get_seaborn_hdr_histogram_data(bench_results, scenarios)
        return get_seaborn_hdr_histogram_figure(df)

    def get_histgoram_figure(scenarios):
        return {
            'data': get_histogram_data(bench_results, scenarios),
            'layout': {
                'yaxis' : {
                    'title': "Latency (ms)"
                },
                'xaxis' : {
                    'title': "Percentile",
                    'type': 'log',
                    'tickvals': [1.0,2.0,10.0,100.0,1000.0,10000.0,10000.0,100000.0],
                    'ticktext': ['0%','50%','90%','99%','99.9%','99.99%','99.999%','99.9999%']
                },
                'title' : 'Latency histogram'
            }
        }

    def get_seaborn_violin_figure(scenarios):
        y_label = 'latency (ms)'
        x_label = 'version'
        category_label = 'req/sec'
        df = seaborn_violin_plot_data(bench_results, scenarios, y_label, x_label, category_label)
        return seaborn_violin_plot_figure(df, y_label, x_label, category_label)

    def get_violin_figure(scenarios):
        return {
            'data': get_violin_plot_data(bench_results, scenarios),
            'layout': {
                'yaxis' : {
                    'title': "Latency (ms)"
                },
                'xaxis': {
                    'title': 'version/docker_image'
                },
                'title': 'Latency'
            }
        }

    @app.callback(
        Output('graph', 'src'),
        [
            Input('plot-type', 'value'),
            Input('query-name', 'value'),
            Input('rps', 'value'),
            Input('ver', 'value')
        ]
    )
    def updateGraph(plot_type, query_name, rps_list, vers):
        scenarios = [
            {
                'query_name': query_name,
                'requests_per_sec': rps,
                **json.loads(v)
            }
            for v in set(vers)
            for rps in set(rps_list)
        ]
        print('Scenario:', scenarios)
        if plot_type == 'histogram':
            return get_seaborn_histogram_figure(scenarios)
        elif plot_type == 'violin plot':
            return get_seaborn_violin_figure(scenarios)

    app.run_server(host="127.0.0.1", debug=False)

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--results', nargs='?', type=argparse.FileType('r'),
        default=sys.stdin)
    args = parser.parse_args()
    bench_results = json.load(args.results)
    print(bench_results)

    print("=" * 20)
    print("starting dash server for graphs")

    run_dash_server(bench_results)

#!/usr/bin/env python3

# Avoid tkinter dependency
import matplotlib
matplotlib.use('agg')

import dash
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
from urllib.parse import urlparse
import boto3

def as_pairs(l, pair_size=2):
    if len(l) < pair_size:
        return [l]
    else:
        return [l[0:pair_size]] + as_pairs(l[pair_size:], pair_size)


def get_scenario_results(results, scenarios):
    out_results = []
    for snro in scenarios:
        def allowed_value(x):
            for k in snro:
                if x[k] != snro[k]:
                    return False
            return True
        req_results = [x for x in results if allowed_value(x)]
        out_results.append((snro, req_results))
    return out_results

def throughput_data(max_rps_results, scenarios):
    results = get_scenario_results(max_rps_results, scenarios)
    data = []
    for (snro, req_results) in results:
        ver_info = snro['version'] or snro['docker_image'].split(':')[1]
        if req_results:
            data.append({
                'version': ver_info,
                'query name': snro['query_name'],
                'max throughput': float(req_results[0]['max_rps'])
            })
    return pd.DataFrame(data)

def throughput_figure(df):
    sns.set_style("whitegrid")
    fig, ax = plt.subplots(figsize=(14,6))
    uniq_versions = set(df['version'])
    if len(uniq_versions) >1:
        sns.lineplot(x='version', y='max throughput', hue='query name', data=df, ax=ax)
    else:
        sns.barplot(x='version', y='max throughput', hue='query name', data=df, ax=ax)
    ax.grid()
    ax.set(
        xlabel='Version/Docker image',
        ylabel='Throughput (req/s)'
    )
    ymax = round(max(df['max throughput'])*1.05)
    plt.ylim(0, ymax)
    out_fig = gen_plot_figure_data(plt)
    plt.close()
    return out_fig

def gen_plot_figure_data(plt):
    with BytesIO() as out_img:
        plt.savefig(out_img, format='png', bbox_inches='tight', dpi=100)
        out_img.seek(0)
        encoded = base64.b64encode(out_img.read()).decode('ascii').replace('\n','')
    return "data:image/png;base64,{}".format(encoded)


def uri_readlines(uri):
    print('Latency file:', uri)
    p = urlparse(uri)
    if p.scheme == 'file':
        return urlopen(uri).readlines()
    elif p.scheme == 's3':
        s3 = boto3.resource('s3')
        obj = s3.Object(bucket_name=p.netloc, key=p.path.lstrip('/'))
        with BytesIO() as data:
            obj.download_fileobj(data)
            return data.getvalue().splitlines()


def violin_plot_data(latency_results, scenarios):
    y_label = 'latency (ms)'
    x_label = 'version'
    category_label = 'req/sec'
    data = []
    results = get_scenario_results(latency_results, scenarios)
    for (snro, req_results) in results:
        ver_info = snro['version'] or snro['docker_image'].split(':')[1]
        if req_results:
            latencies = uri_readlines(req_results[0]['latencies_uri'])
            for x in latencies:
                if isinstance(x, bytes):
                    x = x.decode()
                val_ms = float(x)/1000.0
                data.append({
                    y_label: val_ms,
                    x_label: ver_info,
                    category_label: snro['requests_per_sec']
                })
    return pd.DataFrame(data)


def violin_plot_figure(df):
    y_label = 'latency (ms)'
    x_label = 'version'
    category_label = 'req/sec'
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
    out_fig = gen_plot_figure_data(plt)
    plt.close()
    return out_fig

def hdrhistogram_figure(df):
    sns.set_style("whitegrid")
    fig, ax = plt.subplots(figsize=(14,6))
    sns.lineplot(x='percentile', y='latency', hue='version',
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
    out_fig = gen_plot_figure_data(plt)
    plt.close()
    return out_fig


def hdrhistogram_data(latency_results, scenarios):
    results = get_scenario_results(latency_results, scenarios)
    data = []
    for (snro, req_results) in results:
        ver_info = snro['version'] or snro['docker_image'].split(':')[1]
        if req_results:
            histogram = req_results[0]['latency_histogram']
            for e in histogram:
                data.append({
                    'latency': float(e['latency']),
                    'percentile': float(e['percentile']),
                    'req/sec': snro['requests_per_sec'],
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

def run_dash_server(bench_results):
    latency_results = bench_results['latency']
    max_rps_results = bench_results['max_rps']
    latency_results.sort(key=lambda x : x['version'] or x['docker_image'])

    app = dash.Dash(external_stylesheets=[dbc.themes.BOOTSTRAP])

    uniq_queries = list( set( [ x['query_name'] for x in latency_results ] ) )
    children = []


    rows = []
    plot_types = ['latency histogram','latency violins', 'max throughput']
    plots_filters_1 = [
        dbc.Col([
            html.Label('Plot type'),
            dcc.Dropdown(
                id='plot-type',
                options=[{'label':q, 'value': q} for q in plot_types],
                value= plot_types[2]
            )
        ], width=2),
        dbc.Col([
            html.Label('Query'),
            dcc.Dropdown(
                id='query-name',
                options=[{'label':q, 'value': q} for q in uniq_queries],
                value= uniq_queries[:min(len(uniq_queries), 4)],
                multi=True
            )
        ]),
        dbc.Col(html.Div(
            children=[
                html.Label('Requests/sec'),
                dcc.Dropdown(id='rps', multi=False)

            ],
            id='rps-div'
        ), width=3)
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

    def as_list(x):
        if not isinstance(x, list):
            return [x]
        else:
            return x

    @app.callback(
        Output('ver', 'options'),
        [ Input('plot-type', 'value'), Input('query-name', 'value'), Input('rps', 'value') ]
    )
    def updateVerOptions(plot_type, query_names, rps_list):
        query_names = as_list(query_names)
        rps_list = as_list(rps_list)
        relvnt_q = [
            x for x in latency_results
            if x['query_name'] in query_names and
            (x['requests_per_sec'] in rps_list or plot_type == 'max throughput')
        ]
        uniq_vers = list(set([
            (x['version'], x['docker_image'])
            for x in relvnt_q
        ]))
        print("Updating version options to", uniq_vers)
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
        [ Input('plot-type', 'value'), Input('ver', 'options') ],
        [ State('ver', 'value') ]
    )
    def updateVerValue(plot_types, options, vers):
        print('Updating version value, options', options)
        return updateMultiValue(options, vers)

    def updateMultiValue(options, vals, def_list_size=2):
        vals = as_list(vals)
        new_vals = []
        allowed_vals = [ x['value'] for x in options]
        def_val_size = min(def_list_size, len(allowed_vals))
        default_vals = allowed_vals[:def_val_size]
        if not vals:
            return default_vals
        for val in vals:
            if val in allowed_vals:
                new_vals.append(val)
        if new_vals:
            return new_vals
        else:
            return default_vals

    def updateSingleValue(options, val):
        allowed_vals = [ x['value'] for x in options]
        if val and val in allowed_vals:
            return val
        elif allowed_vals:
            return allowed_vals[0]
        else:
            return None

    # Change queries to multi for throughput plot
    @app.callback(
        Output('query-name', 'multi'),
        [ Input('plot-type', 'value') ],
    )
    def query_dropdown_multi(plot_type):
        return plot_type == 'max throughput'

    @app.callback(
        Output('query-name', 'value'),
        [ Input('plot-type', 'value'), Input('query-name', 'options'), Input('query-name', 'multi') ],
        [ State('query-name', 'value') ]
    )
    def updateQueryValue(plot_type, options, multi, query_names):
        if plot_type == 'max throughput':
            return updateMultiValue(options, query_names, 4)
        else:
            return updateSingleValue(options, query_names)


    @app.callback(
        Output('rps', 'options'),
        [ Input('plot-type', 'value'),  Input('query-name', 'value') ]
    )
    def updateRPSOptions(plot_type, query_name):
        relvnt_q = [ x for x in latency_results if x['query_name'] ==  query_name ]
        rps = list( set( [x['requests_per_sec'] for x in relvnt_q ] ) )
        return [
            {
                'label':  str(x),
                'value': x
            } for x in sorted(rps)
        ]

    @app.callback(
        Output('rps', 'value'),
        [ Input('plot-type', 'value'), Input('rps', 'options') ],
        [ State('rps', 'value') ]
    )
    def updateRPSValue(plot_type, options, rps):
        if plot_type == 'latency histogram':
            return updateSingleValue(options, rps)
        else:
            rps = as_list(rps)
            return updateMultiValue(options, rps)

    # Change RPS to multi for violin plot
    @app.callback(
        Output('rps', 'multi'),
        [ Input('plot-type', 'value') ],
    )
    def rps_dropdown_multi(plot_type):
        return plot_type == 'latency violins'

    # Hide RPS dropdown if plot type is throughput
    @app.callback(
        Output('rps-div', 'style'),
        [ Input('plot-type', 'value') ],
    )
    def rps_dropdown_style(plot_type):
        if plot_type == 'max throughput':
            return { 'display': 'none' }
        else:
            return {'display': 'block'}

    def get_hdrhistogram_figure(scenarios):
        df = hdrhistogram_data(latency_results, scenarios)
        return hdrhistogram_figure(df)

    def get_violin_figure(scenarios):
        df = violin_plot_data(latency_results, scenarios)
        return violin_plot_figure(df)

    def get_throughput_figure(scenarios):
        df = throughput_data(max_rps_results, scenarios)
        return throughput_figure(df)

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
        rps_list = as_list(rps_list)
        def latency_scenarios():
            return [
                {
                    'query_name': query_name,
                    'requests_per_sec': rps,
                    **json.loads(v)
                }
                for v in set(vers)
                for rps in set(rps_list)
            ]
        def throughput_scenarios():
            return [
                {
                    'query_name': qname,
                    **json.loads(v)
                }
                for v in set(vers)
                for qname in as_list(query_name)
            ]
        if plot_type == 'max throughput':
            scenarios = throughput_scenarios()
        else:
            scenarios = latency_scenarios()


        if plot_type == 'latency histogram':
            return get_hdrhistogram_figure(scenarios)
        elif plot_type == 'latency violins':
            return get_violin_figure(scenarios)
        elif plot_type == 'max throughput':
            return get_throughput_figure(scenarios)

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

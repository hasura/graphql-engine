#!/usr/bin/env python3

import dash
from dash.dependencies import Input, Output
import dash_core_components as dcc
import dash_html_components as html

import json

import sys
import argparse

def as_pairs(l, pair_size=2):
    if len(l) < pair_size:
        return [l]
    else:
        return [l[0:pair_size]] + as_pairs(l[pair_size:], pair_size)

def get_data(bench_results, queryNames):
    print(queryNames)
    data = []
    for queryName in queryNames:
        req_results = [x for x in bench_results if x['query_name'] == queryName ]
        if len(req_results) > 1:
            type = 'line'
        else:
            type = 'bar'
        dataRow = {
            'x' : [y['version'] or y['docker_image'].split(':')[1] for y in req_results],
            'y' : [y['requests_per_sec'] for y in req_results],
            'type': type,
            'name': queryName
        }
        data.append(dataRow)
    return data

def run_dash_server(bench_results):

    bench_results.sort(key=lambda x : x['version'] or x['docker_image'])

    app = dash.Dash()

    uniq_queries = list( set( [ x['query_name'] for x in bench_results ] ) )

    children = []
    for i in range(1,5):
        children.extend([
            html.Label('Query {}'.format(i)),
            dcc.Dropdown(
                id='query-name-{}'.format(i),
                options=[{'label':q, 'value': q} for i, q in enumerate([''] + uniq_queries)],
                value= uniq_queries[i-1]  if i <= len(uniq_queries) else ''
            )
        ])
    children.append(dcc.Graph(id='rps-vs-version'))

    app.layout = html.Div(children=children)

    @app.callback(
        Output('rps-vs-version', 'figure'),
        [ Input('query-name-{}'.format(i), 'value') for i in range(1,5) ]
    )
    def updateGraph(*args):
        queryNames = [ q for q in args if q ]
        figure={
            'data': get_data(bench_results, queryNames),
            'layout': {
                'yaxis' : {
                    'title': "Requests/sec"
                },
                'xaxis' : {
                    'title': "Version/Docker image"
                },
                'title' : 'Requests/sec for queries ' + str(queryNames)
            }
        }
        return figure

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

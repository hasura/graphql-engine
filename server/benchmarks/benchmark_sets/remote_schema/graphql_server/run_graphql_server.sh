#!/bin/sh

set -e

pip install -r requirements-graphql-server.txt

python graphql_server.py

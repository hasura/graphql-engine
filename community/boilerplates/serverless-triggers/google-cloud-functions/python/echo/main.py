import sys
import json

def function(request):
    request_json = request.json
    op = request_json.get('event/op')
    data = request_json.get('event/data')
    name = request_json.get('table/name')
    schema = request_json.get('table/schema')
    response = json.dumps({'message': 'received event', 'data': {op, data, name, schema}})
    print(response)
    return response

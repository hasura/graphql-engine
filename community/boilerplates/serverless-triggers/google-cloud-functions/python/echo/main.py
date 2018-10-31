from flask import jsonify


def function(request):
    request_json = request.get_json()
    op = request_json['event']['op']
    data = request_json['event']['data']
    name = request_json['table']['name']
    schema = request_json['table']['schema']
    response = {'message': 'received event', 'data': {op, data, name, schema}}
    print(response)
    return jsonify(message='received event', data={'op': op, 'data': data, 'name': name, 'schema': schema})

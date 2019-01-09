import json


def lambda_handler(event, context):
    try:
        body = json.loads(event['body'])
    except:
        return {
            "statusCode": 400,
            "body": json.dumps({'message': 'Unable to parse hasura event'})
        }

    message = 'Not able to process request'
    data = body['event']['data']

    if body['table']['name'] == 'notes' and body['event']['op'] == 'INSERT':
        message = 'New note {} inserted, with data: {}'.format(data['new']['id'], data['new']['note'])

    elif body['table']['name'] == 'notes' and body['event']['op'] == 'UPDATE':
        message = 'Note {} updated, with data: {}'.format(data['new']['id'], data['new']['note'])

    elif body['table'] == 'notes' and body['op'] == 'DELETE':
        message = 'Note {} deleted, with data: {}'.format(data['old']['id'], data['old']['note'])
    return {
        "statusCode": 200,
        "body": json.dumps({'message': message})
    }

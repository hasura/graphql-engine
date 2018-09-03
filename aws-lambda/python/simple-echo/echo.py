import json

def lambda_handler(event, context):
    try:
        body = json.loads(event['body'])
    except:
        return {
            "statusCode": 400,
            "body": json.dumps({'message': 'Unable to parse request body'})
        }
    message = 'Not able to process request'
    data = body['data']
    if body['table'] == 'notes' and body['op'] == 'INSERT':
        message = 'New note {} inserted, with data: {}'.format(data['id'], data['note'])
    elif body['table'] == 'notes' and body['op'] == 'UPDATE':
        message = 'Note {} updated, with data: {}'.format(data['id'], data['note'])
    elif body['table'] == 'notes' and body['op'] == 'DELETE':
        message = 'Note {} deleted, with data: {}'.format(data['id'], data['note'])
    return {
        "statusCode": 200,
        "body": json.dumps({'message': message})
    }

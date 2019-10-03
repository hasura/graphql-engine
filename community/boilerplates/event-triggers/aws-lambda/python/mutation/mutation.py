import os
import json
from botocore.vendored import requests

ADMIN_SECRET = os.environ['ADMIN_SECRET']
HGE_ENDPOINT = os.environ['HGE_ENDPOINT']
HGE_URL = HGE_ENDPOINT + '/v1/graphql'

HEADERS = {
    'Content-Type': 'application/json',
    'X-Hasura-Admin-Secret': ADMIN_SECRET,
}

query = """
mutation updateNoteRevision ($noteId: Int!, $data: String!) {
  insert_note_revision (objects: [
    {
      note_id: $noteId,
      note: $data
    }
  ]) {
    affected_rows
  }
}
"""


def lambda_handler(event, context):
    try:
        body = json.loads(event['body'])
    except:
        return {
            "statusCode": 400,
            "body": json.dumps({'message': 'Unable to parse request body'})
        }
    data = body['event']['data']
    qv = {'noteId': data['old']['id'], 'data': data['old']['note']}
    jsonBody = {'query': query, 'variables': qv}

    resp = requests.post(HGE_URL, data=json.dumps(jsonBody), headers=HEADERS)
    my_json = resp.json()
    print(my_json)
    return {
        "statusCode": 200,
        "body": json.dumps({'message': 'success'})
    }

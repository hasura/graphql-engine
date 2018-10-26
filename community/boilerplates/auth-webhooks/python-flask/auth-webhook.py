from flask import Flask
from flask import request, jsonify, abort

app = Flask(__name__)


def get_details_for_token(token):
    # execute some logic (say contacting a 3rd party API) to resolve the token
    # to X-Hasura-Role and other variables like X-Hasura-User-Id
    # Here as an example, we return user, 1
    variables = {
        'X-Hasura-Role': 'user',
        'X-Hasura-User-Id': '1'
    }

    # if the request should be rejected, say due to an invalid token, the
    # response should be 403, Unauthorized. In this example if variables are
    # None, we return 401, Unauthorized
    # return None

    return variables


@app.route('/')
def hello():
    return 'webhook is running'


@app.route('/auth-webhook')
def auth_webhook():
    # get the auth token from Authorization header
    token = request.headers.get('Authorization')
    # similarly you can access all headers sent in the request. Hasura forwards
    # all request headers to the webhook

    # get the role and other variables for this token
    variables = get_details_for_token(token)

    if variables is not None:
        # allow the graphql request with variables
        return jsonify(variables)
    else:
        # reject the graphql request
        return abort(401)

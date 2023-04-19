import pytest


class TestConfigApiWithAnInsecureServer:
    def test_responds_correctly(self, hge_ctx):
        headers = {
            'x-hasura-role': 'admin',
        }
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)
        assert resp.status_code == 200, resp

        body = resp.json()
        print('Body:', body)
        assert body['is_admin_secret_set'] == False
        assert body['is_auth_hook_set'] == False
        assert body['is_jwt_set'] == False
        assert body['jwt'] == []

    def test_rejects_an_invalid_role(self, hge_ctx):
        headers = {
            'x-hasura-role': 'user',
        }
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)
        assert resp.status_code == 400, resp


@pytest.mark.admin_secret
class TestConfigApiWithAdminSecret:
    def test_responds_correctly(self, hge_ctx, hge_key):
        headers = {
            'x-hasura-role': 'admin',
            'x-hasura-admin-secret': hge_key,
        }
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)
        assert resp.status_code == 200, resp

        body = resp.json()
        print('Body:', body)
        assert body['is_admin_secret_set'] == True
        assert body['is_auth_hook_set'] == False
        assert body['is_jwt_set'] == False
        assert body['jwt'] == []

    def test_rejects_an_invalid_role(self, hge_ctx, hge_key):
        headers = {
            'x-hasura-role': 'user',
            'x-hasura-admin-secret': hge_key,
        }
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)
        assert resp.status_code == 400, resp

    def test_request_fails_without_auth_headers(self, hge_ctx):
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config')
        assert (resp.status_code == 401) or (resp.status_code == 400)


@pytest.mark.admin_secret
@pytest.mark.usefixtures('auth_hook')
class TestConfigApiWithAuthHook:
    def test_responds_correctly(self, hge_ctx, hge_key):
        headers = {
            'x-hasura-role': 'admin',
            'x-hasura-admin-secret': hge_key,
        }
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)
        assert resp.status_code == 200, resp

        body = resp.json()
        print('Body:', body)
        assert body['is_admin_secret_set'] == True
        assert body['is_auth_hook_set'] == True
        assert body['is_jwt_set'] == False
        assert body['jwt'] == []

    def test_rejects_an_invalid_role(self, hge_ctx, hge_key):
        headers = {
            'x-hasura-role': 'user',
            'x-hasura-admin-secret': hge_key,
        }
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)
        assert resp.status_code == 400, resp

    def test_request_fails_without_auth_headers(self, hge_ctx):
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config')
        assert (resp.status_code == 401) or (resp.status_code == 400)


@pytest.mark.admin_secret
@pytest.mark.usefixtures('jwt_configuration')
@pytest.mark.jwt('ed25519')
class TestConfigApiWithJwtAndNoClaims:
    def test_rejects_an_invalid_role(self, hge_ctx, hge_key):
        headers = {
            'x-hasura-role': 'user',
            'x-hasura-admin-secret': hge_key,
        }
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)
        assert resp.status_code == 400, resp

    def test_responds_correctly(self, hge_ctx, hge_key):
        headers = {
            'x-hasura-role': 'admin',
            'x-hasura-admin-secret': hge_key,
        }
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)
        assert resp.status_code == 200, resp

        body = resp.json()
        print('Body:', body)
        assert body['is_admin_secret_set'] == True
        assert body['is_auth_hook_set'] == False
        assert body['is_jwt_set'] == True

        assert body['jwt'] == [
            {
                'claims_format': 'json',
                'claims_map': None,
                'claims_namespace': 'https://hasura.io/jwt/claims',
            }
        ]

    def test_request_fails_without_auth_headers(self, hge_ctx):
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config')
        assert (resp.status_code == 401) or (resp.status_code == 400)


@pytest.mark.admin_secret
@pytest.mark.usefixtures('jwt_configuration')
@pytest.mark.jwt('ed25519', {
    'claims_format': 'stringified_json',
})
class TestConfigApiWithJwtAndClaimsFormat:
    def test_responds_correctly(self, hge_ctx, hge_key):
        headers = {
            'x-hasura-role': 'admin',
            'x-hasura-admin-secret': hge_key,
        }
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)
        assert resp.status_code == 200, resp

        body = resp.json()
        print('Body:', body)
        assert body['is_admin_secret_set'] == True
        assert body['is_auth_hook_set'] == False
        assert body['is_jwt_set'] == True

        assert body['jwt'] == [
            {
                'claims_format': 'stringified_json',
                'claims_map': None,
                'claims_namespace': 'https://hasura.io/jwt/claims',
            }
        ]


@pytest.mark.admin_secret
@pytest.mark.usefixtures('jwt_configuration')
@pytest.mark.jwt('ed25519', {
    'claims_namespace': 'https://example.org/jwt/claims',
    'claims_format': 'stringified_json',
})
class TestConfigApiWithJwtAndClaimsNamespace:
    def test_responds_correctly(self, hge_ctx, hge_key):
        headers = {
            'x-hasura-role': 'admin',
            'x-hasura-admin-secret': hge_key,
        }
        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)
        assert resp.status_code == 200, resp

        body = resp.json()
        print('Body:', body)
        assert body['is_admin_secret_set'] == True
        assert body['is_auth_hook_set'] == False
        assert body['is_jwt_set'] == True

        assert body['jwt'] == [
            {
                'claims_format': 'stringified_json',
                'claims_map': None,
                'claims_namespace': 'https://example.org/jwt/claims',
            }
        ]

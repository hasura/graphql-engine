import json

class TestConfigAPI:
    def test_config_api_user_role_error(self, hge_ctx, hge_key):
        headers = {'x-hasura-role': 'user'}
        if hge_key is not None:
            headers['x-hasura-admin-secret'] = hge_key

        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)

        assert resp.status_code == 400, resp

    def test_config_api(self, hge_ctx, hge_key):
        jwt_conf = hge_ctx.hge_jwt_conf
        if jwt_conf is not None:
            jwt_conf_dict = json.loads(hge_ctx.hge_jwt_conf)
        else:
            jwt_conf_dict = None

        headers = {'x-hasura-role': 'admin'}
        if hge_key is not None:
            headers['x-hasura-admin-secret'] = hge_key

        resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config', headers=headers)

        assert resp.status_code == 200, resp

        body = resp.json()
        assert body['is_admin_secret_set'] == (hge_key is not None)
        assert body['is_auth_hook_set'] == (hge_ctx.webhook is not None)
        assert body['is_jwt_set'] == (jwt_conf is not None)

        if jwt_conf_dict:
            claims_format = "json"
            if 'claims_namespace_path' in jwt_conf_dict:
                assert body['jwt']['claims_namespace_path'] == jwt_conf_dict['claims_namespace_path']
                assert body['jwt']['claims_format'] == claims_format
            else:
                claims_namespace = "https://hasura.io/jwt/claims"
                if 'claims_namespace' in jwt_conf_dict:
                    claims_namespace = jwt_conf_dict['claims_namespace']
                if 'claims_format' in jwt_conf_dict:
                    claims_format = jwt_conf_dict['claims_format']
                    assert body['jwt']['claims_namespace'] == claims_namespace
                    assert body['jwt']['claims_format'] == claims_format
        else:
            assert body['jwt'] == []

        # test if the request fails without auth headers if admin secret is set
        if hge_key is not None:
            resp = hge_ctx.http.get(hge_ctx.hge_url + '/v1alpha1/config')
            body = resp.json()
            assert ((resp.status_code == 401) or (resp.status_code == 400))

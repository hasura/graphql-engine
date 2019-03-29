import yaml
from validate import check_query_f, check_query
from super_classes import DefaultTestSelectQueries


class TestGraphqlIntrospection(DefaultTestSelectQueries):

    def test_introspection(self, hge_ctx):
        with open(self.dir() + "/introspection.yaml") as c:
            conf = yaml.safe_load(c)
        code, resp = check_query(hge_ctx, conf)
        assert code == 200, resp
        hasArticle = False
        hasArticleAuthorFKRel = False
        hasArticleAuthorManualRel = False
        for t in resp['data']['__schema']['types']:
            if t['name'] == 'article':
                hasArticle = True
                for fld in t['fields']:
                    if fld['name'] == 'author_obj_rel_manual':
                        hasArticleAuthorManualRel = True
                        assert fld['type']['kind'] == 'OBJECT'
                    elif fld['name'] == 'author_obj_rel_fk':
                        hasArticleAuthorFKRel = True
                        assert fld['type']['kind'] == 'NON_NULL'
        assert hasArticle
        assert hasArticleAuthorFKRel
        assert hasArticleAuthorManualRel

    def test_introspection_user(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/introspection_user_role.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_introspection"

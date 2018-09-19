
import pytest
import yaml
from validate import check_query_f

class TestGraphqlIntrospection:

    def test_introspection(self, hge_ctx):
       with open(self.dir + "/introspection.yaml") as c:
         conf = yaml.load(c)
       code, resp = hge_ctx.anyq( conf['url'], conf['query'], {})
       assert code == 200
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
       check_query_f(hge_ctx, self.dir + "/introspection_user_role.yaml")

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = "queries/graphql_introspection"
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

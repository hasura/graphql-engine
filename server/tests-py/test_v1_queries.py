
import pytest
import yaml
from validate import check_query_f

class TestV1Select:

    def test_select_query_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_article.yaml')

    def test_select_col_not_present(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_article_col_not_present_err.yaml')

    def test_select_query_user(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/select_user.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        self.dir = "queries/v1/select/basic"    
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

class TestV1Insert:

    def test_insert_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/insert_author.yaml')

    def test_insert_author_col_not_present_err(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/insert_author_col_not_present_err.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        self.dir = "queries/v1/insert/basic"    
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

class TestV1InsertOnConflict:
   
    def test_upsert_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/upsert_author.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        self.dir = "queries/v1/insert/onconflict"    
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

class TestV1Delete:
 
    def test_delete_author(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/delete_article.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        self.dir = "queries/v1/delete"    
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

class TestMetadata:

    def test_reload_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir + '/reload_metadata.yaml')

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):  
        self.dir = "queries/v1/metadata"    
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

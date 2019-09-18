from http import HTTPStatus
import requests
import json
import re
import graphql

from webserver import RequestHandler, WebServer, MkHandlers, Response

def first(iterable, key, default=None):
   return next( (x for x in iterable if key(x)), default)

def get_introspect_types(introspect):
   return json_get(introspect, ['data', '__schema', 'types'])

def has_name(name):
   return lambda x : x['name'] == name

def get_fld_by_name(flds, name):
   return first(flds, has_name(name))

def get_ty_by_name(types, name):
   return first(types, has_name(name))

def json_get(obj, path, default=None):
   if obj == None:
      return None
   elif len(path) == 0:
      return obj
   elif len(path) == 1:
      return obj.get(path[0], default)
   else:
      return json_get(obj.get(path[0],{ }), path[1:])

def get_base_ty(fld):
   base_ty = fld['type']
   while not base_ty['name']:
      base_ty = base_ty['ofType']
   return base_ty

class GraphQLPrefixerProxy(RequestHandler):
   """
   This proxy adds a prefix to all the object type names (except for the default ones), 
    and also to the top level fields of queries, mutations and subscriptions.

   Further to enable adding this as remote schema to the server it is proxying,
   It also deletes all the types starting with the prefix, and the top-level nodes starting with same prefix.
   """

   def __init__(self, gql_url, headers, prefix):
      self.gql_url = gql_url
      self.headers = headers
      self.prefix = prefix

   def _is_non_def_obj_ty(self, ty):
      return ty['kind'] in ['OBJECT'] and not ty['name'].startswith('__')

   def _is_input_obj_ty(self, ty):
      return ty['kind'] in ['INPUT_OBJECT']

   def _is_enum(self, ty):
      return ty['kind'] == 'ENUM'

   def _add_name_prefix(self, obj):
      assert not obj['name'].startswith(self.prefix), obj
      obj['name'] = self.prefix + obj['name']

   def get(self, request):
      return Response(HTTPStatus.METHOD_NOT_ALLOWED)

   def _assert_prefixes(self, introspect):

      def assert_fld_name(fld):
         return assert_ty_name(fld)

      def assert_ty_name(ty):
         assert ty['name'].startswith(self.prefix), ty
         assert_no_loop(ty)

      def assert_no_loop(ty):
         assert not ty['name'].startswith(self.prefix*2), ty

      def assert_base_ty(elem):
         base_ty = get_base_ty(elem)
         assert_no_loop(base_ty), elem
         return base_ty

      types = get_introspect_types(introspect)
      if not types:
         return

      for ty in types:
         assert_no_loop(ty)
         if self._is_non_def_obj_ty(ty):
            # Ensure name of type starts with prefix
            assert_ty_name(ty)

            # Ensure base type of fields starts with prefix,
            # if base type is object
            for fld in ty['fields']:
               base_ty = assert_base_ty(fld)
               if self._is_non_def_obj_ty(base_ty) or self._is_enum(base_ty):
                  assert_ty_name(base_ty)

               # Ensure base type of args starts with prefix,
               # if base type is input_object
               for arg in fld['args']:
                  base_ty = assert_base_ty(arg)
                  if self._is_input_obj_ty(base_ty) or self._is_enum(base_ty):
                     assert_ty_name(base_ty)

         elif self._is_input_obj_ty(ty):
            # Ensure name of input object starts with prefix
            assert_ty_name(ty)

            for fld in ty['inputFields']:
               base_ty = assert_base_ty(fld)
               if self._is_input_obj_ty(base_ty) or self._is_enum(base_ty):
                  assert_ty_name(base_ty)

         elif self._is_enum(ty):
            assert_ty_name(ty)

      for oper_type in ['queryType', 'mutationType', 'subscriptionType']:
         ty = json_get(introspect, ['data', '__schema', oper_type])
         if not ty:
            continue
         assert_ty_name(ty)
         ty = get_ty_by_name(types, ty['name'])

         for fld in ty['fields']:
            assert_fld_name(fld)

   def _mod_types_introspect(self, introspect):

      def remove_if_base_ty_has_prefix(elems):
         to_remove_elems = []
         for elem in elems:
            base_ty = get_base_ty(elem)
            if base_ty['name'].startswith(self.prefix):
               to_remove_elems.append(elem)
         for elem in to_remove_elems:
            elems.remove(elem)

      def mod_fld_args(fld):
         remove_if_base_ty_has_prefix(fld['args'])
         for arg in fld['args']:
            base_ty = get_base_ty(arg)
            if self._is_input_obj_ty(base_ty) or self._is_enum(base_ty):
               self._add_name_prefix(base_ty)

      def mod_obj_fields(ty):
         remove_if_base_ty_has_prefix(ty['fields'])
         for fld in ty['fields']:
            base_ty = get_base_ty(fld)
            if self._is_non_def_obj_ty(base_ty) or self._is_enum(base_ty):
               self._add_name_prefix(base_ty)
            mod_fld_args(fld)

      def mod_inp_obj_fields(ty):
         remove_if_base_ty_has_prefix(ty['inputFields'])
         for fld in ty['inputFields']:
            base_ty = get_base_ty(fld)
            if self._is_input_obj_ty(base_ty) or self._is_enum(base_ty):
               self._add_name_prefix(base_ty)

      def mod_obj(ty):
         self._add_name_prefix(ty)
         mod_obj_fields(ty)

      def mod_inp_obj(ty):
         self._add_name_prefix(ty)
         mod_inp_obj_fields(ty)

      types = get_introspect_types(introspect)
      if not types:
         return

      to_remove_types=[]
      for ty in types:
         # If types from server start with the given prefix, remove them
         # This would avoid cycles being created when this proxy
         # is added as remote to the GraphQL server itself
         if ty['name'].startswith(self.prefix):
            to_remove_types.append(ty)
         elif self._is_non_def_obj_ty(ty):
            mod_obj(ty)
         elif self._is_input_obj_ty(ty):
            mod_inp_obj(ty)
         elif self._is_enum(ty):
            self._add_name_prefix(ty)

      for ty in to_remove_types:
         types.remove(ty)

      # Add prefix to the operation types
      for oper_type in ['queryType', 'mutationType', 'subscriptionType']:
         ty_info = json_get(introspect, ['data', '__schema', oper_type])
         if ty_info and not ty_info['name'].startswith(self.prefix):
            self._add_name_prefix(ty_info)

   # With queries we need to strip prefix from top level fields (if present)
   def _query_mod_top_level_fields(self, req):

      def set_alias_if_absent(fld):
         if not fld.alias:
            fld.alias = graphql.NameNode(value=fld.name.value)

      def remove_prefix(fld):
         fld.name.value = re.sub(
            '^'+ re.escape(self.prefix), '',
            top_fld.name.value )

      errors = []
      query = graphql.parse(req['query'], no_location=True)
      for oper in query.definitions:
         if not getattr(oper, 'operation', None):
            continue
         for top_fld in oper.selection_set.selections:
            if top_fld.name.value.startswith(self.prefix):
               set_alias_if_absent(top_fld)
               remove_prefix(top_fld)
            elif top_fld.name.value not in ['__schema', '__type', '__typename' ]:
               errors.append('Unknown field ' + top_fld.name.value)
      req['query'] = graphql.print_ast(query)
      return errors


   # Add prefix for top level fields of all the operation types
   def _mod_top_level_fields_introspect(self, introspect):
      types = get_introspect_types(introspect)
      if not types:
         return

      def remove_fields_with_prefix(ty):
         to_drop_fields = []
         for fld in ty['fields']:
            if fld['name'].startswith(self.prefix):
               to_drop_fields.append(fld)
         for fld in to_drop_fields:
            ty['fields'].remove(fld)

      for oper_type in ['queryType', 'mutationType', 'subscriptionType']:
         ty_name = json_get(introspect, ['data', '__schema', oper_type, 'name'])
         if not ty_name:
            continue
         ty = get_ty_by_name(types, ty_name)

         remove_fields_with_prefix(ty)
         for fld in ty['fields']:
            self._add_name_prefix(fld)

   def post(self, request):
      input_query = request.json.copy()

      def log_if_input_query_changed():
         if request.json.get('query') != input_query.get('query'):
            print("Prefixer proxy: GrahpQL url:", self.gql_url)
            print ("input query:", input_query)
            print ("proxied query:", request.json)

      def modify_introspect_output(json_out):
         self._mod_top_level_fields_introspect(json_out)
         self._mod_types_introspect(json_out)
         self._assert_prefixes(json_out)

      if not request.json:
         return Response(HTTPStatus.BAD_REQUEST)

      errors = self._query_mod_top_level_fields(request.json)
      if errors:
         print('ERROR:',errors)
         json_out = {'errors': errors}
      else:
         log_if_input_query_changed()
         resp = requests.post(self.gql_url, json.dumps(request.json), headers=self.headers)
         json_out = resp.json()
         if json_out.get('errors'):
            print('ERROR:', json_out['errors'])
         modify_introspect_output(json_out)
      return Response(HTTPStatus.OK, json_out, {'Content-Type': 'application/json'})

handlers = MkHandlers({ '/graphql': GraphQLPrefixerProxy })

def MkGQLPrefixerProxy(gql_url, headers={}, prefix='prefixer_proxy_'):
   class _GQLPrefixerProxy(GraphQLPrefixerProxy):
      def __init__(self):
         super().__init__(gql_url, headers, prefix)
   return _GQLPrefixerProxy

def create_server(gql_url, headers={}, host='127.0.0.1', port=5000):
   return WebServer((host, port), MkGQLPrefixerProxy(gql_url, headers) )

import os
import ruamel.yaml as yaml
import textwrap
from abc import ABC, abstractmethod

# https://yaml.readthedocs.io/en/latest/example.html#output-of-dump-as-a-string
def dump_to_string(v):
  import io
  s = io.StringIO()
  yaml.YAML().dump(v, stream=s)
  return s.getvalue()

class Fixtures():
  setups = {} # Map backend [Setup]
  specs = {} # Map desc [Spec]

  def add_setup(self, backend, setup):
    if not backend in self.setups.keys():
      self.setups[backend] = []
    self.setups[backend].append(setup)

  def add_spec(self, desc, spec):
    if not desc in self.specs.keys():
      self.specs[desc] = []
    self.specs[desc].append(spec)

def render_gql(query, headers):
  if headers is None:
      return f"""\
GraphqlEngine.postGraphql
  testEnvironment
  [graphql|
{textwrap.indent(query, '    ')}
  |]"""
  else:
      return f"""\
GraphqlEngine.postGraphqlWithHeaders
  testEnvironment
{textwrap.indent(render_headers(headers), '    ')}
  [graphql|
{textwrap.indent(query, '    ')}
      |]"""

def render_v2query(expected_status, query, headers):
  if headers is None:
      return f"""\
GraphqlEngine.postV2Query
  {expected_status}
  testEnvironment
  [interpolateYaml|
{textwrap.indent(dump_to_string(query), '    ')}
  |]"""
  else:
      return f"""
GraphqlEngine.postV2QueryWithHeaders
  {expected_status}
{textwrap.indent(render_headers(headers), '    ')}
  testEnvironment
  [interpolateYaml|
{textwrap.indent(dump_to_string(query), '    ')}
  |]"""

def render_v1query(expected_status, query, headers):
  if headers is None:
      return f"""\
GraphqlEngine.postV1Query
  {expected_status}
  testEnvironment
  [interpolateYaml|
{textwrap.indent(dump_to_string(query), '    ')}
  |]"""
  else:
      return f"""\
GraphqlEngine.postV1QueryWithHeaders
  {expected_status}
{textwrap.indent(render_headers(headers), '    ')}
  testEnvironment
  [interpolateYaml|
{textwrap.indent(dump_to_string(query), '    ')}
  |]"""

def render_v1metadata(expected_status, query, headers):
  if headers is None:
      return f"""\
GraphqlEngine.postMetadataWithStatus
  {expected_status}
  testEnvironment
  [interpolateYaml|
{textwrap.indent(dump_to_string(query), '    ')}
  |]"""
  else:
      return f"""\
GraphqlEngine.postMetadataWithHeaders
  testEnvironment
{textwrap.indent(render_headers(headers), '    ')}
  [interpolateYaml|
{textwrap.indent(dump_to_string(query), '    ')}
  |]"""

def render_headers(headers):
    res = "(\n"
    for key, val in headers.items():
        res += f'  ("{key}", "{val}"):\n'
    res += "[])"

    return res

class Setup():
  def __init__(self, setup_name, original_file, url, value):
    self.setup_name = setup_name
    self.original_file = original_file
    self.url = url
    self.value = value

  def render_define(self, backend):
    return f"""-- original file: {self.original_file}
{self.setup_name}_{backend} :: J.Value
{self.setup_name}_{backend} =
  [interpolateYaml|
{textwrap.indent(self.value, 4*' ')}
  |]
"""

  def render_run(self, backend):
    res = ""
    if self.url == "/v2/query":
      res = f"GraphqlEngine.postV2Query 200 testEnvironment {self.setup_name}_{backend}"
    elif self.url == "/v1/query":
      res = f"GraphqlEngine.postV1Query 200 testEnvironment {self.setup_name}_{backend}"
    elif self.url == "/v1/graphql":
      res = f"GraphqlEngine.postGraphql testEnvironment {self.setup_name}_{backend}"
    elif self.url == "/v1/metadata":
      res = f"""GraphqlEngine.postMetadata_ testEnvironment {self.setup_name}_{backend}"""

    else:
      raise Exception(f"Unknown query url: {self.url}, in file {self.original_file}")

    return res

class Spec(ABC):

  @abstractmethod
  def render(self):
    pass

class PostSpec(Spec):
  """
  Specs that post something to HGE and make assertions against status code and
  response.
  """
  def __init__(self, desc, file, url, headers, query, expected_status, expected_response):
    self.desc = desc
    self.file = file
    self.url = url
    self.headers = headers
    self.query = query
    self.expected_status = expected_status
    self.expected_response = expected_response

  def render(self):
    actual = ""
    if self.url == "/v1/query":
      actual = render_v1query(self.expected_status, self.query, self.headers)
    elif self.url == "/v2/query":
      actual = render_v2query(self.expected_status, self.query, self.headers)
    elif self.url == "/v1/graphql":
      actual = render_gql(self.query, self.headers)
    elif self.url == "/v1/metadata":
      actual = render_v1metadata(self.expected_status, self.query, self.headers)
    else:
      raise Exception(f"Unknown query url: {self.url}, in file {self.file}")

    head =f"""-- from: {self.file}
it "{self.desc}" \\testEnvironment -> do"""

    expected = ""
    assertion = ""
    if self.expected_response is not None:
      expected = f"""
  let expected :: J.Value
      expected =
        [interpolateYaml|
{textwrap.indent(dump_to_string(self.expected_response), '          ')}
        |]
"""
      assertion = "actual `shouldBe` expected"
    else:
      assertion = "void actual"

    return f"""{head}
{expected}
  let actual :: IO J.Value
      actual =
{textwrap.indent(actual, '        ')}

  {assertion}"""


tests_to_port = {} # Map classname Fixtures

def with_test(name):

  if not name in tests_to_port.keys():
    tests_to_port[name] = Fixtures()

  return tests_to_port[name]


def write_tests_to_port():
  global tests_to_port

  os.makedirs("../lib/api-tests/src/Test/PortedFromPytest", exist_ok=True)

  for name, fixtures in tests_to_port.items():
    backends = fixtures.setups.keys()
    # Rather than directly make a hspec-discover'ed spec we include ported specs manually
    # for now.
    # This enables us to bulk-update the ported specs if we make changes to this porting script.
    #hspecified_name = name.removeprefix("Test") + "Spec"

    hspecified_name = name
    with open(f"../lib/api-tests/src/Test/PortedFromPytest/{hspecified_name}.hs", 'w') as ported_hs:
        ported_hs.write("{-# OPTIONS_GHC -Wno-deprecations #-}\n")
        ported_hs.write("{- HLINT ignore -}\n")
        ported_hs.write(f"""\
-- | GENERATED BY 'server/tests-py/PortToHaskell.py'.
-- Please avoid editing this file manually.
module Test.PortedFromPytest.{hspecified_name} (spec) where

import Data.Aeson qualified as J
import Data.List.NonEmpty qualified as NE
""" + str.join('\n', [
        f"import Harness.Backend.{backend} qualified as {backend}"
           for backend in backends

        ]) +
"""
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.PytestPortedCompat (compatSetup)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..))
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec

"""
        )

        for backend, setup_values in fixtures.setups.items():
          for setup in setup_values:
            ported_hs.write(setup.render_define(backend) + '\n')

          ported_hs.write(
        f"""
fixture_{backend} :: Fixture.Fixture ()
fixture_{backend} =
  (Fixture.fixture $ Fixture.Backend Fixture.{backend})""" +
  """
    { Fixture.setupTeardown = \\(testEnvironment, _) ->
        [ Fixture.SetupAction
            { Fixture.setupAction = do""" + f"""
                compatSetup testEnvironment Fixture.{backend}
""" + textwrap.indent(str.join('\n', map(lambda setup: setup.render_run(backend), setup_values)), 16*' ') + """,
              Fixture.teardownAction = \\_ -> return ()
            }
        ]
    }
""")

        ported_hs.write("""
spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runSingleSetup (NE.fromList [""" + str.join(', ', ["fixture_" + backend for backend in backends]) + """]) tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO J.Value -> J.Value -> IO ()
      shouldBe = shouldReturnYaml opts
""")

        for desc, specs in fixtures.specs.items():
          ported_hs.write(f"""
  describe "{desc}" do
""")
          for spec in specs:
              ported_hs.write('\n' + textwrap.indent(spec.render(), 4*' ') + '\n')

  tests_to_port = {}

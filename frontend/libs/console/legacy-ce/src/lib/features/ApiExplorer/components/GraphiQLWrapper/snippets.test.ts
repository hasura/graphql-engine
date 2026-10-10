/**
 * Regression coverage for the code-exporter snippets after the GraphiQL 5 /
 * @graphiql/plugin-code-exporter 5 upgrade. The code exporter still wraps the
 * CodeMirror-5-based `graphiql-code-exporter`, so the default (non-empty)
 * language snippets remain available, and the custom TypeScript `fetch` snippet
 * must emit VALID TypeScript: the real endpoint + headers, a quoted
 * operationName, and the operation's ACTUAL variables (the pre-upgrade version
 * emitted `fetch('undefined')`, an unquoted operationName, and an undeclared
 * `id` identifier from the `{"id": id}` variables helper).
 */
import ts from 'typescript';
import { OperationTypeNode, parse, OperationDefinitionNode } from 'graphql';
import snippets, { Snippet, OperationData } from './snippets';

const findTs = (): Snippet => {
  const snippet = snippets.find(
    (s) => s.language === 'TypeScript' && s.name === 'fetch',
  );
  if (!snippet) throw new Error('TypeScript fetch snippet not found');
  return snippet;
};

const makeOperationData = (
  query: string,
  overrides: Partial<OperationData> = {},
): OperationData => {
  const operationDefinition = parse(query)
    .definitions[0] as OperationDefinitionNode;
  return {
    query,
    name: operationDefinition.name?.value ?? '',
    displayName: operationDefinition.name?.value ?? '',
    type: OperationTypeNode.QUERY,
    variableName: 'OP',
    variables: {},
    operationDefinition,
    ...overrides,
  };
};

// transpileModule surfaces SYNTAX errors — a good "is this valid TS?" gate.
const transpileDiagnostics = (source: string) => {
  const out = ts.transpileModule(source, {
    reportDiagnostics: true,
    compilerOptions: {
      target: ts.ScriptTarget.ES2020,
      module: ts.ModuleKind.ESNext,
    },
  });
  return out.diagnostics ?? [];
};

type FetchCall = { url: string; init: RequestInit };

// Actually EXECUTE the generated snippet with a stubbed fetch/console so we can
// assert the request it would send (the snippet auto-invokes its fetch fn, and
// `fetch(...)` runs synchronously before the first await, so the call is
// recorded by the time the function returns). This catches payload bugs that a
// pure "does it transpile" check cannot.
const runSnippet = (source: string): FetchCall[] => {
  const js = ts.transpileModule(source, {
    compilerOptions: {
      target: ts.ScriptTarget.ES2020,
      module: ts.ModuleKind.ESNext,
    },
  }).outputText;
  const calls: FetchCall[] = [];
  const fetchStub = (url: string, init: RequestInit) => {
    calls.push({ url, init });
    return Promise.resolve({ json: async () => ({ data: {}, errors: null }) });
  };
  const consoleStub = { log: () => undefined, error: () => undefined };

  new Function('fetch', 'console', js)(fetchStub, consoleStub);
  return calls;
};

const bodyOf = (call: FetchCall) =>
  JSON.parse(call.init.body as string) as {
    query: string;
    variables: Record<string, unknown>;
    operationName?: string;
  };

describe('code-exporter snippets (GraphiQL 5)', () => {
  it('exposes a non-empty set of language snippets including the defaults', () => {
    expect(snippets.length).toBeGreaterThan(1);
    const languages = snippets.map((s) => s.language);
    expect(languages).toContain('TypeScript');
    expect(languages.some((l) => l !== 'TypeScript')).toBe(true);
    for (const s of snippets) {
      expect(typeof s.generate).toBe('function');
      expect(typeof s.language).toBe('string');
    }
  });

  it('emits VALID TypeScript with real endpoint, headers, quoted name and actual variables', () => {
    const op = makeOperationData(
      `query GetUser($id: Int!) {\n  user(id: $id) {\n    id\n  }\n}`,
      { variables: { id: 1 } },
    );
    const output = findTs().generate({
      serverUrl: 'https://my-hge.example.com/v1/graphql',
      headers: { 'x-hasura-admin-secret': 'my-secret' },
      context: {},
      operationDataList: [op],
      options: {},
    });

    // valid TypeScript (no syntax diagnostics)
    expect(transpileDiagnostics(output)).toHaveLength(0);
    // real endpoint (JSON-quoted), not the old literal 'undefined'
    expect(output).toContain('fetch("https://my-hge.example.com/v1/graphql"');
    expect(output).not.toContain("fetch('undefined'");
    // headers forwarded
    expect(output).toContain('"Content-Type": "application/json"');
    expect(output).toContain('"x-hasura-admin-secret": "my-secret"');
    // quoted operationName + correct `operation` identifier (not `operations`)
    expect(output).toContain('fetchGraphQL(operation, "GetUser"');
    expect(output).not.toMatch(/fetchGraphQL\(operations,/);
    // ACTUAL variable values, not an undeclared `id` identifier
    expect(output).toContain('"id": 1');
    expect(output).not.toMatch(/"id": id\b/);
    // the operation body is embedded (as an escaped string literal)
    expect(output).toContain('user(id: $id)');
  });

  it('handles an anonymous operation (valid identifier + omitted name)', () => {
    const op = makeOperationData(`{ __typename }`);
    const output = findTs().generate({
      serverUrl: 'http://localhost:8080/v1/graphql',
      headers: {},
      context: {},
      operationDataList: [op],
      options: {},
    });
    expect(transpileDiagnostics(output)).toHaveLength(0);
    expect(output).toContain('function fetchOperation()');
    // anonymous -> pass `undefined`, NOT the empty string `""`
    expect(output).toContain('fetchGraphQL(operation, undefined');
    expect(output).not.toContain('fetchGraphQL(operation, ""');
  });

  it('(executed) sends the real endpoint, headers, name and variables for a named op', () => {
    const op = makeOperationData(
      `query GetUser($id: Int!) {\n  user(id: $id) {\n    id\n  }\n}`,
      { variables: { id: 1 } },
    );
    const output = findTs().generate({
      serverUrl: 'https://my-hge.example.com/v1/graphql',
      headers: { 'x-hasura-admin-secret': 'my-secret' },
      context: {},
      operationDataList: [op],
      options: {},
    });

    const calls = runSnippet(output);
    expect(calls).toHaveLength(1);
    expect(calls[0].url).toBe('https://my-hge.example.com/v1/graphql');
    expect(calls[0].init.method).toBe('POST');
    expect(calls[0].init.headers).toMatchObject({
      'Content-Type': 'application/json',
      'x-hasura-admin-secret': 'my-secret',
    });
    const body = bodyOf(calls[0]);
    expect(body.query).toContain('user(id: $id)');
    expect(body.operationName).toBe('GetUser');
    expect(body.variables).toEqual({ id: 1 });
  });

  it('(executed) OMITS operationName in the body for an anonymous op', () => {
    const op = makeOperationData(`{ __typename }`);
    const output = findTs().generate({
      serverUrl: 'http://localhost:8080/v1/graphql',
      headers: {},
      context: {},
      operationDataList: [op],
      options: {},
    });
    const calls = runSnippet(output);
    expect(calls).toHaveLength(1);
    const body = bodyOf(calls[0]);
    // key must be ABSENT (not "" ) so the server runs the single anonymous op
    expect('operationName' in body).toBe(false);
  });

  it('(executed) round-trips endpoint/headers/variables with quotes, newlines, backticks and ${}', () => {
    const nastyUrl = 'https://h.example/graphql?q="x"&t=`y`${z}\n/end';
    const nastyHeaderValue = 'line1\nline2 "q" `tick` ${inj}';
    const nastyVarValue = 'he said "hi"\n`t`${x}';
    const op = makeOperationData(`query Q($s: String) {\n  f(s: $s)\n}`, {
      variables: { s: nastyVarValue },
    });
    const output = findTs().generate({
      serverUrl: nastyUrl,
      headers: { 'x-weird': nastyHeaderValue },
      context: {},
      operationDataList: [op],
      options: {},
    });

    // still valid TS despite the special characters
    expect(transpileDiagnostics(output)).toHaveLength(0);
    const calls = runSnippet(output);
    expect(calls[0].url).toBe(nastyUrl);
    expect((calls[0].init.headers as Record<string, string>)['x-weird']).toBe(
      nastyHeaderValue,
    );
    expect(bodyOf(calls[0]).variables).toEqual({ s: nastyVarValue });
  });

  it('does not crash on an empty operation list', () => {
    const output = findTs().generate({
      serverUrl: 'http://localhost:8080/v1/graphql',
      headers: {},
      context: {},
      operationDataList: [],
      options: {},
    });
    expect(output).toMatch(/No operation selected/i);
  });
});

const { test } = require('node:test');
const assert = require('node:assert/strict');
const { validateSource } = require('./validate-graphiql-examples.cjs');

function example({ query = 'query { author { id } }', response = '{"data":{"author":[{"id":1}]}}', variables } = {}) {
  return `<GraphiQLIDE query={${JSON.stringify(query)}} response={${JSON.stringify(response)}} ${variables === undefined ? '' : `variables={${JSON.stringify(variables)}}`} />`;
}

test('accepts valid static examples and reports their count', () => {
  assert.deepEqual(validateSource(example()), { examples: 1, errors: [] });
});

test('rejects the Postgres update trailing comma and insert missing brace regressions', () => {
  for (const variables of ['{"changes":{"is_published":false,}}', '{"address":{"city":"Bengaluru"}']) {
    const result = validateSource(example({ variables }));
    assert.match(result.errors.join('\n'), /Invalid variables/);
  }
});

test('decodes template literal escapes before validating JSON', () => {
  const valid =
    '<GraphiQLIDE query={`{ author { id } }`} response={`{"errors":[{"message":"Constraint \\\\"author\\\\""}]}`} />';
  assert.equal(validateSource(valid).errors.length, 0);
  const invalid =
    '<GraphiQLIDE query={`{ author { id } }`} response={`{"errors":[{"message":"Constraint \\"author\\""}]}`} />';
  assert.match(validateSource(invalid).errors.join('\n'), /Invalid response/);
});

test('rejects malformed response JSON and incomplete GraphQL', () => {
  assert.match(validateSource(example({ response: '{"data":' })).errors.join('\n'), /Invalid response/);
  assert.match(validateSource(example({ query: 'query { author { id }' })).errors.join('\n'), /Invalid query/);
});

test('uses field aliases rather than operation names for response keys', () => {
  assert.equal(
    validateSource(example({ query: 'query Authors { writers: author { id } }', response: '{"data":{"writers":[]}}' }))
      .errors.length,
    0
  );
  assert.match(
    validateSource(
      example({ query: 'query Authors { author { id } }', response: '{"data":{"Authors":[]}}' })
    ).errors.join('\n'),
    /missing "author"/
  );
});

test('requires array-shaped returning for table mutations', () => {
  const query = 'mutation { update_article(where: {}, _set: {rating: 1}) { returning { id } } }';
  assert.match(
    validateSource(example({ query, response: '{"data":{"update_article":{"returning":{"id":1}}}}' })).errors.join(
      '\n'
    ),
    /returning must be an array/
  );
  assert.equal(
    validateSource(example({ query, response: '{"data":{"update_article":{"returning":[{"id":1}]}}}' })).errors.length,
    0
  );
});

test('ignores fenced illustrative JSX while preserving diagnostic line numbers', () => {
  const source = '~~~jsx\n<GraphiQLIDE />\n~~~\n\n' + example({ variables: '{' });
  const result = validateSource(source, 'page.mdx');
  assert.equal(result.examples, 1);
  assert.match(result.errors[0], /^page\.mdx:5:/);
});

test('rejects interpolated props, spreads, missing props, and unterminated components', () => {
  assert.match(
    validateSource('<GraphiQLIDE query={`${dynamic}`} response={`{}`} />').errors.join('\n'),
    /static string/
  );
  assert.match(validateSource('<GraphiQLIDE {...props} />').errors.join('\n'), /Spread props/);
  assert.match(validateSource('<GraphiQLIDE />').errors.join('\n'), /Missing query/);
  assert.match(validateSource('<GraphiQLIDE query={`{ author { id } }`}').errors.join('\n'), /unterminated/);
});

test('variables and responses must be JSON objects', () => {
  assert.match(validateSource(example({ variables: '[]' })).errors.join('\n'), /JSON object/);
  assert.match(validateSource(example({ response: 'null' })).errors.join('\n'), /JSON object/);
});
test('supports batched GraphQL response examples and validates each operation', () => {
  const query = 'query First { author { id } } query Second { article { id } }';
  const response = '[{"data":{"author":[]}},{"data":{"article":[]}}]';
  assert.equal(validateSource(example({ query, response })).errors.length, 0);
  assert.match(
    validateSource(example({ query, response: '[{"data":{"author":[]}}]' })).errors.join('\n'),
    /Batch response length/
  );
  assert.match(
    validateSource(example({ query, response: '[{"data":{"author":[]}},{"data":{"author":[]}}]' })).errors.join('\n'),
    /missing "article"/
  );
});

test('reports shorthand and empty expression props without throwing', () => {
  assert.match(validateSource('<GraphiQLIDE query response={`{}`} />').errors.join('\n'), /static string/);
  assert.match(validateSource('<GraphiQLIDE query={} response={`{}`} />').errors.join('\n'), /static string/);
});

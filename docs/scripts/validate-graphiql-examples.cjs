const fs = require('node:fs');
const path = require('node:path');
const ts = require('typescript');
const { parse, Kind } = require('graphql');

// Ignore illustrative code fences, but preserve line numbers for diagnostics.
function withoutCodeFences(source) {
  let fence;
  return source
    .split('\n')
    .map(line => {
      const marker = line.match(/^\s*(`{3,}|~{3,})/);
      if (marker && !fence) {
        fence = marker[1];
        return '';
      }
      if (fence) {
        if (marker && marker[1][0] === fence[0] && marker[1].length >= fence.length) {
          fence = undefined;
        }
        return '';
      }
      return line;
    })
    .join('\n');
}

function validateSource(source, file = '<input>') {
  const text = withoutCodeFences(source);
  const errors = [];
  let examples = 0;
  for (const match of text.matchAll(/<GraphiQLIDE\b[\s\S]*?\/>/g)) {
    examples += 1;
    const line = text.slice(0, match.index).split('\n').length;
    const report = message => errors.push(`${file}:${line}: ${message}`);
    const jsx = ts.createSourceFile('example.tsx', match[0], ts.ScriptTarget.Latest, true, ts.ScriptKind.TSX);
    if (jsx.parseDiagnostics.length) {
      jsx.parseDiagnostics.forEach(d => report(ts.flattenDiagnosticMessageText(d.messageText, '\n')));
      continue;
    }
    const element = jsx.statements[0]?.expression;
    if (!element || !ts.isJsxSelfClosingElement(element)) {
      report('Expected a self-closing GraphiQLIDE example.');
      continue;
    }
    const props = {};
    for (const attribute of element.attributes.properties) {
      if (ts.isJsxSpreadAttribute(attribute)) {
        report('Spread props cannot be statically validated.');
        continue;
      }
      const name = attribute.name.text;
      if (!['query', 'variables', 'response'].includes(name)) continue;
      const value =
        attribute.initializer &&
        (ts.isJsxExpression(attribute.initializer) ? attribute.initializer.expression : attribute.initializer);
      if (!value || !(ts.isStringLiteral(value) || ts.isNoSubstitutionTemplateLiteral(value))) {
        report(`${name} must be a static string without template interpolation.`);
        continue;
      }
      props[name] = value.text; // TypeScript decodes JavaScript escapes, as the browser does.
    }

    let document, response;
    for (const name of ['query', 'response', 'variables']) {
      if (props[name] === undefined) {
        if (name !== 'variables') report(`Missing ${name} string.`);
        continue;
      }
      try {
        if (name === 'query') {
          document = parse(props[name]);
        } else {
          const value = JSON.parse(props[name]);
          const objects = name === 'response' && Array.isArray(value) ? value : [value];
          if (!objects.length || objects.some(item => !item || typeof item !== 'object' || Array.isArray(item))) {
            throw new Error(
              name === 'variables'
                ? 'Expected a JSON object.'
                : 'Expected a JSON object or a batch of response objects.'
            );
          }
          if (name === 'response') response = value;
        }
      } catch (error) {
        report(`Invalid ${name}: ${error.message}`);
      }
    }
    if (!document || !response) continue;

    const operations = document.definitions.filter(d => d.kind === Kind.OPERATION_DEFINITION);
    const responses = Array.isArray(response) ? response : [response];
    if (Array.isArray(response) && response.length !== operations.length) {
      report('Batch response length does not match the number of operations.');
      continue;
    }
    for (const [index, operation] of operations.entries()) {
      const data = responses[Array.isArray(response) ? index : 0]?.data;
      if (!data) continue; // Error-only responses are valid examples.
      for (const field of operation.selectionSet.selections.filter(s => s.kind === Kind.FIELD)) {
        const key = field.alias?.value || field.name.value;
        if (!Object.hasOwn(data, key)) {
          report(`Response data is missing "${key}" (use the field alias, not the operation name).`);
        }
        // Hasura table mutations return a list even when only one row is affected.
        if (operation.operation === 'mutation' && /^(insert|update|delete)_/.test(field.name.value)) {
          const returning = data[key]?.returning;
          if (returning !== undefined && returning !== null && !Array.isArray(returning)) {
            report(`Response data.${key}.returning must be an array.`);
          }
        }
      }
    }
  }
  const starts = [...text.matchAll(/<GraphiQLIDE\b/g)].length;
  if (starts !== examples) errors.push(`${file}: Found an unterminated GraphiQLIDE component.`);
  return { examples, errors };
}

function validateDirectory(directory) {
  let files = 0,
    examples = 0;
  const errors = [];
  function walk(dir) {
    for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
      const file = path.join(dir, entry.name);
      if (entry.isDirectory()) walk(file);
      else if (/\.(md|mdx)$/.test(entry.name)) {
        const result = validateSource(fs.readFileSync(file, 'utf8'), path.relative(directory, file));
        if (result.examples) files += 1;
        examples += result.examples;
        errors.push(...result.errors);
      }
    }
  }
  walk(directory);
  return { files, examples, errors };
}

if (require.main === module) {
  const root = path.resolve(__dirname, '..');
  const results = ['docs', 'wiki'].map(dir => validateDirectory(path.join(root, dir)));
  const errors = results.flatMap(r => r.errors);
  if (errors.length) {
    console.error(errors.join('\n'));
    process.exitCode = 1;
  } else {
    console.log(
      `Validated ${results.reduce((n, r) => n + r.examples, 0)} GraphiQL examples in ${results.reduce((n, r) => n + r.files, 0)} pages.`
    );
  }
}

module.exports = { validateSource, validateDirectory };

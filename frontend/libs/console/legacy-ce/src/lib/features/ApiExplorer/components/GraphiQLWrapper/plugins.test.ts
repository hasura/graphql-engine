/**
 * Regression coverage that the GraphiQL 5 plugin factories still produce valid
 * plugin objects for the console's Explorer + Code Exporter, using the real
 * installed @graphiql/plugin-explorer and @graphiql/plugin-code-exporter
 * packages (not mocks).
 */
// The code-exporter wraps the legacy (babel-regenerator-built)
// `graphiql-code-exporter`; webpack provides regenerator-runtime at build time,
// so polyfill it here for the Vitest environment.
import 'regenerator-runtime/runtime';
import { explorer, codeExplorer } from './plugins';

const isGraphiQLPlugin = (p: unknown) => {
  const plugin = p as { title?: unknown; content?: unknown; icon?: unknown };
  return (
    typeof plugin === 'object' &&
    plugin !== null &&
    typeof plugin.title === 'string' &&
    // `content` and `icon` are React components (functions) in GraphiQL plugins
    typeof plugin.content === 'function' &&
    typeof plugin.icon === 'function'
  );
};

describe('GraphiQL 5 plugins', () => {
  it('explorerPlugin() returns a valid GraphiQLPlugin', () => {
    expect(isGraphiQLPlugin(explorer)).toBe(true);
  });

  it('codeExporterPlugin({ snippets, codeMirrorTheme }) returns a valid GraphiQLPlugin', () => {
    expect(isGraphiQLPlugin(codeExplorer)).toBe(true);
  });

  it('the two plugins have distinct titles', () => {
    expect((explorer as { title: string }).title).not.toBe(
      (codeExplorer as { title: string }).title,
    );
  });
});

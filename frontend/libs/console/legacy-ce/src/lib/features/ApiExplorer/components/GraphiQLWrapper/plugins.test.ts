/**
 * Regression coverage that the GraphiQL 5 plugin factory still produces a valid
 * plugin object for the console's Explorer, using the real installed
 * @graphiql/plugin-explorer package (not mocks).
 */
import { explorer } from './plugins';

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
});

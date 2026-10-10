import { explorerPlugin } from '@graphiql/plugin-explorer';
import snippets from './snippets';
import { codeExporterPlugin } from '@graphiql/plugin-code-exporter';

export const explorer = explorerPlugin();

export const codeExplorer = codeExporterPlugin({
  snippets,
  codeMirrorTheme: 'default',
});

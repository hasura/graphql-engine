import { useEffect } from 'react';
import { useGraphiQL } from '@graphiql/react';

/** Monaco measures its own text, so changing the container CSS is not enough. */
export function GraphiQLFontSize() {
  const queryEditor = useGraphiQL((state) => state.queryEditor);
  const variableEditor = useGraphiQL((state) => state.variableEditor);
  const headerEditor = useGraphiQL((state) => state.headerEditor);
  const responseEditor = useGraphiQL((state) => state.responseEditor);

  useEffect(() => {
    for (const editor of [
      queryEditor,
      variableEditor,
      headerEditor,
      responseEditor,
    ]) {
      editor?.updateOptions({ fontSize: 14 });
    }
  }, [queryEditor, variableEditor, headerEditor, responseEditor]);

  return null;
}

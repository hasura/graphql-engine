import {
  useOperationsEditorState,
  useVariablesEditorState,
  useGraphiQL,
} from '@graphiql/react';
import AnalyzeButton from '../Analyzer/AnalyzeButton';
import RestToolbarButton from '../Rest/RestToolbarButton';
import DeriveActionButton from '../DeriveAction/DeriveActionButton';

const GraphiQLToolbarButtons = ({ mode, headers }) => {
  // GraphiQL v5 (Monaco) moved parsed operations + schema out of the editor
  // instance and onto the store: in v4 these came from
  // `useEditorContext().queryEditor.operations` and `useSchemaStore().schema`,
  // but a Monaco `queryEditor` no longer exposes `.operations`. Read both from
  // the store via the typed `useGraphiQL` selector (no `any` casts).
  const operations = useGraphiQL((state) => state.operations);
  const schema = useGraphiQL((state) => state.schema);
  const [operationsText] = useOperationsEditorState();
  const [variables] = useVariablesEditorState();

  return (
    <>
      <RestToolbarButton operations={operations} schema={schema} />
      {mode === 'graphql' && (
        <DeriveActionButton operations={operations} schema={schema} />
      )}
      <AnalyzeButton
        operations={operations}
        query={operationsText}
        variables={variables}
        headers={headers}
        mode={mode}
      />
    </>
  );
};

export default GraphiQLToolbarButtons;

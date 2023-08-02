import * as React from 'react';
import { Dialog } from '../../../new-components/Dialog';
import { Tabs } from '../../../new-components/Tabs';
import { Schema } from '../types';
import AceEditor from 'react-ace';
import 'brace/mode/html';
import 'brace/mode/markdown';
import 'brace/theme/github';
import 'brace/theme/chrome';

type Props = {
  onClose: VoidFunction;
  schema: Schema | null;
};

export const SchemaModal: React.VFC<Props> = props => {
  const [tabState, setTabState] = React.useState('schema');

  const { schema, onClose } = props;

  if (!schema) {
    return null;
  }

  return (
    <Dialog
      hasBackdrop
      size="lg"
      onClose={onClose}
      title="GraphQL Schema Details"
      description=""
    >
      <div className="w-full h-full p-md">
        <Tabs
          value={tabState}
          onValueChange={state => setTabState(state)}
          items={[
            {
              value: 'schema',
              label: 'GraphQL Schema',
              content: <SchemaView schema={schema.raw} />,
            },
            {
              value: 'diff',
              label: 'Changes',
              content: <DiffView changes={schema.changes} />,
            },
          ]}
        />
      </div>
    </Dialog>
  );
};

export const SchemaView: React.VFC<{ schema: string }> = props => {
  const { schema } = props;
  return (
    <div className="w-full p-sm">
      <AceEditor
        mode="graphql"
        fontSize={14}
        width="100%"
        theme="github"
        name={`schema-registry-schema-modal-view-schema`}
        value={schema}
        editorProps={{ $blockScrolling: true }}
        setOptions={{ useWorker: false }}
      />
    </div>
  );
};

export const DiffView: React.VFC<{ changes: Schema['changes'] }> = props => {
  const { changes } = props;

  if (!changes) {
    return <span>Could not compute what changed in this GraphQL Schema</span>;
  }

  if (!changes.length) {
    return <span>No changes!</span>;
  }

  const breakingChanges = changes.filter(
    c => c.criticality.level === 'BREAKING'
  );
  const dangerousChanges = changes.filter(
    c => c.criticality.level === 'DANGEROUS'
  );
  const safeChanges = changes.filter(
    c => c.criticality.level === 'NON_BREAKING'
  );

  return (
    <div className="w-full p-sm">
      {breakingChanges.length && (
        <div className="flex flex-col w-full mb-sm">
          <b className="mb-xs">Breaking Changes</b>
          <ul className="marker:text-red-600 list-outside list-disc ml-6">
            {breakingChanges.map(c => {
              return <li>{c.message}</li>;
            })}
          </ul>
        </div>
      )}
      {dangerousChanges.length && (
        <div className="flex flex-col w-full mb-sm">
          <b className="mb-xs">Dangerous Changes</b>
          <ul className="marker:text-yellow-500 list-outside list-disc ml-6">
            {dangerousChanges.map(c => {
              return <li>{c.message}</li>;
            })}
          </ul>
        </div>
      )}
      {safeChanges.length && (
        <div className="flex flex-col w-full mb-sm">
          <b className="mb-xs">Safe Changes</b>
          <ul className="marker:text-lime-500 list-outside list-disc ml-6">
            {safeChanges.map(c => {
              return <li>{c.message}</li>;
            })}
          </ul>
        </div>
      )}
    </div>
  );
};

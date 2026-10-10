import * as React from 'react';
import { AceEditor, Dialog, Tabs } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

import { Schema } from '../types';

type Props = {
  onClose: VoidFunction;
  schema: Schema | null;
};

export const SchemaModal: React.FC<Props> = (props) => {
  const [tabState, setTabState] = React.useState('schema');

  const { schema, onClose } = props;

  if (!schema) {
    return null;
  }

  return (
    <Dialog
      size="lg"
      onClose={onClose}
      title="GraphQL Schema Details"
      description=""
    >
      <div className="w-full h-full p-4">
        <Tabs
          value={tabState}
          onValueChange={(state) => setTabState(state)}
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

export const SchemaView: React.FC<{ schema: string }> = (props) => {
  const { schema } = props;
  return (
    <div className="w-full p-2">
      <AceEditor
        mode="graphqlschema"
        fontSize={14}
        width="100%"
        name={`schema-registry-schema-modal-view-schema`}
        value={schema}
        editorProps={{ $blockScrolling: true }}
        setOptions={{ useWorker: false }}
      />
    </div>
  );
};

export const DiffView: React.FC<{ changes: Schema['changes'] }> = (props) => {
  const { changes } = props;

  if (!changes) {
    return <span>Could not compute what changed in this GraphQL Schema</span>;
  }

  if (!changes.length) {
    return <span>No changes!</span>;
  }

  const breakingChanges = changes.filter(
    (c) => c.criticality.level === 'BREAKING',
  );
  const dangerousChanges = changes.filter(
    (c) => c.criticality.level === 'DANGEROUS',
  );
  const safeChanges = changes.filter(
    (c) => c.criticality.level === 'NON_BREAKING',
  );

  return (
    <div className="w-full p-2">
      {breakingChanges.length && (
        <Flex direction="column" className="w-full mb-2">
          <b className="mb-1">Breaking Changes</b>
          <ul className="marker:text-red-600 list-outside list-disc ml-6">
            {breakingChanges.map((c, index) => {
              return <li key={index}>{c.message}</li>;
            })}
          </ul>
        </Flex>
      )}
      {dangerousChanges.length && (
        <Flex direction="column" className="w-full mb-2">
          <b className="mb-1">Dangerous Changes</b>
          <ul className="marker:text-yellow-500 list-outside list-disc ml-6">
            {dangerousChanges.map((c, index) => {
              return <li key={index}>{c.message}</li>;
            })}
          </ul>
        </Flex>
      )}
      {safeChanges.length && (
        <Flex direction="column" className="w-full mb-2">
          <b className="mb-1">Safe Changes</b>
          <ul className="marker:text-lime-500 list-outside list-disc ml-6">
            {safeChanges.map((c, index) => {
              return <li key={index}>{c.message}</li>;
            })}
          </ul>
        </Flex>
      )}
    </div>
  );
};

import React, { useMemo, useState } from 'react';
import LZString from 'lz-string';
import { useGetSchema } from '../hooks/useGetSchema';
import {
  Tabs,
  IconTooltip,
  Input,
  DropdownButton,
  DropdownMenu,
  AceEditor,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

import { SchemaRow } from './SchemaRow';
import {
  findIfSubStringExists,
  schemaTransformFn,
  getPublishTime,
} from '../utils';
import { RoleBasedSchema, Schema } from '../types';
import { FaSearch, FaShareAlt } from 'react-icons/fa';
import { Analytics } from '@hasura/shared/analytics';

interface SchemaChangeDetailsProps {
  schemaId: string;
}

export const SchemaChangeDetails: React.FC<SchemaChangeDetailsProps> = (
  props,
) => {
  const { schemaId } = props;
  const fetchSchemaResponse = useGetSchema(schemaId);
  const { kind } = fetchSchemaResponse;

  switch (kind) {
    case 'loading':
      return <p>Loading...</p>;
    case 'error':
      return <p>Error: {fetchSchemaResponse.message}</p>;
    case 'success': {
      const transformedData = schemaTransformFn(fetchSchemaResponse.response);
      if (transformedData) {
        return (
          <div className="w-full">
            <SchemasDetails schema={transformedData} />
          </div>
        );
      }

      return <p>Schema registry not found</p>;
    }
  }
};

const SchemasDetails: React.FC<{
  schema: Schema;
}> = (props) => {
  const { schema } = props;

  const [tabState, setTabState] = React.useState('changes');

  // We know for sure only one roleBasedSchema exists
  const roleBasedSchema = schema.roleBasedSchemas[0];
  const [isCopied, toggle] = useState(false);
  const onCopy = () => {
    const currentURL = `${window.location.origin}${window.location.pathname}`; // Get the current URL
    const urlToCopy = `${currentURL}/${roleBasedSchema.id}`;
    try {
      navigator.clipboard.writeText(urlToCopy).then(() => {
        toggle(true);
        setTimeout(() => {
          toggle(false);
        }, 2000);
      });
    } catch (error) {
      alert(`Failed to copy URL!`);
    }
  };

  return (
    <Flex
      direction="column"
      className="border-neutral-200 bg-white border rounded-md"
    >
      <Flex className="bg-gray-100 px-4 py-2 w-full">
        <Flex justify="between" className="text-base w-[15%]">
          <span className="text-sm font-bold">ROLE</span>
        </Flex>
        <Flex className="text-base justify-around w-[30%]">
          <span className="text-sm font-bold mx-2">BREAKING</span>
          <span className="text-sm font-bold mx-2">DANGEROUS</span>
          <span className="text-sm font-bold mr-4">SAFE</span>
        </Flex>
        <Flex align="center" className="text-base justify-around w-[55%]">
          <span className="text-sm font-bold mx-2">TOTAL</span>
        </Flex>
      </Flex>
      <SchemaRow
        role={roleBasedSchema.role || ''}
        changes={roleBasedSchema.changes}
      />
      <div className="px-4 mb-2">
        <Flex className="mt-4">
          <Flex align="center">
            <p className="font-bold text-gray-500 py-2">Published: </p>

            <Flex align="center" className="ml-2 font-semibold text-gray-600">
              <span>{getPublishTime(schema.created_at)}</span>
              <IconTooltip message="The time at which this GraphQL schema was generated" />
            </Flex>
          </Flex>
          <Flex align="center" justify="center" className="ml-auto w-1/2">
            <div className="flex-col">
              <Flex align="center" className="mt-4">
                <p className="font-bold text-gray-500 py-2">Hash</p>
                <IconTooltip message="Hash of the GraphQL Schema SDL. Hash for two identical schema is identical." />
              </Flex>
              <span className="font-bold bg-gray-100 px-1 rounded text-sm">
                {roleBasedSchema.hash}
              </span>
            </div>
            <Flex direction="column" align="center">
              <Analytics name="schema-registry-share-btn">
                <button
                  className="flex items-center mx-auto  rounded-lg transition-colors duration-300"
                  onClick={onCopy}
                >
                  <FaShareAlt
                    color={isCopied ? '#3182ce' : '#718096'}
                    size={18}
                  />
                  <span
                    className={`font-bold ${
                      isCopied ? 'text-blue-500' : 'text-gray-500'
                    } py-2 pl-2 text-md mr-2 hover:text-blue-500`}
                  >
                    Share
                  </span>
                </button>
              </Analytics>
              {isCopied ? <p className="ml-2 text-gray-500">Copied!</p> : ''}
            </Flex>
          </Flex>
        </Flex>
      </div>

      <div className="h-full px-4">
        <Tabs
          value={tabState}
          onValueChange={(state) => setTabState(state)}
          items={[
            {
              value: 'changes',
              label: 'Changes',
              content: (
                <ChangesView
                  changes={roleBasedSchema.changes}
                  role={roleBasedSchema.role}
                />
              ),
            },
            {
              value: 'graphql',
              label: 'GraphQL',
              content: <SchemaView schema={roleBasedSchema.raw} />,
            },
          ]}
        />
      </div>
    </Flex>
  );
};

export const SchemaView: React.FC<{ schema: string }> = (props) => {
  const { schema } = props;
  const decompressedSchema = LZString.decompressFromBase64(schema);
  return (
    <div className="w-full p-2">
      <AceEditor
        mode="graphqlschema"
        fontSize={14}
        width="100%"
        theme="chrome"
        name={`schema-registry-schema-modal-view-schema`}
        value={decompressedSchema}
        editorProps={{ $blockScrolling: true }}
        setOptions={{ useWorker: false }}
      />
    </div>
  );
};

export const ChangesView: React.FC<{
  changes: RoleBasedSchema['changes'];
  role: string;
}> = (props) => {
  const { changes, role } = props;

  const [searchText, setSearchText] = React.useState('');
  const [selectedChangeLevel, setSelectedChangeLevel] = useState<string | null>(
    null,
  );
  const handleSearch = (e: React.ChangeEvent<HTMLInputElement>) =>
    setSearchText(e.target.value);

  const changesList = useMemo(() => {
    if (!searchText) return changes;

    return changes?.filter((change) =>
      findIfSubStringExists(change.message, searchText),
    );
  }, [searchText, changes]);

  if (!changes) {
    return (
      <div className="p-4">
        <span className="text-muted">
          Could not compute changes in this GraphQL schema with respect to the
          previous schema for role <b>{role}</b>. This typically happens if
          there is no previous schema for role <b>{role}</b> or if your GraphQL
          schema is erroneous.
        </span>
      </div>
    );
  }

  if (!changes.length) {
    return (
      <div className="p-4">
        <span className="text-muted">
          No changes in this GraphQL schema with respect to the previous schema
          for role <b>{role}</b>.
        </span>
      </div>
    );
  }

  const breakingChanges =
    changesList &&
    changesList.filter((c) => c.criticality.level === 'BREAKING');
  const dangerousChanges =
    changesList &&
    changesList.filter((c) => c.criticality.level === 'DANGEROUS');
  const safeChanges =
    changesList &&
    changesList.filter((c) => c.criticality.level === 'NON_BREAKING');

  const showBreakingChanges =
    (!selectedChangeLevel || selectedChangeLevel === 'breaking') &&
    !!breakingChanges?.length;
  const showDangerousChanges =
    (!selectedChangeLevel || selectedChangeLevel === 'dangerous') &&
    !!dangerousChanges?.length;
  const showSafeChanges =
    (!selectedChangeLevel || selectedChangeLevel === 'safe') &&
    !!safeChanges?.length;

  const options = [
    {
      value: 'breaking',
      label: 'Breaking',
    },
    {
      value: 'dangerous',
      label: 'Dangerous',
    },
    {
      value: 'safe',
      label: 'Safe',
    },
  ];

  return (
    <div className="flex-col">
      <Flex align="center" justify="between" className="w-full mt-4 mb-4">
        <Flex className="font-semibold text-lg text-gray-500">Changes</Flex>
        <Flex className="w-[50%]">
          <Flex className="w-full">
            <div className="px-2 w-1/2">
              <Analytics name="schema-registry-schema-change-details-filter">
                <DropdownButton
                  mode="default"
                  items={options.map((opt) => (
                    <DropdownMenu.Item
                      key={opt.value}
                      onSelect={() => setSelectedChangeLevel(opt.value)}
                    >
                      {opt.label}
                    </DropdownMenu.Item>
                  ))}
                />
              </Analytics>
            </div>

            <div className="pr-2 w-1/2">
              <Analytics name="schema-registry-schema-change-details-search">
                <label>
                  <Input
                    type="text"
                    placeholder="Search"
                    name="search"
                    icon={FaSearch}
                    iconPosition="start"
                    onChange={handleSearch}
                  />
                </label>
              </Analytics>
            </div>
          </Flex>
        </Flex>
      </Flex>

      {showBreakingChanges && (
        <div>
          <Flex
            align="center"
            className="font-semibold text-md text-gray-500 mt-4"
          >
            Breaking
            <IconTooltip message="Breaking changes due the schema change" />
          </Flex>
          <ChangesTable changes={breakingChanges} level="breaking" />
        </div>
      )}
      {showDangerousChanges && dangerousChanges && (
        <div>
          <Flex
            align="center"
            className="font-semibold text-md text-gray-500 mt-4"
          >
            Dangerous
            <IconTooltip message="Dangerous changes due the schema change" />
          </Flex>
          <ChangesTable changes={dangerousChanges} level="dangerous" />
        </div>
      )}
      {showSafeChanges && safeChanges && (
        <div>
          <Flex
            align="center"
            className="font-semibold text-md text-gray-500 mt-4"
          >
            Safe
            <IconTooltip message="Safe changes due the schema change" />
          </Flex>
          <ChangesTable changes={safeChanges} level="safe" />
        </div>
      )}
    </div>
  );
};
interface ChangesTableProps {
  changes: RoleBasedSchema['changes'];
  level: 'breaking' | 'dangerous' | 'safe';
}

const ChangesTable: React.FC<ChangesTableProps> = ({ changes, level }) => {
  const textColor = {
    breaking: 'text-red-600',
    dangerous: 'text-yellow-600',
    safe: 'text-green-600',
  };
  return (
    <Flex direction="column" className="rounded border-neutral-200 border my-2">
      <table className="w-full border-neutral-200 rounded">
        <thead>
          <tr style={{ textAlign: 'left' }}>
            <th className="text-base w-[69%] bg-[#F8FAFC] border pl-2">
              <span className="text-sm text-color-[#AEB7C3]">
                AFFECTED OPERATIONS
              </span>
            </th>
          </tr>
        </thead>
        <tbody>
          {changes &&
            changes.map((change, index) => (
              <tr
                className={`${textColor[level]} border border-neutral-200 p-2`}
                key={index}
              >
                <td className="pl-2">{change.message}</td>
              </tr>
            ))}
        </tbody>
      </table>
    </Flex>
  );
};

import React, { useMemo } from 'react';
import LZString from 'lz-string';
import { useGetSchema } from '../hooks/useGetSchema';
import {
  Tabs,
  IconTooltip,
  Input,
  RelativeLink,
  CodeEditorField,
  Text,
} from '@hasura/shared/ui';

import { SchemaRow } from './SchemaRow';
import { ChangeSummary } from './ChangeSummary';
import {
  findIfSubStringExists,
  schemaTransformFn,
  getPublishTime,
} from '../utils';
import { RoleBasedSchema, Schema } from '../types';
import { FaHome, FaAngleRight, FaFileImport, FaSearch } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';

export const Breadcrumbs = () => (
  <Flex align="center" className="space-x-xs mb-4">
    <RelativeLink
      to="/api/schema-registry"
      className="cursor-pointer flex items-center"
    >
      <FaHome className="mr-1.5" />
      <Text size="1">Schema</Text>
    </RelativeLink>
    <FaAngleRight className="text-muted" />
    <Text size="1" asChild color="indigo">
      <Flex align="center" className="cursor-pointer" gap="1">
        <FaFileImport className="mr-1.5" />
        Roles
      </Flex>
    </Text>
  </Flex>
);

type SchemaDetailsViewProps = {
  params: {
    id: string;
  };
};

export const SchemaDetailsView = (props: SchemaDetailsViewProps) => {
  const { id: schemaId } = props.params;
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
        return <SchemasDetails schema={transformedData} />;
      }

      return <p>Schema registry not found</p>;
    }
  }
};

const SchemasDetails: React.FC<{
  schema: Schema;
}> = (props) => {
  const { schema } = props;

  const [tabState, setTabState] = React.useState('graphql');

  // We know for sure only one roleBasedSchema exists
  const roleBasedSchema = schema.roleBasedSchemas[0];

  return (
    <div className="mx-4 mt-4">
      <Breadcrumbs />
      <Flex className="mb-2">
        <span className="font-bold text-xl text-black mr-4">
          {roleBasedSchema.role}:{schema.entry_hash}
        </span>
      </Flex>
      <div className="border-neutral-200 bg-white border ">
        <Flex className="w-full bg-gray-100 px-4 py-2">
          <Flex className="text-base w-[69%]" justify="start">
            <span className="text-sm font-bold">SCHEMA</span>
          </Flex>
          <Flex className="text-base w-[28%]" justify="between">
            <span className="text-sm font-bold">BREAKING</span>
            <span className="text-sm font-bold">DANGEROUS</span>
            <span className="text-sm font-bold">SAFE</span>
          </Flex>
        </Flex>

        <div className="ml-4 mb-2 ">
          <SchemaRow
            role={roleBasedSchema.role || ''}
            changes={roleBasedSchema.changes}
          />

          <Flex className="mt-4">
            <div className="flex-col w-1/2">
              <Flex align="center">
                <p className="font-bold text-gray-500">Published</p>
                <IconTooltip message="The time at which this GraphQL schema was generated" />
              </Flex>
              <span>{getPublishTime(schema.created_at)}</span>
            </div>
            <div className="flex-col w-1/2">
              <Flex align="center">
                <p className="font-bold text-gray-500">Schema Hash</p>
                <IconTooltip message="Hash of the GraphQL Schema SDL. Hash for two identical schema is identical." />
              </Flex>
              <span className="font-bold bg-gray-100 px-1 rounded text-sm">
                {roleBasedSchema.hash}
              </span>
            </div>
          </Flex>
        </div>

        <div className="w-full h-full">
          <Tabs
            value={tabState}
            onValueChange={(state) => setTabState(state)}
            items={[
              {
                value: 'graphql',
                label: 'GraphQL',
                content: <SchemaView schema={roleBasedSchema.raw} />,
              },
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
            ]}
          />
        </div>
      </div>
    </div>
  );
};

export const SchemaView: React.FC<{ schema: string }> = (props) => {
  const { schema } = props;
  const decompressedSchema = LZString.decompressFromBase64(schema);

  return (
    <div className="w-full p-2">
      <CodeEditorField
        name="schema-registry-schema-modal-view-schema"
        editorProps={{
          mode: 'graphqlschema',
          width: '100%',
          defaultValue: decompressedSchema,
        }}
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

  return (
    <div className="flex-col m-8">
      <div className="mb-8">
        <p className="text-muted">
          These changes are with respect to the previous schema for role{' '}
          <b>{role}</b>.
        </p>
      </div>
      <div className="flex-col">
        <div className="font-bold text-md mb-8 text-gray-500">
          Change Summary
        </div>
        <ChangeSummary changes={changes} />
      </div>
      <Flex className="w-full border-b border-gray-300 my-8" />
      <Flex className="w-full mb-4" justify="between">
        <Flex className="font-bold text-gray-600">Changes</Flex>

        <label className="block">
          <Input
            type="text"
            placeholder="Search"
            name="search"
            icon={FaSearch}
            iconPosition="start"
            onChange={handleSearch}
          />
        </label>
      </Flex>
      {breakingChanges && breakingChanges.length > 0 && (
        <Flex direction="column">
          {breakingChanges.map((c, index) => {
            return (
              <div
                key={index}
                className="text-red-600 border border-gray-400 p-2"
              >
                {c.message}
              </div>
            );
          })}
        </Flex>
      )}
      {dangerousChanges && dangerousChanges.length > 0 && (
        <Flex direction="column">
          {dangerousChanges.map((c, index) => {
            return (
              <div
                key={index}
                className="text-red-800 border border-gray-400 p-2"
              >
                {c.message}
              </div>
            );
          })}
        </Flex>
      )}
      {safeChanges && safeChanges.length > 0 && (
        <Flex direction="column">
          {safeChanges.map((c, index) => {
            return (
              <div
                key={index}
                className="text-green-600 border border-gray-400 p-2"
              >
                {c.message}
              </div>
            );
          })}
        </Flex>
      )}
    </div>
  );
};

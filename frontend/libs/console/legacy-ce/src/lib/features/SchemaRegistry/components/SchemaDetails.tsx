import React, { useMemo } from 'react';
import LZString from 'lz-string';
import moment from 'moment';
import { useGetSchema } from '../hooks/useGetSchema';
import { Tabs } from '../../../new-components/Tabs';
import { SchemaRow } from './SchemaRow';
import { ChangeSummary } from './ChangeSummary';
import { FindIfSubStringExists, schemaTransformFn } from '../utils';
import { Link } from 'react-router';
import { RoleBasedSchema, Schema } from '../types';
import { FaHome, FaAngleRight, FaFileImport, FaSearch } from 'react-icons/fa';
import { Input } from '../../../new-components/Form';
import AceEditor from 'react-ace';

export const Breadcrumbs = () => (
  <div className="flex items-center space-x-xs mb-4">
    <Link
      to="/settings/schema-registry"
      className="cursor-pointer flex items-center text-muted hover:text-gray-900"
    >
      <FaHome className="mr-1.5" />
      <span className="text-sm">Schema</span>
    </Link>
    <FaAngleRight className="text-muted" />
    <div className="cursor-pointer flex items-center text-yellow-500">
      <FaFileImport className="mr-1.5" />
      <span className="text-sm">Roles</span>
    </div>
  </div>
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
      return <SchemasDetails schema={transformedData} />;
    }
  }
};

const SchemasDetails: React.VFC<{
  schema: Schema;
}> = props => {
  const { schema } = props;

  const [tabState, setTabState] = React.useState('graphql');

  // We know for sure only one roleBasedSchema exists
  const roleBasedSchema = schema.roleBasedSchemas[0];
  const published = moment(schema.created_at);

  return (
    <div className="mx-4 mt-4">
      <Breadcrumbs />
      <div className="flex mb-sm">
        <span className="font-bold text-xl text-black mr-4">
          {roleBasedSchema.role}:{schema.entry_hash.substr(0, 16)}
        </span>
      </div>
      <div className="border-neutral-200 bg-white border w-3/5">
        <div className="w-full flex bg-gray-100 px-4 py-2">
          <div className="flex text-base w-[69%] justify-start">
            <span className="text-sm font-bold">SCHEMA</span>
          </div>
          <div className="flex text-base w-[28%] justify-between">
            <span className="text-sm font-bold">BREAKING</span>
            <span className="text-sm font-bold">DANGEROUS</span>
            <span className="text-sm font-bold">SAFE</span>
          </div>
        </div>

        <div className="ml-4 mb-2 ">
          <SchemaRow
            role={roleBasedSchema.role || ''}
            changes={roleBasedSchema.changes || []}
          />

          <div className="flex mt-4">
            <div className="flex-col w-1/4">
              <div className="font-bold text-gray-500">Published</div>
              <span>{published.fromNow()}</span>
            </div>
            <div className="flex-col w-3/4">
              <div className="font-bold text-gray-500">Hash</div>
              <span className="font-bold bg-gray-100 px-1 rounded text-sm">
                {roleBasedSchema.hash}
              </span>
            </div>
          </div>
        </div>

        <div className="w-full h-full">
          <Tabs
            value={tabState}
            onValueChange={state => setTabState(state)}
            headerTabBackgroundColor="bg-white"
            items={[
              {
                value: 'graphql',
                label: 'GraphQL',
                content: <SchemaView schema={roleBasedSchema.raw} />,
              },
              {
                value: 'changes',
                label: 'Changes',
                content: <ChangesView changes={roleBasedSchema.changes} />,
              },
            ]}
          />
        </div>
      </div>
    </div>
  );
};

export const SchemaView: React.VFC<{ schema: string }> = props => {
  const { schema } = props;
  const decompressedSchema = LZString.decompressFromBase64(schema);
  return (
    <div className="w-full p-sm">
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

export const ChangesView: React.VFC<{
  changes: RoleBasedSchema['changes'];
}> = props => {
  const { changes } = props;

  const [searchText, setSearchText] = React.useState('');
  const handleSearch = (e: React.ChangeEvent<HTMLInputElement>) =>
    setSearchText(e.target.value);

  const changesList = useMemo(() => {
    if (!searchText) return changes;

    return changes?.filter(change =>
      FindIfSubStringExists(change.message, searchText)
    );
  }, [searchText, changes]);

  if (!changes) {
    return <span>Could not compute what changed in this GraphQL Schema</span>;
  }

  if (!changes.length) {
    return <span className="ml-4">No changes!</span>;
  }

  const breakingChanges =
    changesList && changesList.filter(c => c.criticality.level === 'BREAKING');
  const dangerousChanges =
    changesList && changesList.filter(c => c.criticality.level === 'DANGEROUS');
  const safeChanges =
    changesList &&
    changesList.filter(c => c.criticality.level === 'NON_BREAKING');

  return (
    <div className="flex-col m-8">
      <div className="flex-col">
        <div className="font-bold text-md mb-8 text-gray-500">
          Change Summary
        </div>
        <ChangeSummary changes={changes} />
      </div>
      <div className="flex w-full border-b border-gray-300 my-8" />
      <div className="flex w-full mb-4 justify-between">
        <div className="flex font-bold text-gray-600">Changes</div>

        <label className="block">
          <Input
            type="text"
            placeholder="Search"
            name="search"
            icon={<FaSearch />}
            iconPosition="start"
            onChange={handleSearch}
          />
        </label>
      </div>
      {breakingChanges && breakingChanges.length > 0 && (
        <div className="flex flex-col">
          {breakingChanges.map(c => {
            return (
              <div className="text-red-600 border border-gray-400 p-2">
                {c.message}
              </div>
            );
          })}
        </div>
      )}
      {dangerousChanges && dangerousChanges.length > 0 && (
        <div className="flex flex-col">
          {dangerousChanges.map(c => {
            return (
              <div className="text-red-800 border border-gray-400 p-2">
                {c.message}
              </div>
            );
          })}
        </div>
      )}
      {safeChanges && safeChanges.length > 0 && (
        <div className="flex flex-col">
          {safeChanges.map(c => {
            return (
              <div className="text-green-600 border border-gray-400 p-2">
                {c.message}
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
};

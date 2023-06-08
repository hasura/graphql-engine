import React from 'react';
import { SchemaRow } from './SchemaRow';
import { useGetSchemaList } from '../hooks/useGetSchemaList';
import { Schema, RoleBasedSchema } from '../types';
import moment from 'moment';
import { browserHistory } from 'react-router';
import globals from '../../../Globals';
import { schemaListTransformFn } from '../utils';

export const SchemasList = () => {
  const projectID = globals.hasuraCloudProjectId || '';
  const fetchSchemaResponse = useGetSchemaList(projectID);

  const { kind } = fetchSchemaResponse;

  switch (kind) {
    case 'loading':
      return <p>Loading...</p>;
    case 'error':
      return <p>Error: {fetchSchemaResponse.message}</p>;
    case 'success': {
      const schemaList = schemaListTransformFn(fetchSchemaResponse.response);

      return <Tabularised schemas={schemaList} />;
    }
  }
};

export const Tabularised: React.VFC<{ schemas: Schema[] }> = props => {
  const { schemas } = props;
  return (
    <div className="overflow-x-auto rounded-sm border-neutral-200 bg-gray-100 border w-3/5">
      <div className="w-full flex bg-gray-100 px-4 py-2">
        <div className="flex text-base w-[69%] justify-start">
          <span className="text-sm font-bold">SCHEMA</span>
        </div>
        <div className="flex text-base w-[28%] justify-between">
          <span className="text-sm font-bold">BREAKING</span>
          <span className="text-sm font-bold">DANGEROUS</span>
          <span className="text-sm font-bold">SAFE</span>
        </div>
        {/* <div className="flex text-base w-[2%] justify-end">he</div> */}
      </div>
      <div className="flex flex-col w-full">
        {schemas.length ? (
          schemas.map(schema => {
            return (
              <SchemaCard
                createdAt={schema.created_at}
                schemaId={schema.id}
                hash={schema.hash}
                roleBasedSchemas={schema.roleBasedSchemas}
              />
            );
          })
        ) : (
          <div className="white border-t border-neutral-200">
            <div className="p-xs" data-test="label-no-domain-found">
              No schemas published to the schema registry yet
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

const SchemaCard: React.VFC<{
  createdAt: string;
  hash: string;
  schemaId: string;
  roleBasedSchemas: RoleBasedSchema[];
}> = props => {
  const { createdAt, hash, roleBasedSchemas } = props;

  const published = moment(createdAt);

  return (
    <div className="w-full flex-col px-4 py-2 mb-2 bg-white">
      <div className="flex flex-col w-1/2">
        <div className="flex mt-4">
          <div className="flex-col w-1/4">
            <div className="font-bold text-gray-500">Published</div>
            <span>{published.fromNow()}</span>
          </div>
          <div className="flex-col w-3/4">
            <div className="font-bold text-gray-500">Hash</div>
            <span className="font-bold bg-gray-100 px-1 rounded text-sm">
              {hash}
            </span>
          </div>
        </div>
      </div>

      <div className="flex-col w-full mt-8">
        {roleBasedSchemas.length ? (
          roleBasedSchemas.map((roleBasedSchema, index) => {
            return (
              <div className="flex-col w-full">
                <SchemaRow
                  role={roleBasedSchema.role || ''}
                  changes={roleBasedSchema.changes || []}
                  onClick={() => {
                    browserHistory.push(
                      `${globals.urlPrefix}/settings/schema-registry/${roleBasedSchema.id}`
                    );
                  }}
                />
                {!(index + 1 === roleBasedSchemas.length) ? (
                  <div className="flex w-full border-b border-gray-300" />
                ) : null}
              </div>
            );
          })
        ) : (
          <div className="white border-t border-neutral-200">
            <div className="p-xs" data-test="label-no-domain-found">
              No schemas published to the schema registry yet
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

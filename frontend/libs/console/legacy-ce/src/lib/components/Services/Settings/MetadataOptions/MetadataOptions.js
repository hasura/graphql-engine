import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import ExportMetadata from './ExportMetadata';
import ImportMetadata from './ImportMetadata';
import ReloadMetadata from './ReloadMetadata';
import ResetMetadata from './ResetMetadata';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';

const MetadataOptions = props => {
  const getMetadataImportExportSection = () => {
    return (
      <div>
        <div className="mb-md mt-sm pt-sm">
          <h4 className="text-lg font-bold">Import/Export metadata</h4>
          <div className="w-8/12 mt-sm">Get Hasura metadata as JSON.</div>
        </div>

        <div className="inline-block">
          <ExportMetadata {...props} />
        </div>

        <div className="inline-block">
          <ImportMetadata {...props} />
        </div>
      </div>
    );
  };

  const getMetadataUpdateSection = () => {
    return (
      <div>
        <div key="meta_data_1" className="mb-md mt-md ">
          <h4 className="text-lg font-bold">Reload metadata</h4>
          <div className="w-8/12 mt-sm">
            Refresh Hasura metadata, typically required if you have changes in
            the underlying databases or if you have updated your remote schemas.
          </div>
        </div>

        <div key="meta_data_2" className="flex">
          <ReloadMetadata {...props} />
        </div>

        <div key="meta_data_3" className="mb-md mt-md pt-sm">
          <h4 className="text-lg font-bold">Reset metadata</h4>
          <div className="w-8/12 mt-sm">
            Permanently clear GraphQL Engine's metadata and configure it from
            scratch (tracking relevant tables and relationships). This process
            is not reversible.
          </div>
        </div>

        <div key="meta_data_4">
          <ResetMetadata {...props} />
        </div>
      </div>
    );
  };

  return (
    <Analytics name="MetadataOptions" {...REDACT_EVERYTHING}>
      <div className={`clear-both pl-md mt-md mb-md bootstrap-jail`}>
        <h2 className="text-xl font-bold">Hasura Metadata Actions</h2>
        <div className="mt-xs">
          <div className="w-8/12">
            Hasura metadata stores information about your tables, relationships,
            permissions, etc. that is used to generate the GraphQL schema and
            API.
            <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/how-it-works/metadata-schema.html" />
          </div>

          {getMetadataImportExportSection()}

          {getMetadataUpdateSection()}
        </div>
      </div>
    </Analytics>
  );
};

const mapStateToProps = state => {
  return {
    ...state.main,
    metadata: state.metadata,
    dataHeaders: { ...state.tables.dataHeaders },
  };
};

const metadataOptsConnector = connect =>
  connect(mapStateToProps)(MetadataOptions);

export default metadataOptsConnector;

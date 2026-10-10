import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import ExportMetadata from './ExportMetadata';
import ImportMetadata from './ImportMetadata';
import ReloadMetadata from './ReloadMetadata';
import ResetMetadata from './ResetMetadata';
import { LearnMoreLink, Text } from '@hasura/shared/ui';
import { Flex, Heading } from '@radix-ui/themes';

const MetadataOptions = () => {
  const getMetadataImportExportSection = () => {
    return (
      <div className="my-4">
        <Flex className="mb-4" direction="column" gap="2">
          <Heading size="4">Import/Export metadata</Heading>
          <Text as="div">Get Hasura metadata as JSON.</Text>
        </Flex>

        <Flex gap="4">
          <ExportMetadata /> <ImportMetadata />
        </Flex>
      </div>
    );
  };

  const getMetadataUpdateSection = () => {
    return (
      <div>
        <Flex className="mb-4" direction="column" gap="2">
          <Heading size="4">Reload metadata</Heading>
          <Text as="div" className="mt-4">
            Refresh Hasura metadata, typically required if you have changes in
            the underlying databases or if you have updated your remote schemas.
          </Text>
        </Flex>

        <ReloadMetadata />

        <Flex className="my-4" direction="column" gap="2">
          <Heading size="4">Reset metadata</Heading>
          <Text as="p">
            Permanently clear GraphQL Engine&apos;s metadata and configure it
            from scratch (tracking relevant tables and relationships). This
            process is not reversible.
          </Text>
        </Flex>

        <ResetMetadata />
      </div>
    );
  };

  return (
    <Analytics name="MetadataOptions" {...REDACT_EVERYTHING}>
      <div className="p-4">
        <Heading size="6">Hasura Metadata Actions</Heading>
        <div className="mt-4 w-8/12">
          <div>
            <Text>
              Hasura metadata stores information about your tables,
              relationships, permissions, etc. that is used to generate the
              GraphQL schema and API.
            </Text>{' '}
            <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/how-it-works/metadata-schema.html" />
          </div>

          {getMetadataImportExportSection()}
          {getMetadataUpdateSection()}
        </div>
      </div>
    </Analytics>
  );
};

export default MetadataOptions;

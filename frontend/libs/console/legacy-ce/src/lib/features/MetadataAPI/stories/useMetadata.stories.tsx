import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { StoryObj, Meta } from '@storybook/react';
import React from 'react';
import { MetadataDataSource } from '../../../metadata/types';

import { useMetadata } from '../hooks/useMetadata';
import { MetadataSelector } from '../hooks/metadataSelectors';
import { handlers } from './mocks/handlers.mock';

// test transformer to ensure type checking works
const testTransformer = (
  args: {
    source: string;
    kind: MetadataDataSource['kind'];
  }[]
) => {
  return args.map(({ source, kind }) => ({
    transformedSource: `transformed ${source}`,
    transformedKind: `transformed ${kind}`,
  }));
};

function UseMetadata() {
  // use metadata can be used:
  // without any arguments -> returns all metadata
  const queryNoInput = useMetadata();
  // with a selector -> returns a "chunk" of metadata (this should still be formatted as per the metadata spec)
  const queryMetadataSelector = useMetadata(MetadataSelector.getAllDriversList);
  // with a selector and a transformer -> returns a "chunk" from the selector
  // and then transforms it into a relevant shape that the console understands
  const queryMetadataSelectorWithTransformer = useMetadata(
    MetadataSelector.getAllDriversList,
    testTransformer
  );

  const error =
    queryNoInput.error ||
    queryMetadataSelector.error ||
    queryMetadataSelectorWithTransformer.error;

  return (
    <div>
      {queryNoInput.isSuccess ? (
        <ReactJson collapsed src={queryNoInput.data} />
      ) : (
        'no response'
      )}
      {queryMetadataSelector.isSuccess ? (
        <ReactJson src={queryMetadataSelector.data} />
      ) : (
        'no response'
      )}
      {queryMetadataSelectorWithTransformer.isSuccess ? (
        <ReactJson src={queryMetadataSelectorWithTransformer.data} />
      ) : (
        'no response'
      )}

      {error ? <ReactJson src={error} /> : null}
    </div>
  );
}

export const Primary: StoryObj = {
  render: () => {
    return <UseMetadata />;
  },

  args: {
    database: 'default',
  },
};

export default {
  title: 'hooks/useMetadata',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers(),
  },
} as Meta;

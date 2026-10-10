import { JsonCodeBlock } from '@hasura/shared/ui';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { handlers } from './mocks/handlers.mock';
import { useMetadata } from '../hooks';
import { Source } from '@hasura/shared/types';
import { ReactQueryDecorator } from '@hasura/shared/testing';

// test transformer to ensure type checking works
const testTransformer = (
  args: {
    name: string;
    kind: Source['kind'];
  }[],
) => {
  return args.map(({ name, kind }) => ({
    transformedSource: `transformed ${name}`,
    transformedKind: `transformed ${kind}`,
  }));
};

function UseMetadata() {
  // use metadata can be used:
  // without any arguments -> returns all metadata
  const queryNoInput = useMetadata();
  // with a selector -> returns a "chunk" of metadata (this should still be formatted as per the metadata spec)
  const queryMetadataSelector = useMetadata((m) => m.metadata?.sources);
  // with a selector and a transformer -> returns a "chunk" from the selector
  // and then transforms it into a relevant shape that the console understands
  const queryMetadataSelectorWithTransformer = useMetadata((m) =>
    testTransformer(m.metadata?.sources ?? []),
  );

  const error =
    queryNoInput.error ||
    queryMetadataSelector.error ||
    queryMetadataSelectorWithTransformer.error;

  return (
    <div>
      {queryNoInput.isSuccess ? (
        <JsonCodeBlock value={queryNoInput.data} />
      ) : (
        'no response'
      )}
      {queryMetadataSelector.isSuccess ? (
        <JsonCodeBlock value={queryMetadataSelector.data} />
      ) : (
        'no response'
      )}
      {queryMetadataSelectorWithTransformer.isSuccess ? (
        <JsonCodeBlock value={queryMetadataSelectorWithTransformer.data} />
      ) : (
        'no response'
      )}

      {error ? <JsonCodeBlock value={error} /> : null}
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
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta;

import React from 'react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { StoryFn, Meta } from '@storybook/react';

import { handlers } from '../../../../mocks/metadata.mock';
import { useQueryCollections } from './useQueryCollections';

const UseQueryCollections: React.FC = () => {
  const { data, isLoading, isError } = useQueryCollections();

  if (isLoading) {
    return <div>Loading...</div>;
  }

  if (isError) {
    return <div>Error</div>;
  }

  return data ? <ReactJson name="query_collections" src={data} /> : null;
};

export const Primary: StoryFn = () => {
  return <UseQueryCollections />;
};

export default {
  title: 'hooks/Query Collections/useQueryCollections',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta;

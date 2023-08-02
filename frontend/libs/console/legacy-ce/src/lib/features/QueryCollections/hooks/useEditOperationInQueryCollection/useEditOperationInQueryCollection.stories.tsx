import React from 'react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../../storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { Button } from '../../../../new-components/Button';
import { StoryFn, Meta } from '@storybook/react';

import { handlers } from '../../../../mocks/metadata.mock';
import { useEditOperationInQueryCollection } from '.';

const UseEditOperationInQueryCollection: React.FC = () => {
  const { editOperationInQueryCollection, isSuccess, isLoading, error } =
    useEditOperationInQueryCollection();

  return (
    <div>
      <ReactJson
        name="Hook State"
        src={{
          isSuccess,
          isLoading,
          error: error?.message,
        }}
      />
      <Button
        onClick={() =>
          editOperationInQueryCollection('testCollection', 'MyQuery', {
            name: 'NewMyQuery',
            query: 'query NewMyQuery { user { email name}}',
          })
        }
      >
        Edit MyQuery to NewMyQuery
      </Button>
    </div>
  );
};

export const Primary: StoryFn = () => {
  return <UseEditOperationInQueryCollection />;
};

export default {
  title: 'hooks/Query Collections/useEditOperationInQueryCollection',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers({ delay: 500 }),
  },
} as Meta;

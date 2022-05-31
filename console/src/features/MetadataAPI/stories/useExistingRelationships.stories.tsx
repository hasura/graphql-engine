import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { ReduxDecorator } from '@/storybook/decorators/redux-decorator';
import ReactJson from 'react-json-view';
import { Meta, Story } from '@storybook/react';
import React from 'react';

import { useExistingRelationships } from '../hooks/useMetadataTables';
import { handlers } from './mocks/handlers.mock';

function UseLocalRelationships() {
  const { data: arrayRelationship } = useExistingRelationships('default', {
    name: 'user',
    schema: 'public',
  });

  const { data: objectRelationship } = useExistingRelationships('default', {
    name: 'product',
    schema: 'public',
  });

  return (
    <div>
      <p>Array relationships</p>
      {arrayRelationship ? (
        <ReactJson src={arrayRelationship} />
      ) : (
        'no response'
      )}

      <p>Object relationships</p>
      {objectRelationship ? (
        <ReactJson src={objectRelationship} />
      ) : (
        'no response'
      )}
    </div>
  );
}

export const Primary: Story = () => {
  return <UseLocalRelationships />;
};

Primary.args = {
  database: 'default',
};

export default {
  title: 'hooks/useExistingRelationships',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: handlers(),
  },
} as Meta;

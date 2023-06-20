import React from 'react';
import { handlers } from '../../../mocks/metadata.mock';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { ReduxDecorator } from '../../../storybook/decorators/redux-decorator';
import { StoryObj, Meta } from '@storybook/react';

import { useCreateRestEndpoints } from './useCreateRestEndpoints';
import { Button } from '../../../new-components/Button';
import ReactJson from 'react-json-view';
import { rest } from 'msw';
import introspectionSchema from './mocks/introspectionWithoutCustomizations.json';
import { useMetadata } from '../../MetadataAPI';
import { userEvent, waitFor, within } from '@storybook/testing-library';
import { expect } from '@storybook/jest';

const UseCreateRestEndpoints: React.FC = () => {
  const { createRestEndpoints, isLoading, isError, isSuccess, isReady } =
    useCreateRestEndpoints();

  const { data: metadata } = useMetadata();

  return (
    <div>
      <Button
        onClick={() =>
          createRestEndpoints('user', [
            'VIEW',
            'VIEW_ALL',
            'DELETE',
            'CREATE',
            'UPDATE',
          ])
        }
      >
        Create REST Endpoint
      </Button>
      <div data-testid="ready-state">Is Ready: {JSON.stringify(isReady)}</div>
      <div data-testid="loading-state">
        Is Error: {JSON.stringify(isLoading)}
      </div>
      <div data-testid="success-state">
        Is Success: {JSON.stringify(isSuccess)}
      </div>
      <div data-testid="error-state">Is Error: {JSON.stringify(isError)}</div>
      <div>
        <ReactJson src={{ metadata }} name="metadata" collapsed={1} />
      </div>
    </div>
  );
};

export const Primary: StoryObj = {
  render: args => {
    return <UseCreateRestEndpoints {...args} />;
  },

  args: {
    collectionName: 'rest-endpoint',
  },
};

Primary.play = async ({ canvasElement }: any) => {
  const canvas = within(canvasElement);

  await waitFor(
    () =>
      expect(canvas.getByTestId('ready-state')).toHaveTextContent(
        'Is Ready: true'
      ),

    {
      timeout: 5000,
    }
  );
  await userEvent.click(await canvas.findByText('Create REST Endpoint'));

  await canvas.findByTestId('success-state');
  await waitFor(
    () =>
      expect(canvas.getByTestId('success-state')).toHaveTextContent(
        'Is Success: true'
      ),
    {
      timeout: 5000,
    }
  );
};

export default {
  title: 'features/Rest endpoints/hooks/useCreateRestEndpoint',
  decorators: [
    ReduxDecorator({ tables: { currentDataSource: 'default' } }),
    ReactQueryDecorator(),
  ],
  parameters: {
    msw: [
      ...handlers({ delay: 500 }),
      rest.post(`http://localhost:8080/v1/graphql`, async (req, res, ctx) => {
        return res(ctx.json(introspectionSchema));
      }),
    ],
  },
} as Meta;

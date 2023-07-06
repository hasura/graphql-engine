import React from 'react';
import { RestEndpointModal } from './RestEndpointModal';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { handlers } from '../../../../../mocks/metadata.mock';
import { Meta } from '@storybook/react';
import { rest } from 'msw';
import introspectionSchema from '../../../../../features/RestEndpoints/hooks/mocks/introspectionWithoutCustomizations.json';

export default {
  title: 'Features/REST endpoints/Modal',
  component: RestEndpointModal,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: [
      ...handlers({ delay: 500 }),
      rest.post(`http://localhost:8080/v1/graphql`, async (req, res, ctx) => {
        return res(ctx.json(introspectionSchema));
      }),
    ],
  },
} as Meta<typeof RestEndpointModal>;

export const Base = () => (
  <RestEndpointModal tableName="user" onClose={() => {}} />
);

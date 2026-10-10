import { RestEndpointModal } from './RestEndpointModal';
import { ReactQueryDecorator, handlers } from '@hasura/shared/testing';
import { Meta } from '@storybook/react-webpack5';
import { http, HttpResponse } from 'msw';
import { introspectionFromSchema } from 'graphql';

export default {
  title: 'Features/REST endpoints/Modal',
  component: RestEndpointModal,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: [
      ...handlers({ delay: 500 }),
      http.post(`http://localhost:8080/v1/graphql`, async () => {
        return HttpResponse.json(introspectionFromSchema);
      }),
    ],
  },
} as Meta<typeof RestEndpointModal>;

export const Base = () => (
  <RestEndpointModal
    tableName="user"
    dataSourceName="default"
    table={{ name: 'test', schema: 'test-schema' }}
    onClose={() => {}}
  />
);

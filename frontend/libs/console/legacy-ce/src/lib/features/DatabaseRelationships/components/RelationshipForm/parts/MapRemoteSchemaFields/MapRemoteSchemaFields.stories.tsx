import { useConsoleForm } from '../../../../../../new-components/Form';
import { z } from 'zod';
import { Controller } from 'react-hook-form';
import { Button } from '../../../../../../new-components/Button';
import { ReactQueryDecorator } from '../../../../../../storybook/decorators/react-query';
import { StoryFn, Meta } from '@storybook/react';
import { MapRemoteSchemaFields } from './MapRemoteSchemaFields';
import { useRemoteSchemaIntrospection } from '../../../../hooks/useRemoteSchema';
import { handlers } from '../../../../mocks/handler.mock';

export default {
  component: MapRemoteSchemaFields,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof MapRemoteSchemaFields>;

export const StandaloneComponent: StoryFn<
  typeof MapRemoteSchemaFields
> = () => {
  const { data: remoteSchemaGraphQLSchema, isLoading } =
    useRemoteSchemaIntrospection({
      remoteSchemaName: 'trevorBladeCountriesAPI',
    });

  if (isLoading) return <>Loading...</>;

  if (!remoteSchemaGraphQLSchema) return <>no data</>;

  return (
    <MapRemoteSchemaFields
      graphQLSchema={remoteSchemaGraphQLSchema}
      onChange={result => {
        console.log(result);
      }}
    />
  );
};

export const WithReactHookForm: StoryFn<typeof MapRemoteSchemaFields> = () => {
  const { data: remoteSchemaGraphQLSchema, isLoading } =
    useRemoteSchemaIntrospection({
      remoteSchemaName: 'trevorBladeCountriesAPI',
    });

  const {
    methods: { control },
    Form,
  } = useConsoleForm({
    schema: z.object({
      remote_schema_field_mapping: z.any(),
    }),
    options: {
      defaultValues: {
        remote_schema_field_mapping: {
          country: { arguments: { code: '$AlbumId' } },
        },
      },
    },
  });

  if (isLoading) return <>Loading...</>;

  if (!remoteSchemaGraphQLSchema) return <>no data</>;

  return (
    <Form onSubmit={console.log}>
      <Controller
        control={control}
        name="remote_schema_field_mapping"
        render={({ field: { onChange, value } }) => (
          <MapRemoteSchemaFields
            graphQLSchema={remoteSchemaGraphQLSchema}
            onChange={onChange}
            defaultValue={value}
          />
        )}
      />
      <Button type="submit">Submit</Button>
    </Form>
  );
};

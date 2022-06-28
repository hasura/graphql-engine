import React from 'react';
import axios from 'axios';
import { DataSource } from '@/features/DataSource';
import { Button } from '@/new-components/Button';
import { Form } from '@/new-components/Form';
import { useQuery } from 'react-query';
import { Driver } from './Driver';
import { Name } from './Name';
import { Configuration } from './Configuration';
import { useMetadataMigration } from '../MetadataAPI';

const usePossibleFormSchemas = () => {
  const fetch = axios.create();
  return useQuery({
    queryKey: ['validation-schemas'],
    queryFn: async () => {
      return DataSource(fetch).connectDB.getFormSchema();
    },
  });
};

export const Connect = () => {
  const metadataMutation = useMetadataMigration();

  const { data: schemas } = usePossibleFormSchemas();
  if (!schemas) return <>unable to retrieve any validation schema</>;

  return (
    <Form
      schema={schemas}
      onSubmit={values => {
        console.log('form data', values);
        metadataMutation.mutate({
          query: {
            type: 'pg_add_source',
            args: values,
          },
        });
      }}
      options={{
        defaultValues: {
          name: '',
          driver: 'postgres',
        },
      }}
    >
      {options => {
        console.log(options.formState.errors);
        return (
          <div className="w-1/2">
            <Driver name="driver" />
            <Name name="name" />
            <Configuration name="configuration" />
            <Button type="submit">Connect</Button>
          </div>
        );
      }}
    </Form>
  );
};

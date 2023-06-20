import React from 'react';
import z from 'zod';
import { ComponentMeta } from '@storybook/react';
import { RestEndpointModal, modalSchema } from './RestEndpointModal';
import { SimpleForm } from '../../../../../new-components/Form';

export default {
  title: 'Features/REST endpoints/Modal',
  component: RestEndpointModal,
} as ComponentMeta<typeof RestEndpointModal>;

const modalInitialValues: z.infer<typeof modalSchema> = {
  tableName: 'Table',
  methods: ['CREATE', 'DELETE'],
};

export const Base = () => (
  <SimpleForm
    schema={modalSchema}
    options={{ defaultValues: modalInitialValues }}
    onSubmit={() => {}}
  >
    <RestEndpointModal />
  </SimpleForm>
);

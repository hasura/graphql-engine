import {
  Badge,
  Dialog,
  DialogFooter,
  SimpleForm,
  LearnMoreLink,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import React from 'react';
import z from 'zod';
import { ImportTypesForm } from './ImportTypesForm';
import { schema } from './utils';

interface ImportTypesModalProps {
  currentValue: string;
  onInsertTypes: (types: string) => void;
  onClose: () => void;
  isOpen: boolean;
}
type SchemaType = z.infer<typeof schema>;

export const ImportTypesModal = (props: ImportTypesModalProps) => {
  const { isOpen, onClose, onInsertTypes, currentValue } = props;
  const [values, setValues] = React.useState<SchemaType>({
    selectedTypes: [],
    typeDef: '',
  });

  if (!isOpen) {
    return null;
  }

  const title = () => {
    return (
      <Flex className="align center">
        Import Types from Tables
        <Badge className="mx-2" color="blue">
          BETA
        </Badge>
      </Flex>
    );
  };

  return (
    <Dialog
      size="xl"
      footer={
        <DialogFooter
          onSubmit={() => {
            onInsertTypes(`${currentValue}
${values.typeDef}`);
            onClose();
          }}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Insert Types"
          onSubmitAnalyticsName="actions-tab-btn-import-table-types"
          onCancelAnalyticsName="actions-tab-btn-cancel-table-types"
        />
      }
      title={title()}
      onClose={onClose}
    >
      <div className="px-sm">
        <p className="text-muted mb-6">
          Generate type definition from the current state of a table.
          <LearnMoreLink href="https://github.com/hasura/graphql-engine/discussions/9273" />
        </p>
        <SimpleForm
          options={{
            defaultValues: values,
          }}
          className="pl-0 pr-0"
          schema={schema}
          onSubmit={() => {}}
        >
          <ImportTypesForm setValues={setValues} />
        </SimpleForm>
      </div>
    </Dialog>
  );
};

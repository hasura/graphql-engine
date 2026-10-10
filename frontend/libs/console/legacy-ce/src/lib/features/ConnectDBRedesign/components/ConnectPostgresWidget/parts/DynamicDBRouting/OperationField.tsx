import { InputField, SelectField } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

export const OperationField = () => {
  return (
    <Flex>
      <div>
        <SelectField
          name="validation.operation_type"
          options={[
            { label: 'Query', value: 'query' },
            { label: 'Mutation', value: 'mutation' },
            { label: 'Subscription', value: 'subscription' },
          ]}
        />
      </div>
      <div className="flex-1">
        <InputField
          name="validation.operation_name"
          fieldProps={{
            type: 'text',
            placeholder: 'Operation Name',
          }}
        />
      </div>
    </Flex>
  );
};

import React from 'react';
import { InputField, Select } from '../../../../../../new-components/Form';

export const OperationField = () => {
  return (
    <div className="flex">
      <div>
        <Select
          name="validation.operation_type"
          selectClassName="rounded-r-none bg-slate-100 font-semibold color-slate-900"
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
          type="text"
          placeholder="Operation Name"
          inputClassName="rounded-l-none"
        />
      </div>
    </div>
  );
};

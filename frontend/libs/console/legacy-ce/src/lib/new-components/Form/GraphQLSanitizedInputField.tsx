import {
  sanitizeGraphQLFieldNames,
  SanitizeTipsMessages,
} from '../../utils/sanitizeGraphQLFieldNames';
import React from 'react';
import { z } from 'zod';
import { InputField, InputFieldProps, Schema } from './InputField';

export type GraphQLSanitizedInputFieldProps<T extends z.infer<Schema>> =
  InputFieldProps<T> & {
    hideTips?: boolean;
  };

export const GraphQLSanitizedInputField = <T extends z.infer<Schema>>({
  hideTips,
  inputTransform,
  ...props
}: GraphQLSanitizedInputFieldProps<T>) => {
  const handleTransform = (x: string): string => {
    // do the sanitization first
    let value = sanitizeGraphQLFieldNames(x);

    // if an inputTransform is also passed, we run it after sanitization
    if (inputTransform) {
      value = inputTransform(value);
    }

    return value;
  };

  return (
    <div>
      <InputField
        {...props}
        renderDescriptionLineBreaks={!hideTips}
        description={
          !hideTips ? SanitizeTipsMessages.join('\n') : props?.description
        }
        inputTransform={handleTransform}
      />
    </div>
  );
};

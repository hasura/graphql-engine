import React, { useEffect } from 'react';
import clsx from 'clsx';
import get from 'lodash.get';
import { FieldError, useFormContext } from 'react-hook-form';
import {
  FieldWrapper,
  FieldWrapperPassThroughProps,
} from '@/new-components/Form';
import { parseQueryString, readFile } from './utils';

export type GraphQLFileUploadProps = FieldWrapperPassThroughProps & {
  /**
   * The input field name used by react-hook-form (to store bulk data && validation errors
   */
  name: string;
  /**
   * The input field classes
   */
  className?: string;
};

/**
 * GraphQLFileUpload
 * GraphQLFileUpload is a custom implementation that parses the uploaded file and set it to existing react hook form context
 *
 */
export const GraphQLFileUpload: React.FC<GraphQLFileUploadProps> = ({
  name,
  dataTest,
  ...wrapperProps
}: GraphQLFileUploadProps) => {
  const {
    setValue,
    reset,
    setError,
    formState: { errors },
  } = useFormContext();
  const handleFileUpload = (e: React.ChangeEvent<HTMLInputElement>) => {
    const files = e.target.files;

    readFile(files![0], data => {
      try {
        const parsedData = parseQueryString(data);
        setValue(name, parsedData);
      } catch (error) {
        setError(name, { type: 'custom', message: 'Invalid GraphQL query' });
      }
    });
  };

  const maybeError = get(errors, name) as FieldError | undefined;

  useEffect(() => {
    return () => {
      // reset the form when the component is unmounted
      reset();
    };
  }, [reset]);

  return (
    <FieldWrapper id={name} {...wrapperProps} error={maybeError}>
      <div className={clsx('relative flex max-w-xl')}>
        <input
          type="file"
          id={name}
          aria-invalid={maybeError ? 'true' : 'false'}
          aria-label={wrapperProps.label}
          data-test={dataTest}
          data-testid={name}
          onChange={handleFileUpload}
        />
      </div>
    </FieldWrapper>
  );
};

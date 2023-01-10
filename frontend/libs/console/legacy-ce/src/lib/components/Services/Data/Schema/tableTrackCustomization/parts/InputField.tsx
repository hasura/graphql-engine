import { TrackingTableFormValues } from '@/components/Services/Data/Schema/tableTrackCustomization/types';
import { Button } from '@/new-components/Button';
import { sanitizeGraphQLFieldNames } from '@/utils';
import clsx from 'clsx';
import React from 'react';
import { useFormContext, useWatch } from 'react-hook-form';
import { FaTimes } from 'react-icons/fa';

interface InputFieldProps
  extends Omit<React.HTMLProps<HTMLInputElement>, 'onChange'> {
  fieldName: keyof TrackingTableFormValues;
  label: string;
  onClear?: () => void;
  onChange?: (value: string) => void;
}

// TODO NEXT: replace with InputField component when the horizontal version is ready
export const InputField: React.VFC<InputFieldProps> = ({
  label,
  fieldName,
  onClear,
  onChange,
  className,
  ...props
}) => {
  const { register, setValue } = useFormContext<TrackingTableFormValues>();

  const value = useWatch<TrackingTableFormValues>({ name: fieldName });

  const hasValue = React.useMemo(() => !!value, [value]);

  const fieldMethods = register(fieldName);

  return (
    <div className="flex items-center">
      <div className="grid grid-cols-12 gap-3 flex-grow">
        <div className="col-span-4 flex items-center">
          <label className="block font-normal">{label}</label>
        </div>
        <div className="col-span-8">
          <input
            type="text"
            autoCapitalize="off"
            autoComplete="off"
            className={clsx(
              'block w-full text-black placeholder:text-gray-400 text-lg h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400 flex-grow rounded-r-none',
              className
            )}
            {...fieldMethods}
            {...props}
            onChange={e => {
              e.target.value = sanitizeGraphQLFieldNames(e.target.value);
              fieldMethods.onChange(e);
              if (onChange) {
                onChange(e.target.value);
              }
            }}
          />
        </div>
      </div>
      <Button
        className="border-l-0 rounded-l-none"
        disabled={!hasValue}
        onClick={() => {
          setValue(fieldName, '');
          if (onClear) onClear();
        }}
        icon={
          <FaTimes className="w-5 h-5 cursor-pointer mr-0 text-gray-400 fill-current hover:text-gray-500" />
        }
      />
    </div>
  );
};

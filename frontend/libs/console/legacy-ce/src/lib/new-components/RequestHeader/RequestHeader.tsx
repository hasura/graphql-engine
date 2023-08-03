import React, { ReactText } from 'react';
import { useFieldArray } from 'react-hook-form';
import { FaPlusCircle, FaShieldAlt } from 'react-icons/fa';
import { Button } from '../Button';
import { RequestHeadersSchema } from './schema';
import { KeyValue } from './components/KeyValue';
import { IconTooltip } from '../Tooltip';

export interface RequestHeadersProps {
  name: string;
  addButtonText?: ReactText;
}

export const RequestHeaders = (props: RequestHeadersProps) => {
  const { name, addButtonText = 'Add' } = props;
  const { fields, append, remove } = useFieldArray<
    Record<string, RequestHeadersSchema>
  >({
    name,
  });
  const thereIsAtLeastOneField = fields.length > 0;

  return (
    <div>
      {thereIsAtLeastOneField ? (
        <>
          <div className="grid gap-3 grid-cols-2 mb-sm">
            <label className="block text-gray-600 font-medium mb-xs">Key</label>
            <label className="block text-gray-600 font-medium mb-xs flex">
              Value
              <IconTooltip
                message={
                  <div>
                    Value can be either static string or a template which can
                    reference environment variables.
                    <p>Example:</p>
                    <p>Static string: "abc"</p>
                    <p>
                      Template with environment variables:
                      &#123;&#123;ACTION_BASE_URL&#125;&#125;/payment" or
                      &#123;&#123;FULL_ACTION_URL&#125;&#125;
                    </p>
                  </div>
                }
                icon={<FaShieldAlt className="h-4 text-muted cursor-pointer" />}
              />
            </label>
          </div>

          {fields.map((field, index) => (
            <KeyValue
              key={field.id}
              fieldId={field.id}
              fieldName={name}
              rowIndex={index}
              removeRow={remove}
            />
          ))}
        </>
      ) : null}

      <Button
        data-testid="add-header"
        icon={<FaPlusCircle />}
        onClick={() => {
          append({
            name: '',
            value: '',
          });
        }}
        size="sm"
      >
        {addButtonText}
      </Button>
    </div>
  );
};

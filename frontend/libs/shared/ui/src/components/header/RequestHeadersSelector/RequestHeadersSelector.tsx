import React from 'react';
import { useFieldArray } from 'react-hook-form';
import { FaPlusCircle } from 'react-icons/fa';
import { Button } from '../../Button';
import { RequestHeadersSelectorSchema } from './schema';
import { KeyValueHeader } from './components/KeyValueHeader';
import { Table } from '../../table/Table';
import { IconTooltip } from '../../Tooltip';
import { Flex } from '@radix-ui/themes';

export interface RequestHeadersSelectorProps {
  name: string;
  addButtonText?: React.ReactNode;
  typeSelect?: boolean;
  disabled?: boolean;
  /**
   * Called right before a new (empty) header row is appended. Useful for
   * callers that need to track whether the user has explicitly opted in to
   * managing this header list (e.g. separate introspection headers).
   */
  onAdd?: () => void;
}

export const RequestHeadersSelector = ({
  name,
  addButtonText = 'Add',
  typeSelect = true,
  disabled,
  onAdd,
}: RequestHeadersSelectorProps) => {
  const { fields, append, remove } = useFieldArray<
    Record<string, RequestHeadersSelectorSchema>
  >({
    name,
  });
  const thereIsAtLeastOneField = fields.length > 0;

  return (
    <div>
      {thereIsAtLeastOneField ? (
        <Table.Root>
          <Table.Header>
            <Table.Row>
              <Table.RowHeaderCell>Key</Table.RowHeaderCell>
              {typeSelect ? (
                <Table.RowHeaderCell>Type</Table.RowHeaderCell>
              ) : (
                <></>
              )}
              <Table.RowHeaderCell>
                <Flex align="center" gap="2">
                  Value
                  <IconTooltip
                    message={
                      <div>
                        Value can be either static string or a template which
                        can reference environment variables.
                        <p>Example:</p>
                        <p>Static string: &quot;abc&quot;</p>
                        <p>
                          Template with environment variables:
                          &#123;&#123;ACTION_BASE_URL&#125;&#125;/payment or
                          &#123;&#123;FULL_ACTION_URL&#125;&#125;
                        </p>
                      </div>
                    }
                  />
                </Flex>
              </Table.RowHeaderCell>
            </Table.Row>
          </Table.Header>
          <Table.Body>
            {fields.map((field, index) => (
              <KeyValueHeader
                key={field.id}
                fieldName={name}
                rowIndex={index}
                typeSelect={typeSelect}
                removeRow={remove}
                disabled={disabled}
              />
            ))}
          </Table.Body>
        </Table.Root>
      ) : null}

      <div className="mt-4">
        <Button
          data-testid="add-header"
          mode="default"
          size="1"
          leftIcon={FaPlusCircle}
          onClick={() => {
            onAdd?.();
            append({
              name: '',
              value: '',
              type: 'value',
            });
          }}
          disabled={disabled}
        >
          {addButtonText}
        </Button>
      </div>
    </div>
  );
};

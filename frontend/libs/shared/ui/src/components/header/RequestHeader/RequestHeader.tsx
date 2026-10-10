import React from 'react';
import { useFieldArray } from 'react-hook-form';
import { FaPlusCircle, FaShieldAlt } from 'react-icons/fa';
import { Button } from '../../Button';
import { RequestHeadersSchema } from './schema';
import { KeyValue } from './components/KeyValue';
import { IconTooltip } from '../../Tooltip';
import { Text } from '../../typography';
import { Flex, Grid } from '@radix-ui/themes';

export interface RequestHeadersProps {
  name: string;
  addButtonText?: React.ReactNode;
}

export const RequestHeaders = ({
  name,
  addButtonText = 'Add',
}: RequestHeadersProps) => {
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
          <Grid columns="2">
            <Text weight="medium">Key</Text>
            <Flex className="mb-2" gap="2" align="center">
              <Text weight="medium">Value</Text>
              <IconTooltip
                message={
                  <div>
                    Value can be either static string or a template which can
                    reference environment variables.
                    <p>Example:</p>
                    <p>Static string: &quot;abc&quot;</p>
                    <p>
                      Template with environment variables:
                      &#123;&#123;ACTION_BASE_URL&#125;&#125;/payment or
                      &#123;&#123;FULL_ACTION_URL&#125;&#125;
                    </p>
                  </div>
                }
                icon={<FaShieldAlt />}
              />
            </Flex>
          </Grid>

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
        mode="default"
        data-testid="add-header"
        leftIcon={FaPlusCircle}
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

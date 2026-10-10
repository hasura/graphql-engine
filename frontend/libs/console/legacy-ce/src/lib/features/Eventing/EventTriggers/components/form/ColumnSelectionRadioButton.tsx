import { IconTooltip, RadioGroup, Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import React from 'react';

interface InputProps extends React.ComponentProps<'input'> {
  isAllColumnChecked: boolean;
  handleColumnRadioButton: (value: boolean) => void;
  readOnly: boolean;
}

export const ColumnSelectionRadioButton: React.FC<InputProps> = ({
  isAllColumnChecked,
  handleColumnRadioButton,
  readOnly,
}) => {
  return (
    <div className="mt-2">
      <RadioGroup
        value={isAllColumnChecked ? 'true' : 'false'}
        orientation="horizontal"
        onChange={(value) => {
          handleColumnRadioButton(value === 'true');
        }}
        options={[
          {
            value: 'true',
            label: (
              <Flex align="center" gap="2">
                <Text>All columns</Text>
                <IconTooltip
                  side="top"
                  message="All columns will automatically include (or remove) any columns that are added (or removed) after creating the Event Trigger"
                />
              </Flex>
            ),
            disabled: readOnly,
          },
          {
            value: 'false',
            label: 'Choose columns',
            disabled: readOnly,
          },
        ]}
      />
    </div>
  );
};

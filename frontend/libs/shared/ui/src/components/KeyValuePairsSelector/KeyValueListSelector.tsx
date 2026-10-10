import { useFieldArray, useFormContext } from 'react-hook-form';
import { Button, IconButtonDelete } from '../Button';
import { FaPlusCircle } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { CheckboxField, InputField } from '../Form';

interface KeyValueListSelectorProps {
  addLabel?: string;
  name: string;
}

export const KeyValueListSelector = (props: KeyValueListSelectorProps) => {
  const { addLabel, name } = props;

  const { control } = useFormContext();
  const { fields, append, remove } = useFieldArray({
    control,
    name,
  });

  const addRow = () => {
    append({ key: '', value: '', checked: false });
  };

  return (
    <div>
      {fields.map((field: any, index) => (
        <Flex key={field.id} align="center" gap="2" className="mb-2">
          <div className="w-6">
            <CheckboxField
              name={`${name}.${index}.checked`}
              noErrorPlaceholder
            />
          </div>
          <InputField
            name={`${name}.${index}.key`}
            noErrorPlaceholder
            fieldProps={{
              placeholder: 'Key',
            }}
          />
          <InputField
            key={`${name}-${field.key}-value`}
            name={`${name}.${index}.value`}
            noErrorPlaceholder
            fieldProps={{
              placeholder: 'Value',
            }}
          />
          <div className="w-10">
            {index > 0 && <IconButtonDelete onClick={() => remove(index)} />}
          </div>
        </Flex>
      ))}
      <Button
        onClick={addRow}
        leftIcon={FaPlusCircle}
        size="sm"
        mode="default"
        className="mt-2"
      >
        {addLabel || 'Add'}
      </Button>
    </div>
  );
};

export default KeyValueListSelector;

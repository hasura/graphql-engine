import { useImperativeHandle, useState } from 'react';
import { Flex } from '@radix-ui/themes';
import { FaPlus, FaTrash } from 'react-icons/fa';
import { Button, IconButton, Input } from '@hasura/shared/ui';
import { TableColumn } from '@hasura/metadata/data-source';
import { isArrayString, parsePostgresArrayLiteral } from '@hasura/shared/utils';
import { convertTableValue } from '../InsertRowForm.utils';

type ArrayInputProps = {
  name?: string;
  /** console data type of the elements */
  itemType?: TableColumn['arrayItemType'];
  disabled?: boolean;
  /** an array, or its JSON or Postgres literal text */
  initialValue?: unknown;
  /**
   * Exposes the items as `value`, so the row can read and clear it like a
   * text input.
   */
  ref?: React.Ref<unknown>;
  onValueChange: (e: { target: { value: unknown } }) => void;
  onCheckValueRadio: () => void;
};

const toText = (item: unknown) =>
  item === null || item === undefined
    ? ''
    : typeof item === 'object'
      ? JSON.stringify(item)
      : String(item);

const toItems = (value: unknown): string[] => {
  if (Array.isArray(value)) return value.map(toText);
  if (typeof value !== 'string' || !value) return [];

  const items = isArrayString(value)
    ? (JSON.parse(value) as unknown[])
    : parsePostgresArrayLiteral(value);
  return items ? items.map(toText) : [value];
};

const isTextItemType = (itemType: TableColumn['arrayItemType']) =>
  !itemType || itemType === 'string' || itemType === 'text';

/** An empty item is a `NULL` element, except in a text array. */
const toItemValue = (
  item: string,
  itemType: TableColumn['arrayItemType'],
): unknown =>
  item === '' && !isTextItemType(itemType)
    ? null
    : convertTableValue(item, itemType);

export const ArrayInput = ({
  name,
  itemType,
  disabled,
  initialValue,
  ref,
  onValueChange,
  onCheckValueRadio,
}: ArrayInputProps) => {
  const [items, setItems] = useState(() => toItems(initialValue));

  useImperativeHandle(
    ref,
    () => ({
      // empty when there is no item, so a reset knows there is nothing to clear
      get value() {
        return items.length ? items : '';
      },
      set value(newValue: unknown) {
        setItems(toItems(newValue));
      },
    }),
    [items],
  );

  const updateItems = (newItems: string[]) => {
    setItems(newItems);
    onValueChange({
      target: { value: newItems.map((item) => toItemValue(item, itemType)) },
    });
    onCheckValueRadio();
  };

  const isNumeric =
    itemType === 'integer' || itemType === 'number' || itemType === 'float';

  return (
    <Flex direction="column" gap="2" data-testid={`${name}-array-input`}>
      {items.map((item, index) => (
        <Flex key={index} align="center" gap="2">
          <Input
            name={`${name}[${index}]`}
            aria-label={`${name} item ${index + 1}`}
            value={item}
            disabled={disabled}
            inputMode={isNumeric ? 'decimal' : undefined}
            placeholder={isTextItemType(itemType) ? undefined : 'NULL'}
            containerClassName="grow"
            onChange={(e) =>
              updateItems(
                items.map((current, i) =>
                  i === index ? e.target.value : current,
                ),
              )
            }
          />
          <IconButton
            type="button"
            color="gray"
            variant="ghost"
            size="1"
            icon={FaTrash}
            aria-label={`Remove ${name} item ${index + 1}`}
            disabled={disabled}
            onClick={() => updateItems(items.filter((_, i) => i !== index))}
          />
        </Flex>
      ))}
      <div>
        <Button
          type="button"
          mode="default"
          size="sm"
          leftIcon={FaPlus}
          disabled={disabled}
          onClick={() => updateItems([...items, ''])}
        >
          Add item
        </Button>
      </div>
    </Flex>
  );
};

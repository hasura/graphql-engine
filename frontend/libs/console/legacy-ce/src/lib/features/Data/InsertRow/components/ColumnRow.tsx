import { useEffect, useRef } from 'react';
import { Code, Flex } from '@radix-ui/themes';
import { InputCustomEvent } from './TextInput';
import { ColumnRowInput } from './ColumnRowInput';
import { TableColumn } from '@hasura/metadata/data-source';
import { Text, Radio } from '@hasura/shared/ui';

type RowValue = {
  columnName: string;
  selectionType: 'value' | 'null' | 'default';
  value?: unknown;
};

export type ColumnRowProps = {
  isDisabled: boolean;
  isNullDisabled: boolean;
  isDefaultDisabled: boolean;
  label: string;
  name: string;
  /** the database type, shown next to the input */
  dataType: TableColumn['dataType'];
  /** the generic data type, which picks the input */
  consoleDataType: TableColumn['consoleDataType'];
  /** the element data type of an array column */
  arrayItemType?: TableColumn['arrayItemType'];
  onChange: ({ columnName, selectionType, value }: RowValue) => void;
  resetToken: string;
  placeholder: string;
  initialValue?: unknown;
  /** allowed values when the column references an enum table */
  enumValues?: string[];
};

export const ColumnRow = ({
  isDisabled,
  isNullDisabled,
  isDefaultDisabled,
  label,
  name,
  onChange,
  resetToken,
  placeholder,
  dataType,
  consoleDataType,
  arrayItemType,
  initialValue,
  enumValues,
}: ColumnRowProps) => {
  const hasInitialValue = initialValue !== undefined && initialValue !== null;
  const valueId = `${name}-value`;
  const nullId = `${name}-null`;
  const defaultId = `${name}-default`;

  const valueRadioRef = useRef<HTMLInputElement>(null);
  const valueInputRef = useRef<HTMLInputElement>(null);

  // The reset effect below must run only when `resetToken` changes, but it
  // needs the latest `onChange`/`name` (which the parent recreates on every
  // render). Keep them in refs so they stay current without re-triggering the
  // effect. The refs are updated in their own effect (updating a ref during
  // render is not allowed).
  const onChangeRef = useRef(onChange);
  const nameRef = useRef(name);
  useEffect(() => {
    onChangeRef.current = onChange;
    nameRef.current = name;
  });

  const checkValueRadio = () => {
    if (valueRadioRef.current) {
      valueRadioRef.current.checked = true;
    }
  };

  const onCheckValueRadio = () => {
    checkValueRadio();
  };

  const previousResetToken = useRef(resetToken);
  useEffect(() => {
    // This effect also runs on mount (and is replayed in StrictMode).
    // Only an explicit Reset should clear the prefilled row values.
    if (previousResetToken.current === resetToken) return;
    previousResetToken.current = resetToken;

    if (valueInputRef.current && !!valueInputRef.current.value) {
      valueInputRef.current.value = '';
      onChangeRef.current({
        columnName: nameRef.current,
        selectionType: 'default',
        value: '',
      });
    }

    if (valueRadioRef.current?.checked) {
      valueRadioRef.current.checked = false;
    }
  }, [resetToken]);

  const onValueChange = (
    e: React.ChangeEvent<HTMLInputElement> | InputCustomEvent,
  ) => {
    onChange({
      columnName: name,
      selectionType: 'value',
      value: e.target?.value,
    });
  };

  const onRadioChange = (selectionType: string) => {
    if (
      valueInputRef &&
      valueInputRef?.current &&
      (selectionType === 'null' || selectionType === 'default')
    ) {
      valueInputRef.current.value = '';
    }

    const value = consoleDataType === 'boolean' ? false : undefined;

    onChange({
      columnName: name,
      selectionType: selectionType as RowValue['selectionType'],
      value,
    });
  };

  const _isNullDisabled = isDisabled || isNullDisabled;
  const _isDefaultDisabled = isDisabled || isDefaultDisabled;

  return (
    <Flex direction="row" align="center" gap="2">
      <Flex className="w-3/12" align="center" justify="between">
        <Text as="label" htmlFor={valueId} weight="medium">
          {label}
        </Text>
        <Radio
          id={valueId}
          name={name}
          ref={valueRadioRef}
          value="value"
          defaultChecked={hasInitialValue}
          onChange={onRadioChange}
          disabled={isDisabled}
          tabIndex={1}
        />
      </Flex>
      <div className="w-5/12">
        <ColumnRowInput
          consoleDataType={consoleDataType}
          arrayItemType={arrayItemType}
          dataType={dataType}
          name={name}
          onChange={onValueChange}
          onInput={checkValueRadio}
          ref={valueInputRef}
          disabled={isDisabled}
          placeholder={placeholder}
          onValueChange={onValueChange}
          onCheckValueRadio={onCheckValueRadio}
          initialValue={initialValue}
          enumValues={enumValues}
        />
      </div>
      <Flex align="center" gap="3" className="w-4/12">
        <Radio
          id={nullId}
          name={name}
          value="null"
          onChange={onRadioChange}
          disabled={_isNullDisabled}
          tabIndex={3}
        >
          NULL
        </Radio>
        <Radio
          id={defaultId}
          name={name}
          value="default"
          onChange={onRadioChange}
          disabled={_isDefaultDisabled}
          tabIndex={4}
        >
          Default
        </Radio>
        <Code size="1" variant="ghost">
          ({typeof dataType === 'string' ? dataType : dataType.name})
        </Code>
      </Flex>
    </Flex>
  );
};

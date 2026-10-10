import { useEffect, useRef } from 'react';
import { Code, Flex } from '@radix-ui/themes';
import { InputCustomEvent } from './TextInput';
import { ColumnRowInput } from './ColumnRowInput';
import { SupportedDriver } from '@hasura/shared/types';
import {
  columnDataType,
  isDataType,
  TableColumn,
  TableColumnTypeMap,
} from '@hasura/metadata/data-source';
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
  dataType: TableColumn['dataType'];
  onChange: ({ columnName, selectionType, value }: RowValue) => void;
  resetToken: string;
  placeholder: string;
  driver: SupportedDriver;
  supportedDataTypes: TableColumnTypeMap | undefined;
  initialValue?: unknown;
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
  driver,
  supportedDataTypes,
  initialValue,
}: ColumnRowProps) => {
  const hasInitialValue = initialValue !== undefined && initialValue !== null;
  const valueId = `${name}-value`;
  const nullId = `${name}-null`;
  const defaultId = `${name}-default`;

  const valueRadioRef = useRef<HTMLInputElement>(null);
  const valueInputRef = useRef<HTMLInputElement>(null);

  const checkValueRadio = () => {
    if (valueRadioRef.current) {
      valueRadioRef.current.checked = true;
    }
  };

  const onCheckValueRadio = () => {
    checkValueRadio();
  };

  useEffect(() => {
    if (valueInputRef.current && !!valueInputRef.current.value) {
      valueInputRef.current.value = '';
      onChange({
        columnName: name,
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

    const value =
      supportedDataTypes && isDataType(supportedDataTypes, dataType, 'boolean')
        ? false
        : undefined;

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
          dataType={columnDataType(dataType)}
          name={name}
          onChange={onValueChange}
          onInput={checkValueRadio}
          ref={valueInputRef}
          disabled={isDisabled}
          placeholder={placeholder}
          onValueChange={onValueChange}
          onCheckValueRadio={onCheckValueRadio}
          driver={driver}
          initialValue={initialValue}
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

import { GraphQLType, isScalarType } from 'graphql';
import React from 'react';
import { Flex } from '@radix-ui/themes';
import { isJsonString } from '@hasura/shared/utils';
import { ArgValue } from '../../../types';
import { Input } from '@hasura/shared/ui';

type StaticArgValueProps = {
  localArgValue: ArgValue;
  onValueChangeHandler: (value: number | string) => void;
  argType: GraphQLType;
};

const SCALAR_PREFIX = '__SCALAR__';

const StaticArgValue = ({
  argType,
  localArgValue,
  onValueChangeHandler,
}: StaticArgValueProps) => {
  const onChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (
      isScalarType(argType) &&
      (argType?.name === 'Int' || argType?.name === 'Float')
    )
      onValueChangeHandler(`${SCALAR_PREFIX}${e.target.value}`);
    else onValueChangeHandler(e.target.value);
  };

  if (isScalarType(argType) && argType?.name === 'Int') {
    let value: string | boolean | number = localArgValue.value;
    if (typeof value === 'string' && value?.startsWith(SCALAR_PREFIX))
      value = Number.parseInt(value?.substring(10), 10);
    return (
      <Input
        type="number"
        name="argValue"
        id="argValue"
        value={value as number}
        onChange={onChange}
        data-test="select-static-value"
      />
    );
  }

  if (isScalarType(argType) && argType?.name === 'Float') {
    let value: string | boolean | number = localArgValue.value;
    if (typeof value === 'string' && value?.startsWith(SCALAR_PREFIX))
      value = Number.parseFloat(value?.substring(10));
    return (
      <Input
        type="number"
        name="argValue"
        id="argValue"
        value={value as number}
        onChange={onChange}
        data-test="select-static-value"
      />
    );
  }

  if (isScalarType(argType) && argType?.name === 'Boolean') {
    let value: boolean | undefined;
    if (
      typeof localArgValue.value === 'string' &&
      localArgValue.value?.startsWith(SCALAR_PREFIX)
    )
      value = isJsonString(localArgValue.value.substring(10))
        ? JSON.parse(localArgValue.value.substring(10))
        : false;

    return (
      <Flex>
        {[true, false].map((bool) => (
          <p
            key={String(bool)}
            className="flex items-center font-semibold text-muted"
          >
            <input
              id={`radio-select-${bool}`}
              type="radio"
              x-model="relationType"
              className="cursor-pointer rounded-full border shadow-sm "
              onChange={() => onValueChangeHandler(`${SCALAR_PREFIX}${bool}`)}
              checked={value === bool}
              data-test={`radio-select-${bool}`}
            />
            <label
              htmlFor={`radio-select-${bool}`}
              className="cursor-pointer ml-2 mr-4 font-semibold"
            >
              {bool ? 'true' : 'false'}
            </label>
          </p>
        ))}
      </Flex>
    );
  }

  return (
    <Input
      type="text"
      name="argValue"
      id="argValue"
      value={localArgValue.value as string}
      onChange={(e) => onValueChangeHandler(e.target.value)}
      data-test="select-static-value"
    />
  );
};

export default StaticArgValue;

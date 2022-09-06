import React from 'react';
import get from 'lodash.get';

import { utils } from '@/features/DataSource';

import type { OneOf, Property, Ref } from '@/features/DataSource';

import { BasicInput } from './BasicInput';
import { ObjectArray } from './ObjectArray';

const { isRef } = utils;

interface Props {
  name: string;
  property: Property & { type: 'array' };
  otherSchemas: Record<string, Property>;
}

interface ObjectWithProperties {
  type: 'object';
  properties: Record<string, Ref | Property | OneOf>;
}

interface ObjectWithAdditionalProperties {
  type: 'object';
  additionalProperties: true;
  nullable?: boolean;
}

type Items = ObjectWithProperties | ObjectWithAdditionalProperties;

export const isFreeFormArrayItemType = (
  items: Items
): items is {
  type: 'object';
  additionalProperties: true;
} => {
  if (!('additionalProperties' in items)) return false;

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const additionalProperties = items.additionalProperties;

  return true;
};

type GetItemsArgs = {
  property: Property & { type: 'array' };
  otherSchemas: Record<string, Property>;
};

export const getItems = ({ property, otherSchemas }: GetItemsArgs) => {
  return isRef(property.items)
    ? get(otherSchemas, property.items.$ref.split('/').slice(2).join('.'))
    : property.items;
};

export const ArrayInput = ({ name, property, otherSchemas }: Props) => {
  const items = getItems({ property, otherSchemas });

  if (
    items.type === 'string' ||
    items.type === 'number' ||
    (items.type === 'object' && isFreeFormArrayItemType(items))
  )
    return (
      <BasicInput
        name={name}
        label={property.description ?? name}
        type={items.type}
      />
    );

  if (items.type === 'object' && !isFreeFormArrayItemType(items))
    return (
      <ObjectArray
        name={name}
        items={items}
        otherSchemas={otherSchemas}
        label={property.description ?? name}
      />
    );

  return null; // anything outside of the above is not supported
};

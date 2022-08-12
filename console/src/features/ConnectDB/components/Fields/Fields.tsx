import React from 'react';
import get from 'lodash.get';

import { utils } from '@/features/DataSource';
import type { Ref, Property } from '@/features/DataSource';

import { RadioGroup } from '../RadioGroup';
import { RenderProperty } from './RenderProperty';

const { isProperty, isRef, isOneOf } = utils;

interface Props {
  name: string;
  property: Ref | Property | { oneOf: (Property | Ref)[] };
  otherSchemas: Record<string, Property>;
}

export const Field = ({ name, property, otherSchemas }: Props) => {
  if (isProperty(property)) {
    return (
      <RenderProperty
        property={property}
        name={name}
        otherSchemas={otherSchemas}
      />
    );
  }

  if (isRef(property)) {
    const ref = property.$ref;
    const refProperty = get(otherSchemas, ref.split('/').slice(2).join('.'));

    return (
      <Field property={refProperty} otherSchemas={otherSchemas} name={name} />
    );
  }

  if (isOneOf(property)) {
    return (
      <RadioGroup property={property} otherSchemas={otherSchemas} name={name} />
    );
  }

  return null;
};

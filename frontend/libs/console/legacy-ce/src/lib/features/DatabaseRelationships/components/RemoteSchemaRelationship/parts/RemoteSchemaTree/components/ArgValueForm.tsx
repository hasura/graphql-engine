import React, { useState, useEffect } from 'react';
import { FaCircle } from 'react-icons/fa';
import { GraphQLType } from 'graphql';

import { useDebouncedEffect } from '../../../../../../../hooks/useDebounceEffect';
import {
  ArgValue,
  ArgValueKind,
  HasuraRsFields,
  RelationshipFields,
} from '../../../types';
import { defaultArgValue } from '../utils';
import StaticArgValue from './StaticArgValue';

const fieldStyle =
  'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400';
export interface ArgValueFormProps {
  argKey: string;
  relationshipFields: RelationshipFields[];
  setRelationshipFields: React.Dispatch<
    React.SetStateAction<RelationshipFields[]>
  >;
  argValue: ArgValue;
  fields: HasuraRsFields;
  argType: GraphQLType;
}

const argValueTypeOptions = [
  { key: 'field', content: 'Source Field' },
  { key: 'static', content: 'Static Value' },
];

export const ArgValueForm = ({
  argKey,
  relationshipFields,
  setRelationshipFields,
  argValue,
  fields,
  argType,
}: ArgValueFormProps) => {
  const [localArgValue, setLocalArgValue] = useState(argValue);

  useEffect(() => {
    setLocalArgValue(argValue);
  }, [argValue]);

  useDebouncedEffect(
    () => {
      setRelationshipFields(
        relationshipFields.map(f => {
          if (f.key === argKey) {
            return {
              ...f,
              argValue: {
                ...(f.argValue ?? defaultArgValue),
                value: localArgValue.value,
              },
            };
          }
          return f;
        })
      );
    },
    400,
    [localArgValue.value]
  );

  const changeInputType = (e: React.ChangeEvent<HTMLSelectElement>) => {
    setRelationshipFields(
      relationshipFields.map(f => {
        if (f.key === argKey) {
          return {
            ...f,
            argValue: {
              ...(f.argValue ?? defaultArgValue),
              value: '',
              kind: e.target.value as ArgValueKind,
            },
          };
        }
        return f;
      })
    );
  };

  const changeInputColumnValue = (e: React.ChangeEvent<HTMLSelectElement>) => {
    setRelationshipFields(
      relationshipFields.map(f => {
        if (f.key === argKey) {
          return {
            ...f,
            argValue: {
              ...(f.argValue ?? defaultArgValue),
              value: e.target.value,
            },
          };
        }
        return f;
      })
    );
  };

  const onValueChangeHandler = (value: string | number | boolean) => {
    setLocalArgValue({ ...localArgValue, value });
  };

  return (
    <div
      onClick={e => e.stopPropagation()}
      className="rounded bg-white shadow pt-xs pb-sm px-sm my-sm -ml-8 border-l-2 border-yellow-400 w-full"
    >
      <div className="grid grid-cols-2 gap-2">
        <div>
          <label htmlFor="fillFrom text-muted font-semibold">Fill From</label>
          <select
            id="fillFrom"
            className={fieldStyle}
            value={localArgValue.kind}
            onChange={changeInputType}
            data-test="select-argument"
          >
            <option disabled>Select an arugment...</option>
            {argValueTypeOptions.map(option => (
              <option key={option.key} value={option.key}>
                {option.content}
              </option>
            ))}
          </select>
        </div>
        <div>
          {localArgValue.kind === 'field' ? (
            <>
              <label htmlFor="fromField" className="text-muted font-semibold">
                <FaCircle className="text-green-600 mr-2 mb-1" />
                From Field
              </label>
              <select
                className={fieldStyle}
                value={localArgValue.value as string | number}
                onChange={changeInputColumnValue}
                data-test="select-source-field"
                id="fromField"
              >
                <option value="" disabled>
                  Select Field...
                </option>
                {(fields ?? []).map(option => (
                  <option key={option} value={option}>
                    {option}
                  </option>
                ))}
              </select>
            </>
          ) : (
            <>
              <label htmlFor="argValue text-muted font-semibold">
                Static Value
              </label>
              <StaticArgValue
                data-test="select-static-value"
                argType={argType}
                localArgValue={localArgValue}
                onValueChangeHandler={onValueChangeHandler}
              />
            </>
          )}
        </div>
      </div>
    </div>
  );
};

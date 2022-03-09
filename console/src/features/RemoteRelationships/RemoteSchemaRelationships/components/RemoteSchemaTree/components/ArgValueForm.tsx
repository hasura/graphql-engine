import React, { useState, useEffect, useMemo } from 'react';
import { useDebouncedEffect } from '@/hooks/useDebounceEffect';
import {
  ArgValue,
  ArgValueKind,
  HasuraColumn,
  RelationshipFields,
} from '../types';
import { defaultArgValue } from '../utils';

export interface ArgValueFormProps {
  argKey: string;
  relationshipFields: RelationshipFields[];
  setRelationshipFields: React.Dispatch<
    React.SetStateAction<RelationshipFields[]>
  >;
  argValue: ArgValue;
  columns: HasuraColumn;
}

const fieldStyle =
  'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400';

const argValueTypeOptions = [
  { key: 'column', content: 'Source Column' },
  { key: 'static', content: 'Static Value' },
];

export const ArgValueForm = ({
  argKey,
  relationshipFields,
  setRelationshipFields,
  argValue,
  columns,
}: ArgValueFormProps) => {
  const allColumns = useMemo(
    () => [...(columns.columns ?? []), ...(columns.computedFields ?? [])],
    [columns]
  );
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

  const onValueChangeHandler = (value: string) => {
    setLocalArgValue({ ...localArgValue, value });
  };

  return (
    <div
      onClick={e => e.stopPropagation()}
      className="rounded bg-white shadow pt-xs pb-sm px-sm mb-sm border-l-2 border-yellow-400 w-full"
    >
      <div className="grid grid-cols-2 gap-2">
        <div>
          <p className="mb-xs text-muted font-semibold">Fill From</p>
          <select
            className={fieldStyle}
            value={localArgValue.kind}
            onChange={changeInputType}
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
          {localArgValue.kind === 'column' ? (
            <>
              <p className="mb-xs text-muted font-semibold">
                <i className="fa fa-columns text-sm mr-xs" /> From Column
              </p>
              <select
                className={fieldStyle}
                value={localArgValue.value}
                onChange={changeInputColumnValue}
              >
                <option value="" disabled>
                  Select Column...
                </option>
                {allColumns.map(option => (
                  <option key={option} value={option}>
                    {option}
                  </option>
                ))}
              </select>
            </>
          ) : (
            <>
              <p className="mb-xs text-muted font-semibold">Static Value</p>
              <input
                type="text"
                name="argValue"
                id="argValue"
                className={fieldStyle}
                value={localArgValue.value}
                onChange={e => onValueChangeHandler(e.target.value)}
              />
            </>
          )}
        </div>
      </div>
    </div>
  );
};

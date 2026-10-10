import React, { useState, useEffect } from 'react';
import { FaCircle } from 'react-icons/fa';
import { GraphQLType } from 'graphql';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import {
  ArgValue,
  ArgValueKind,
  HasuraRsFields,
  RelationshipFields,
} from '../../../types';
import { defaultArgValue } from '../utils';
import StaticArgValue from './StaticArgValue';
import { useFormContext } from 'react-hook-form';
import { Grid } from '@radix-ui/themes';
import {
  Card,
  createTextOption,
  FieldLabel,
  ReactSelect,
  ReactSelectOptionType,
  Select,
} from '@hasura/shared/ui';
import { RsToRsSchema } from '../../RemoteSchemaToRemoteSchemaForm/schemas';
import { createFilter } from 'react-select';

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

export const ArgValueForm = ({
  argKey,
  relationshipFields,
  setRelationshipFields,
  argValue,
  fields,
  argType,
}: ArgValueFormProps) => {
  const [localArgValue, setLocalArgValue] = useState(argValue);

  const { watch } = useFormContext<RsToRsSchema>();

  const sourceType = watch('rsSourceType');

  const argValueTypeOptions = [
    { key: 'field', content: `Source Type Field (${sourceType})` },
    { key: 'static', content: 'Static Value' },
  ];

  useEffect(() => {
    setLocalArgValue(argValue);
  }, [argValue]);

  useDebouncedEffect(
    () => {
      setRelationshipFields(
        relationshipFields.map((f) => {
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
        }),
      );
    },
    400,
    [localArgValue.value],
  );

  const changeInputType = (value: string) => {
    setRelationshipFields(
      relationshipFields.map((f) => {
        if (f.key === argKey) {
          return {
            ...f,
            argValue: {
              ...(f.argValue ?? defaultArgValue),
              value: '',
              kind: value as ArgValueKind,
            },
          };
        }
        return f;
      }),
    );
  };

  const changeInputColumnValue = (option: ReactSelectOptionType | null) => {
    setRelationshipFields(
      relationshipFields.map((f) => {
        if (f.key === argKey) {
          return {
            ...f,
            argValue: {
              ...(f.argValue ?? defaultArgValue),
              value: option?.value,
            },
          };
        }
        return f;
      }),
    );
  };

  const onValueChangeHandler = (value: string | number | boolean) => {
    setLocalArgValue({ ...localArgValue, value });
  };

  return (
    <Card className="my-2">
      <Grid columns="2" gap="2">
        <div>
          <FieldLabel label="Fill From" className="mb-2" />
          <Select
            value={localArgValue.kind}
            onChange={changeInputType}
            data-test="select-argument"
            placeholder="Select an argument..."
            options={argValueTypeOptions.map((option) => ({
              value: option.key,
              label: option.content,
            }))}
          />
        </div>
        <div onClick={(e) => e.stopPropagation()}>
          {localArgValue.kind === 'field' ? (
            <>
              <FieldLabel
                label="From Source Type Field"
                labelIcon={<FaCircle className="text-green-600" />}
                className="mb-2"
              />
              <ReactSelect
                isSearchable
                options={fields.map(createTextOption)}
                filterOption={createFilter({
                  ignoreCase: true,
                  matchFrom: 'any',
                })}
                value={createTextOption(localArgValue.value as string)}
                onChange={changeInputColumnValue}
                data-test="select-source-field"
                placeholder={`Select Field from ${sourceType}`}
              />
            </>
          ) : (
            <>
              <FieldLabel label="Static Value" className="mb-2" />
              <StaticArgValue
                data-test="select-static-value"
                argType={argType}
                localArgValue={localArgValue}
                onValueChangeHandler={onValueChangeHandler}
              />
            </>
          )}
        </div>
      </Grid>
    </Card>
  );
};

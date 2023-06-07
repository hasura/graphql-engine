import React, { useState, useEffect } from 'react';
import { GraphQLSchema } from 'graphql';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { Button } from '../../../../../new-components/Button';
import { graphQLCustomization as GType } from '../../types';
import TypeMapping from './TypeMapping';
import FieldNames from './FieldNames';
import { useIntrospectionSchemaRemote } from '../../graphqlUtils';
import globals from '../../../../../Globals';
import { Dispatch } from '../../../../../types';
import { checkDefaultGQLScalarType } from '../../Permissions/utils';
import { inputStyles } from '../../constants';

type Props = {
  graphQLCustomization: GType;
  onChange: (updatedGraphQLCustomization: GType) => void;
  remoteSchemaName: string;
  dispatch: Dispatch;
  isDisabled: boolean;
};

type TypeNamesType = {
  prefix?: string | null;
  suffix?: string | null;
  mapping?: { type: string; custom_name: string }[];
};

type FieldNamesType = { parentType?: string } & TypeNamesType;

const convertToMapping = (
  values: { type: string; custom_name: string }[]
): Record<string, string> => {
  const obj: Record<string, string> = {};
  values.forEach(v => {
    obj[v.type] = v.custom_name;
  });
  return obj;
};

const GraphQLCustomizationEdit = ({
  graphQLCustomization,
  remoteSchemaName,
  dispatch,
  onChange,
  isDisabled,
}: Props) => {
  const res = useIntrospectionSchemaRemote(
    remoteSchemaName,
    {
      'x-hasura-admin-secret': globals.adminSecret,
    },
    dispatch
  );
  const schema = res.schema as GraphQLSchema | null;

  const [types, setTypes] = useState<
    {
      typeName: string;
      fields: string[];
    }[]
  >([]);
  const { error } = res;

  useEffect(() => {
    setTypes(
      Object.entries(schema?.getTypeMap() || {})
        .map(([typeName, x]) => ({
          typeName,
          // eslint-disable-next-line no-underscore-dangle
          fields: Object.keys((x as any)._fields || {}),
        }))
        .filter(({ typeName }) => !checkDefaultGQLScalarType(typeName))
    );
  }, [schema]);

  const [openEditor, setOpenEditor] = useState(false);

  const [rootFieldNamespace, setRootFieldNamespace] = useState<
    string | null | undefined
  >(null);
  const [typeNames, setTypesNames] = useState<null | TypeNamesType>(null);
  const [fieldNames, setFieldNames] = useState<null | FieldNamesType[]>(null);
  const [showFieldCustomizationBtn, updateShowFieldCustomizationBtn] =
    useState(true);
  const [tempFieldName, setTempFieldName] = useState<
    FieldNamesType | undefined
  >(undefined);
  useEffect(() => {
    setRootFieldNamespace(graphQLCustomization?.root_fields_namespace);
  }, [graphQLCustomization?.root_fields_namespace]);

  useEffect(() => {
    setTypesNames({
      prefix:
        graphQLCustomization?.type_names?.prefix === ''
          ? null
          : graphQLCustomization?.type_names?.prefix,
      suffix:
        graphQLCustomization?.type_names?.suffix === ''
          ? null
          : graphQLCustomization?.type_names?.suffix,
      mapping: Object.entries(
        graphQLCustomization?.type_names?.mapping || {}
      ).map(([type, custom_name]) => ({
        type,
        custom_name,
      })),
    });
  }, [graphQLCustomization?.type_names]);

  useEffect(() => {
    if (graphQLCustomization?.field_names)
      setFieldNames(
        graphQLCustomization?.field_names.map(fieldName => {
          return {
            parentType: fieldName.parent_type,
            prefix: fieldName?.prefix === '' ? null : fieldName?.prefix,
            suffix: fieldName?.suffix === '' ? null : fieldName?.suffix,
            mapping: Object.entries(fieldName.mapping || {}).map(
              ([type, custom_name]) => ({
                type,
                custom_name,
              })
            ),
          };
        })
      );
  }, [graphQLCustomization?.field_names]);

  if (error) return <div>Something went wrong with schema introspection</div>;
  return (
    <>
      <br />
      {!openEditor ? (
        <div className="mt-md">
          <Button
            size="sm"
            onClick={() => setOpenEditor(true)}
            disabled={isDisabled}
            data-test="remote-schema-customization-editor-expand-btn"
          >
            {!graphQLCustomization ? 'Add' : 'Edit'}
          </Button>
        </div>
      ) : (
        <div
          className="border border-gray-300 p-md mt-xs w-[700px] bg-white"
          data-test="remote-schema-editor"
        >
          <Button size="sm" onClick={() => setOpenEditor(false)}>
            close
          </Button>

          <div className="flex items-center mt-md">
            <label className="w-1/3">
              Root Field Namespace{' '}
              <IconTooltip message="Root field type names will be prefixed by this name." />
            </label>
            <div className="w-2/3">
              <input
                type="text"
                className={inputStyles}
                placeholder="namespace_"
                name="namespace"
                value={rootFieldNamespace || ''}
                data-test="remote-schema-customization-root-field-input"
                onChange={e =>
                  onChange({
                    ...graphQLCustomization,
                    root_fields_namespace: e.target.value || null,
                  })
                }
              />
            </div>
          </div>

          <div className="text-lg font-bold mt-md">Type Names</div>

          <div className="flex items-center mt-md">
            <label className="w-1/3">Prefix</label>
            <div className="w-2/3">
              <input
                type="text"
                className={inputStyles}
                placeholder="prefix_"
                name="prefix"
                value={typeNames?.prefix || ''}
                data-test="remote-schema-customization-type-name-prefix-input"
                onChange={e =>
                  onChange({
                    ...graphQLCustomization,
                    type_names: {
                      ...graphQLCustomization?.type_names,
                      prefix: e.target.value || null,
                    },
                  })
                }
              />
            </div>
          </div>

          <div className="flex items-center mt-md">
            <label className="w-1/3">Suffix</label>
            <div className="w-2/3">
              <input
                type="text"
                className={inputStyles}
                placeholder="_suffix"
                name="suffix"
                value={typeNames?.suffix || ''}
                data-test="remote-schema-customization-type-name-suffix-input"
                onChange={e =>
                  onChange({
                    ...graphQLCustomization,
                    type_names: {
                      ...graphQLCustomization?.type_names,
                      suffix: e.target.value || null,
                    },
                  })
                }
              />
            </div>
          </div>

          <div className="text-lg font-bold mt-md">
            Rename Type Names{' '}
            <IconTooltip
              message="Type remapping takes precedence to prefixes and suffixes."
              side="right"
            />
          </div>

          <TypeMapping
            onChange={updatedMaps =>
              onChange({
                ...graphQLCustomization,
                type_names: {
                  ...graphQLCustomization?.type_names,
                  mapping: convertToMapping(updatedMaps),
                },
              })
            }
            types={types.map(v => v.typeName)}
            typeMappings={typeNames?.mapping ?? []}
            label="type-name"
          />

          {/* Existing Field Names */}
          <div className="text-lg font-bold mt-md">Field Names</div>

          {(fieldNames ?? []).map((fm, i) => (
            <FieldNames
              mode="edit"
              types={types.filter(
                x =>
                  !fieldNames
                    ?.filter(v => v.parentType !== fm.parentType)
                    .map(v => v.parentType)
                    .includes(x.typeName)
              )}
              fieldName={fm}
              onChange={updatedFieldName =>
                onChange({
                  ...graphQLCustomization,
                  field_names: graphQLCustomization.field_names?.map((x, j) => {
                    if (i !== j) return x;
                    return {
                      parent_type: updatedFieldName.parentType,
                      prefix: updatedFieldName.prefix,
                      suffix: updatedFieldName.suffix,
                      mapping: convertToMapping(updatedFieldName.mapping || []),
                    };
                  }),
                })
              }
              onDelete={() => {
                onChange({
                  ...graphQLCustomization,
                  field_names: graphQLCustomization.field_names?.filter(
                    (x, j) => i !== j
                  ),
                });
              }}
              label={`field-type-${i}`}
            />
          ))}

          {/* Adding a new field name */}
          {showFieldCustomizationBtn ? (
            <div className="mt-md">
              <Button
                size="md"
                mode="primary"
                onClick={() => updateShowFieldCustomizationBtn(false)}
                data-test="remote-schema-customization-open-field-mapping"
              >
                Add Field Mapping
              </Button>
            </div>
          ) : (
            <FieldNames
              mode="create"
              types={types.filter(
                x => !fieldNames?.map(v => v.parentType).includes(x.typeName)
              )}
              fieldName={tempFieldName || {}}
              onChange={updatedFieldName => {
                setTempFieldName(updatedFieldName);
              }}
              onClose={() => {
                updateShowFieldCustomizationBtn(true);
                setTempFieldName(undefined);
              }}
              onSave={() => {
                const newObj = {
                  parent_type: tempFieldName?.parentType,
                  prefix: tempFieldName?.prefix,
                  suffix: tempFieldName?.suffix,
                  mapping: convertToMapping(tempFieldName?.mapping || []),
                };
                onChange({
                  ...graphQLCustomization,
                  field_names: [
                    ...(graphQLCustomization?.field_names || []),
                    newObj,
                  ],
                });
                updateShowFieldCustomizationBtn(true);
                setTempFieldName(undefined);
              }}
              label="field-type"
            />
          )}
        </div>
      )}
    </>
  );
};

export default GraphQLCustomizationEdit;

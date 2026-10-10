import React from 'react';
import { Flex } from '@radix-ui/themes';
import {
  isEmpty,
  getLastArrayElement,
  getTableDisplayName,
} from '@hasura/shared/utils';
import { Metadata } from '@hasura/shared/types';
import { defaultRelFieldMapping } from '../../Form/state';
import { FlattenCustomType } from '../../../../../shared/utils/hasuraCustomTypeUtils';
import {
  CustomTypeObjectRelationshipFormState,
  CustomTypeRelationshipFieldMapping,
} from '../../../types';
import {
  getDatabaseMethods,
  useTableColumnInfos,
} from '@hasura/metadata/data-source';
import {
  createTextOption,
  FieldLabel,
  getDialogPortalTarget,
  Input,
  ReactSelect,
  Select,
  Text,
} from '@hasura/shared/ui';
import { createFilter } from 'react-select';

type Props = {
  outputType: FlattenCustomType;
  isDisabled: boolean;
  relConfig: CustomTypeObjectRelationshipFormState;
  setRelConfig: React.Dispatch<
    React.SetStateAction<CustomTypeObjectRelationshipFormState>
  >;
  metadata: Metadata['metadata'];
};

const relationshipTypeOptions = [
  {
    label: 'Object Relationship',
    value: 'object',
  },
  {
    label: 'Array Relationship',
    value: 'array',
  },
];

const filterOption = createFilter({
  ignoreCase: true,
  matchFrom: 'any',
});

const TypeRelationshipEditor = ({
  outputType,
  isDisabled,
  relConfig,
  setRelConfig,
  metadata,
}: Props) => {
  const { data: columns, isFetching: isColumnsFetching } = useTableColumnInfos({
    source: relConfig.source
      ? metadata.sources.find((s) => s.name === relConfig.source)
      : undefined,
    table: relConfig.remote_table,
  });

  const dialogTarget = getDialogPortalTarget();
  const dataSources = metadata.sources.filter((s) =>
    getDatabaseMethods(s.kind).check.isFeatureSupported(
      'actions.relationships',
    ),
  );

  // relname on change
  const setRelName = (e) => {
    const relName = e.target.value;
    setRelConfig((rc) => ({
      ...rc,
      name: relName,
    }));
  };

  // reltype on change
  const setRelType = (relType: string) => {
    if (relType !== 'array' && relType !== 'object') {
      return;
    }

    setRelConfig((rc) => ({
      ...rc,
      type: relType,
    }));
  };

  const setDatabase = (value: string) => {
    setRelConfig((rc) => ({
      ...rc,
      source: value,
    }));
  };

  // field mappings on change
  const setFieldMappings = (f: CustomTypeRelationshipFieldMapping[]) => {
    const lastFieldMapping = getLastArrayElement(f);
    if (!isEmpty(f) && lastFieldMapping) {
      if (!!lastFieldMapping.column && !!lastFieldMapping.field) {
        f = [...f, defaultRelFieldMapping];
      }
    }
    setRelConfig((rc) => ({
      ...rc,
      field_mapping: f,
    }));
  };

  // field mapping array builder
  const relFieldMappings = () => {
    return (
      <Flex direction="column" className="mb-5">
        <Flex direction="row" className="mb-2.5">
          <div className="mr-5 w-1/3">
            <Text weight="bold">From:</Text>
          </div>
          <div className="w-1/3mr-5">
            <Text weight="bold">To:</Text>
          </div>
        </Flex>
        {relConfig.field_mapping.map((fieldMap, i) => {
          const setColumn = (selectedCol: string) => {
            const newFM = relConfig.field_mapping.map((fm, fi) => {
              if (fi === i) {
                return {
                  ...fieldMap,
                  column: selectedCol,
                };
              }

              return fm;
            });

            setFieldMappings(newFM);
          };

          const setField = (selectedField: string) => {
            const newFM = relConfig.field_mapping.map((fm, fi) => {
              if (fi === i) {
                return {
                  ...fieldMap,
                  field: selectedField,
                };
              }

              return fm;
            });

            setFieldMappings(newFM);
          };

          const field = fieldMap.field;
          const refColumn = fieldMap.column;

          const removeField = () => {
            setFieldMappings([
              ...relConfig.field_mapping.slice(0, i),
              ...relConfig.field_mapping.slice(i + 1),
            ]);
          };

          let removeIcon;
          if (i + 1 === relConfig.field_mapping.length) {
            removeIcon = null;
          } else {
            removeIcon = (
              <i
                className="w-2.5 cursor-pointer fa-lg fa fa-times"
                onClick={removeField}
              />
            );
          }

          const fields =
            outputType.kind === 'objects'
              ? outputType.definition.fields.map((f) => f.name)
              : [];

          return (
            <Flex direction="row" className="mb-2.5" key={`fk-col-${i}`}>
              <div className="w-1/3 mr-5">
                <ReactSelect
                  value={field ? createTextOption(field) : undefined}
                  onChange={(option) => setField(option?.value ?? '')}
                  isSearchable
                  filterOption={filterOption}
                  data-test={`manual-relationship-lcol-${i}`}
                  isDisabled={!relConfig.remote_table}
                  placeholder="-- field --"
                  menuPortalTarget={dialogTarget}
                  menuPlacement="top"
                  menuPosition="fixed"
                  options={fields.map((f) => ({
                    label: f,
                    value: f,
                  }))}
                />
              </div>
              <div className="w-1/3">
                <ReactSelect
                  value={refColumn ? createTextOption(refColumn) : undefined}
                  onChange={(option) => setColumn(option?.value ?? '')}
                  isDisabled={!relConfig.remote_table || isColumnsFetching}
                  isSearchable
                  filterOption={filterOption}
                  menuPortalTarget={dialogTarget}
                  data-test={`manual-relationship-rcol-${i}`}
                  placeholder="-- ref_column --"
                  menuPlacement="top"
                  menuPosition="fixed"
                  options={
                    columns?.map((col) => createTextOption(col.name)) ?? []
                  }
                />
              </div>
              <div className="self-center">{removeIcon}</div>
            </Flex>
          );
        })}
      </Flex>
    );
  };

  return (
    <Flex direction="column" gap="4">
      <div>
        <FieldLabel className={`mb-2.5`} label="Relationship Type:" />
        <Select
          value={relConfig.type}
          data-test={'manual-relationship-type'}
          onChange={setRelType}
          placeholder="-- relationship type --"
          options={relationshipTypeOptions}
        />
      </div>
      <div>
        <FieldLabel className={`mb-2.5`} label="Relationship Name:" />
        <Input
          onChange={setRelName}
          type="text"
          placeholder="Enter relationship name"
          data-test="rel-name"
          title={
            isDisabled
              ? 'A relationship cannot be renamed. Please drop and re-create if you really must.'
              : undefined
          }
          value={relConfig.name}
        />
      </div>
      <div>
        <FieldLabel className={`mb-2.5`} label="Database:" />
        <Select
          data-test={'manual-relationship-db-choice'}
          placeholder="-- data source --"
          onChange={setDatabase}
          disabled={!relConfig.name || isDisabled}
          value={relConfig.source}
          options={dataSources.map((s) => ({
            value: s.name,
            label: `${s.name} (${s.kind})`,
          }))}
        />
      </div>
      <div>
        <FieldLabel className={`mb-2.5`} label="Reference Table:" />
        <ReactSelect
          value={
            relConfig.remote_table
              ? {
                  label: getTableDisplayName(relConfig.remote_table),
                  value: relConfig.remote_table,
                }
              : undefined
          }
          isSearchable
          filterOption={(option, inputValue) =>
            option.label.toLowerCase().includes(inputValue.toLowerCase())
          }
          menuPosition="fixed"
          menuPortalTarget={dialogTarget}
          data-test={'manual-relationship-ref-table'}
          onChange={(option) => {
            setRelConfig((rc) => ({
              ...rc,
              remote_table: option?.value,
              field_mapping: [defaultRelFieldMapping],
            }));
          }}
          isDisabled={!relConfig.remote_table}
          placeholder="-- reference table --"
          options={dataSources.map((source) => {
            return {
              label: source.name,
              options: source.tables.map((t) => ({
                label: getTableDisplayName(t.table),
                value: t.table,
              })),
            };
          })}
        />
      </div>
      {relFieldMappings()}
    </Flex>
  );
};

export default TypeRelationshipEditor;

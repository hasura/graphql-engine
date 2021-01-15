import { defaultRelFieldMapping } from '../Common/stateDefaults';
import { generateTableDef } from '../../../../dataSources';

const reformRelationship = relConfig => {
  return {
    source: relConfig.source || relConfig.refDb,
    name: relConfig.name,
    type: relConfig.type,
    remote_table: generateTableDef(relConfig.refTable, relConfig.refSchema),
    field_mapping: relConfig.fieldMapping.reduce((all, fm) => {
      if (fm.column && fm.field) {
        return {
          ...all,
          [fm.field]: fm.column,
        };
      }
      return all;
    }, {}),
  };
};

export const parseCustomTypeRelationship = relConfig => {
  const localRelConfig = {
    ...relConfig,
    refSchema: relConfig.remote_table.schema,
    refTable: relConfig.remote_table.name,
    refDb: relConfig.source,
    fieldMapping: Object.keys(relConfig.field_mapping).map(field => {
      return {
        field,
        column: relConfig.field_mapping[field],
      };
    }),
  };
  localRelConfig.fieldMapping.push(defaultRelFieldMapping);
  return localRelConfig;
};

export const injectTypeRelationship = (types, typename, relConfig) => {
  return types.map(t => {
    if (t.name === typename && t.kind === 'object') {
      return {
        ...t,
        relationships: [
          ...(t.relationships || []).filter(r => r.name !== relConfig.name),
          reformRelationship(relConfig),
        ],
      };
    }
    return t;
  });
};

export const getRelValidationError = relConfig => {
  if (!relConfig.name) return 'relationship name is mandatory';
  if (!relConfig.type) {
    return 'relationship type is mandatory; choose "array" or "object"';
  }
  if (!relConfig.refSchema) return 'please select a reference schema';
  if (!relConfig.refTable) return 'please select a reference table';
  if (!relConfig.refDb) return 'please select a reference data source';
  if (relConfig.fieldMapping.length < 2) {
    return 'please choose the mapping between table column(s) and type field(s)';
  }
  return null;
};

export const getRelDef = relMeta => {
  const lcol =
    Object.keys(relMeta.field_mapping).length > 1
      ? '( ' + Object.keys(relMeta.field_mapping).join(', ') + ' )'
      : Object.keys(relMeta.field_mapping)[0];
  const rcol =
    Object.values(relMeta.field_mapping).length > 1
      ? '( ' + Object.values(relMeta.field_mapping).join(', ') + ' )'
      : Object.values(relMeta.field_mapping)[0];

  const tableLabel = relMeta.remote_table.schema
    ? `${relMeta.remote_table.schema}.${relMeta.remote_table.name}`
    : relMeta.remote_table;

  const sourceDef = relMeta.source ? `${relMeta.source} . ` : '';

  return `${relMeta.typename} . ${lcol} → ${sourceDef}${tableLabel} . ${rcol}`;
};

export const removeTypeRelationship = (types, typename, relName) => {
  return types.map(t => {
    if (t.name === typename && t.kind === 'object') {
      const newRel = (t.relationships || []).filter(r => r.name !== relName);
      return {
        ...t,
        relationships: newRel.length ? newRel : undefined,
      };
    }
    return t;
  });
};

export const validateRelTypename = (types, typename, relname) => {
  for (let i = types.length - 1; i >= 0; i--) {
    const type = types[i];
    if (type.kind === 'object' && type.name === typename) {
      if ((type.relationships || []).some(r => r.name === relname)) {
        return `Relationship with name "${relname}" already exists.`;
      }
    }
  }
  return null;
};

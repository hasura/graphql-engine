import { addSource } from './../../../../metadata/sourcesUtils';
import { isObject, isEqual } from './../../../Common/utils/jsUtils';
import { Table } from '../../../../dataSources/types';
import { MetadataDataSource } from '../../../../metadata/types';

export const getErrorMessageFromMissingFields = (
  host: string,
  port: string,
  username: string,
  database: string
) => {
  const missingFields = [];
  if (!host) {
    missingFields.push('host');
  }
  if (!port) {
    missingFields.push('port');
  }
  if (!username) {
    missingFields.push('username');
  }
  if (!database) {
    missingFields.push('database');
  }

  return `The following fields are required: ${missingFields
    .slice(0, missingFields.length - 1)
    .join(', ')} and ${missingFields[missingFields.length - 1]}`;
};

export const getDatasourceURL = (
  link: string | { from_env: string } | undefined
) => {
  if (!link) {
    return '';
  }
  if (typeof link === 'string') {
    return link.toString();
  }
  return link.from_env.toString();
};

export function parsePgUrl(
  url: string
): Partial<Omit<URL, 'searchParams' | 'toJSON'>> {
  try {
    const protocol = new URL(url).protocol;
    const newUrl = url.replace(protocol, 'http://');
    const parsed = new URL(newUrl);
    return {
      origin: parsed.origin.replace('http:', protocol),
      hash: parsed.hash,
      host: parsed.host,
      hostname: parsed.hostname,
      port: parsed.port,
      href: parsed.href.replace('http:', protocol),
      password: parsed.password,
      pathname: parsed.pathname,
      search: parsed.search,
      username: parsed.username,
      protocol,
    };
  } catch (error) {
    return {};
  }
}

type TableType = Record<string, { table_type: Table['table_type'] }>;
type SchemaType = Record<string, TableType>;
type SourceSchemasType = Record<string, SchemaType>;

export const canReUseTableTypes = (
  allSources: SourceSchemasType,
  sources: MetadataDataSource[]
) => {
  if (
    !sources ||
    !allSources ||
    Object.keys(allSources).length !== sources.length
  )
    return false;

  // make sure all table names and schema names are same in metadata and table_type cache (allSourcesSchemas)
  return sources.every(sourceFromMetada =>
    sourceFromMetada?.tables?.every(
      ({ table: { name, schema } = {} }) =>
        name &&
        schema &&
        allSources[sourceFromMetada.name] &&
        allSources[sourceFromMetada.name][schema] &&
        allSources[sourceFromMetada.name][schema][name]
    )
  );
};

export type AddSourceArg = ReturnType<typeof addSource>['args'];

export const dataSourceIsEqual = (
  sourceFromMetaData: MetadataDataSource,
  data: AddSourceArg
) => {
  const ignoreFields = ['tables', 'kind', 'name', 'replace_configuration'];

  const filterFields = (obj: Record<string, any>) =>
    Object.entries(obj).reduce((acc: Record<string, any>, [key, value]) => {
      if (value !== null && !ignoreFields.includes(key)) {
        if (isObject(value)) {
          acc[key] = filterFields(value);
        } else {
          acc[key] = value;
        }
      }
      return acc;
    }, {});

  return isEqual(filterFields(sourceFromMetaData), filterFields(data));
};

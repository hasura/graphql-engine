import { FetchJson } from '@hasura/shared/utils';
import {
  BulkMetadataQueryType,
  SingleMetadataTypes,
} from '@hasura/shared/types';

export type TMigrationSingleQuery<
  ArgsType extends Record<string, any> = Record<string, any>,
> = {
  type: SingleMetadataTypes;
  args: ArgsType;
  resource_version?: number;
};

export type TMigrationBulkQuery = {
  type: BulkMetadataQueryType;
  args: TMigrationSingleQuery[];
  resource_version?: number;
};

export type TMigrationQuery<
  ArgsType extends Record<string, any> = Record<string, any>,
> = TMigrationBulkQuery | TMigrationSingleQuery<ArgsType>;

export const runMetadataQuery = async <ResponseType>({
  url,
  fetchJson,
  body,
}: {
  url: string;
  body: TMigrationQuery;
  fetchJson: FetchJson;
}): Promise<ResponseType> => {
  return await fetchJson(url, {
    method: 'POST',
    body: JSON.stringify(body),
  });
};

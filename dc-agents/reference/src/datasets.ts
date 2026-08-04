import { DatasetDeleteCloneResponse, DatasetGetTemplateResponse, DatasetCreateCloneRequest, DatasetCreateCloneResponse, } from '@hasura/dc-api-types';
import { loadStaticData, StaticData, staticDataExists } from './data';

export async function getDataset(name: string): Promise<DatasetGetTemplateResponse> {
  return {
    exists: await staticDataExists(name)
  };
}

export async function cloneDataset(store: Record<string, StaticData>, dbName: string, body: DatasetCreateCloneRequest): Promise<DatasetCreateCloneResponse> {
  const storeName = getDbStoreName(dbName);
  const staticData = await loadStaticData(body.from);
  store[storeName] = staticData;
  return { config: { db: dbName } };
}

export async function deleteDataset(store: Record<string, StaticData>, dbName: string): Promise<DatasetDeleteCloneResponse> {
  const storeName = getDbStoreName(dbName);
  const exists = store[storeName];
  if(exists) {
    delete store[storeName];
    return {message: "success"};
  } else {
    throw(Error("Dataset does not exist."));
  }
}

// Prefix '$' to disambiguate from default datasets
export const getDbStoreName = (dbName: string) => `$${dbName}`

export const defaultDbStoreName = "@default";

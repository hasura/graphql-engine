import { DatasetDeleteCloneResponse, DatasetGetTemplateResponse, DatasetCreateCloneRequest, DatasetCreateCloneResponse, } from '@hasura/dc-api-types';
import { loadStaticData, StaticData, staticDataExists } from './data';

export async function getDataset(name: string): Promise<DatasetGetTemplateResponse> {
  const safePath = mkPath(name);
  return {
    exists: await staticDataExists(safePath)
  };
}

export async function cloneDataset(store: Record<string, StaticData>, dbName: string, body: DatasetCreateCloneRequest): Promise<DatasetCreateCloneResponse> {
  const storeName = getDbStoreName(dbName);
  const safePathName = mkPath(body.from);
  const data = await loadStaticData(safePathName);
  store[storeName] = data;
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

function mkPath(name: string): string {
  const base = name.replace(/\//g,''); // TODO: Can this be made safer?
  return `${base}.xml.gz`;
}

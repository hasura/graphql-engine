import { DatasetDeleteResponse, DatasetGetResponse, DatasetPostRequest, DatasetPostResponse, } from '@hasura/dc-api-types';
import { loadStaticData, StaticData } from './data';

export async function getDataset(name: string): Promise<DatasetGetResponse> {
  const safePath = mkPath(name);
  const data = await loadStaticData(safePath); // TODO: Could make this more efficient, but this works for now!
  if(data) {
    return { exists: true };
  } else {
    return { exists: false };
  }
}

export async function cloneDataset(store: Record<string, StaticData>, name: string, body: DatasetPostRequest): Promise<DatasetPostResponse> {
  const safePathName = mkPath(body.from);
  const data = await loadStaticData(safePathName);
  store[`$${name}`] = data;
  return { config: { db: `$${name}` } };
}

export async function deleteDataset(store: Record<string, StaticData>, name: string): Promise<DatasetDeleteResponse> {
  const exists = store[`$${name}`];
  if(exists) {
    delete store[`$${name}`];
    return {message: "success"};
  } else {
    throw(Error("Dataset does not exist."));
  }
}

function mkPath(name: string): string {
  const base = name.replace(/\//g,''); // TODO: Can this be made safer?
  return `${base}.xml.gz`;
}

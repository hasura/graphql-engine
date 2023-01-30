import { connect, SqlLogger } from './db';
import { DatasetDeleteCloneResponse, DatasetGetTemplateResponse, DatasetCreateCloneRequest, DatasetCreateCloneResponse, } from '@hasura/dc-api-types';
import { promises, existsSync } from 'fs';
import { DATASET_CLONES, DATASET_DELETE, DATASET_TEMPLATES } from "./environment";
import path from 'path';

export async function getDataset(template_name: string): Promise<DatasetGetTemplateResponse> {
  const path = mkTemplatePath(template_name);
  if(existsSync(path)) {
    const stats = await promises.stat(path);
    if(stats.isFile()) {
      return { exists: true };
    } else {
      return { exists: false };
    }
  } else {
    return { exists: false };
  }
}

export async function cloneDataset(logger: SqlLogger, clone_name: string, body: DatasetCreateCloneRequest): Promise<DatasetCreateCloneResponse> {
  const fromPath = mkTemplatePath(body.from);
  const toPath = mkClonePath(clone_name);
  const fromStats = await promises.stat(fromPath);
  const exists = existsSync(toPath);
  if(fromStats.isFile() && ! exists) {
    // Check if this is a real SQLite DB
    const db = connect({ db: fromPath, explicit_main_schema: false, tables: [], meta: false }, logger);
    if(db) {
      db.close();
    } else {
      throw(Error("Dataset is not an SQLite Database!"))
    }
    await promises.cp(fromPath, toPath);
    return { config: { db: toPath } };
  } else if(exists) {
    throw(Error("Dataset already exists!"))
  } else {
    throw(Error("Can't Clone!"))
  }
}

export async function deleteDataset(clone_name: string): Promise<DatasetDeleteCloneResponse> {
  if(DATASET_DELETE) {
    const path = mkClonePath(clone_name);
    const exists = existsSync(path);
    if(exists) {
      const stats = await promises.stat(path);
      if(stats.isFile()) {
        await promises.rm(path);
        return { message: "success" };
      } else {
        throw(Error("Dataset is not a file."));
      }
    } else {
      throw(Error("Dataset does not exist."));
    }
  } else {
    throw(Error("Dataset deletion not available."));
  }
}

function mkTemplatePath(name: string): string {
  const parsed = path.parse(name);
  const safeName = parsed.base;
  if(name != safeName) {
    throw(Error(`Template name ${name} is not valid.`));
  }
  return path.join(DATASET_TEMPLATES, safeName);
}

function mkClonePath(name: string): string {
  const parsed = path.parse(name);
  const safeName = parsed.base;
  if(name != safeName) {
    throw(Error(`Template name ${name} is not valid.`));
  }
  return path.join(DATASET_CLONES, safeName);
}

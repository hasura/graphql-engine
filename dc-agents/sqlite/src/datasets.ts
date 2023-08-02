import { createDbMode, defaultMode, SqlLogger, withConnection } from './db';
import { DatasetDeleteCloneResponse, DatasetGetTemplateResponse, DatasetCreateCloneRequest, DatasetCreateCloneResponse, } from '@hasura/dc-api-types';
import { access, constants, promises, existsSync } from 'fs';
import { DATASET_CLONES, DATASET_DELETE, DATASET_TEMPLATES } from "./environment";
import path from 'path';

export async function getDataset(template_name: string): Promise<DatasetGetTemplateResponse> {
  const templatePaths = mkTemplatePaths(template_name);
  return {
    exists: await fileIsReadable(templatePaths.dbFileTemplatePath) || await fileIsReadable(templatePaths.sqlFileTemplatePath)
  };
}

export async function cloneDataset(logger: SqlLogger, clone_name: string, body: DatasetCreateCloneRequest): Promise<DatasetCreateCloneResponse> {
  const templatePaths = mkTemplatePaths(body.from);
  const toPath = mkClonePath(clone_name);
  const cloneExistsAlready = await fileIsReadable(toPath);
  if (cloneExistsAlready) {
    throw new Error("Dataset clone already exists");
  }

  if (await fileIsReadable(templatePaths.dbFileTemplatePath)) {
    try {
      await withConnection({ db: templatePaths.dbFileTemplatePath, explicit_main_schema: false, tables: [], meta: false }, defaultMode, logger, async db => {});
    }
    catch {
      throw new Error("Dataset template is not a valid SQLite database!");
    }

    await promises.cp(templatePaths.dbFileTemplatePath, toPath);
    return { config: { db: toPath } };

  } else if (await fileIsReadable(templatePaths.sqlFileTemplatePath)) {
    const sql = await promises.readFile(templatePaths.sqlFileTemplatePath, { encoding: "utf-8" });
    await withConnection({db: toPath, explicit_main_schema: false, tables: [], meta: false}, createDbMode, logger, async db => {
      await db.withTransaction(async () => {
        await db.exec(sql);
      });
    });
    return { config: { db: toPath } };
  } else {
    throw new Error("Dataset template does not exist");
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

type TemplatePaths = {
  dbFileTemplatePath: string,
  sqlFileTemplatePath: string,
}

function mkTemplatePaths(name: string): TemplatePaths {
  const parsed = path.parse(name);
  const safeName = parsed.base;
  if(name != safeName) {
    throw(Error(`Template name ${name} is not valid.`));
  }
  return {
    dbFileTemplatePath: path.join(DATASET_TEMPLATES, safeName + ".sqlite"),
    sqlFileTemplatePath: path.join(DATASET_TEMPLATES, safeName + ".sql"),
  };
}

function mkClonePath(name: string): string {
  const parsed = path.parse(name);
  const safeName = parsed.base;
  if(name != safeName) {
    throw(Error(`Template name ${name} is not valid.`));
  }
  return path.join(DATASET_CLONES, safeName + ".sqlite");
}

export const fileIsReadable = async(filepath: string): Promise<boolean> => {
  return new Promise((resolve) => {
    access(filepath, constants.R_OK, err => err ? resolve(false) : resolve(true));
  });
}

import { Config } from "./config";
import { Sequelize } from 'sequelize';
import { DB_ALLOW_LIST, DB_CREATE, DB_PRIVATECACHE, DB_READONLY } from "./environment";

import SQLite from 'sqlite3';

export type SqlLogger = (sql: string) => void

export function connect(config: Config, sqlLogger: SqlLogger): Sequelize {
  if(DB_ALLOW_LIST != null) {
    if(DB_ALLOW_LIST.includes(config.db)) {
      throw new Error(`Database ${config.db} is not present in DB_ALLOW_LIST 😭`);
    }
  }

  // See https://github.com/TryGhost/node-sqlite3/wiki/API#new-sqlite3databasefilename--mode--callback
  // mode (optional): One or more of
  //   * OPEN_READONLY
  //   * OPEN_READWRITE
  //   * OPEN_CREATE
  //   * OPEN_FULLMUTEX
  //   * OPEN_URI
  //   * OPEN_SHAREDCACHE
  //   * OPEN_PRIVATECACHE
  // The default value is OPEN_READWRITE | OPEN_CREATE | OPEN_FULLMUTEX.
  const readMode   = DB_READONLY     ? SQLite.OPEN_READONLY     : SQLite.OPEN_READWRITE;
  const createMode = DB_CREATE       ? SQLite.OPEN_CREATE       : 0; // Flag style means 0=off
  const cacheMode  = DB_PRIVATECACHE ? SQLite.OPEN_PRIVATECACHE : SQLite.OPEN_SHAREDCACHE;
  const mode       = readMode | createMode | cacheMode;

  const db = new Sequelize({
    dialect: 'sqlite',
    storage: config.db,
    dialectOptions: { mode: mode },
    logging: sqlLogger
  });

  return db;
};

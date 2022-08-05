import { Config } from "./config";
import { Sequelize } from 'sequelize';
import { env } from "process";
import { stringToBool } from "./util";
import SQLite from 'sqlite3';

export function connect(config: Config): Sequelize {
  if(env.DB_ALLOW_LIST != null) {
    if(!env.DB_ALLOW_LIST.split(',').includes(config.db)) {
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
  const readMode   = stringToBool(process.env['DB_READONLY'])     ? SQLite.OPEN_READONLY     : SQLite.OPEN_READWRITE;
  const createMode = stringToBool(process.env['DB_CREATE'])       ? SQLite.OPEN_CREATE       : 0; // Flag style means 0=off
  const cacheMode  = stringToBool(process.env['DB_PRIVATECACHE']) ? SQLite.OPEN_PRIVATECACHE : SQLite.OPEN_SHAREDCACHE;
  const mode       = readMode | createMode | cacheMode;

  const db = new Sequelize({
    dialect: 'sqlite',
    storage: config.db,
    dialectOptions: { mode: mode }
  });

  return db;
};

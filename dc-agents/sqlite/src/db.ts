import { Config } from "./config";
import { Sequelize } from 'sequelize';
import { DB_ALLOW_LIST, DB_CREATE, DB_PRIVATECACHE, DB_READONLY } from "./environment";
import SQLite from 'sqlite3';

export type SqlLogger = (sql: string) => void

// See https://github.com/TryGhost/node-sqlite3/wiki/API#new-sqlite3databasefilename--mode--callback
// mode (optional): One or more of OPEN_READONLY | OPEN_READWRITE | OPEN_CREATE | OPEN_FULLMUTEX | OPEN_URI | OPEN_SHAREDCACHE | OPEN_PRIVATECACHE
// The default value is OPEN_READWRITE | OPEN_CREATE | OPEN_FULLMUTEX.
const readMode   = DB_READONLY     ? SQLite.OPEN_READONLY     : SQLite.OPEN_READWRITE;
const createMode = DB_CREATE       ? SQLite.OPEN_CREATE       : 0; // Flag style means 0=off
const cacheMode  = DB_PRIVATECACHE ? SQLite.OPEN_PRIVATECACHE : SQLite.OPEN_SHAREDCACHE;
const mode       = readMode | createMode | cacheMode;

export function connect(config: Config, sqlLogger: SqlLogger): Sequelize {
  if(DB_ALLOW_LIST != null) {
    if(DB_ALLOW_LIST.includes(config.db)) {
      throw new Error(`Database ${config.db} is not present in DB_ALLOW_LIST 😭`);
    }
  }

  const db = new Sequelize({
    dialect: 'sqlite',
    storage: config.db,
    dialectOptions: { mode: mode },
    logging: sqlLogger
  });

  return db;
};

export type Connected = {
  query: ((query: string, params?: Record<string, unknown>) => Promise<Array<any>>),
  close: (() => Promise<boolean>)
}

/**
 * @param config: Config
 * @param sqlLogger: SqlLogger
 * @returns {query, mutation}
 * 
 * Query and mutation support implemented directly on the SQLite3 library.
 * See: https://github.com/TryGhost/node-sqlite3/wiki/API
 */
export function connect2(config: Config, sqlLogger: SqlLogger): Connected {
  if(DB_ALLOW_LIST != null) {
    if(DB_ALLOW_LIST.includes(config.db)) {
      throw new Error(`Database ${config.db} is not present in DB_ALLOW_LIST 😭`);
    }
  }

  const db_ = new SQLite.Database(config.db, mode);
  
  // NOTE: Avoiding util.promisify as this seems to be causing connection failures.
  const dbQueryPromise = (query: string, params?: Record<string, unknown>): Promise<Array<any>> => {
    return new Promise((resolve, reject) => {
      /* Pass named params:
       * db.run("UPDATE tbl SET name = $name WHERE id = $id", {
       *   $id: 2,
       *   $name: "bar"
       * });
       */
      db_.all(query, params || {}, (err, data) => {
        if (err) {
          return reject(err);
        } else {
          resolve(data);
        }
      })
    })
  }

  const dbClosePromise = (): Promise<boolean> => {
    return new Promise((resolve, reject) => {
      db_.close((err) => {
        if (err) {
          return reject(err);
        } else {
          resolve(true); // What should we resolve with if there's no data to promise?
        }
      })
    })
  }

  return {
    query: dbQueryPromise,
    close: dbClosePromise,
  };
}

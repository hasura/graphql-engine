import { Config } from "./config";
import { DB_ALLOW_LIST, DB_CREATE, DB_PRIVATECACHE, DB_READONLY } from "./environment";
import SQLite from 'sqlite3';

export type SqlLogger = (sql: string) => void

// See https://github.com/TryGhost/node-sqlite3/wiki/API#new-sqlite3databasefilename--mode--callback
// mode (optional): One or more of OPEN_READONLY | OPEN_READWRITE | OPEN_CREATE | OPEN_FULLMUTEX | OPEN_URI | OPEN_SHAREDCACHE | OPEN_PRIVATECACHE
// The default value is OPEN_READWRITE | OPEN_CREATE | OPEN_FULLMUTEX.
const readMode   = DB_READONLY     ? SQLite.OPEN_READONLY     : SQLite.OPEN_READWRITE;
const createMode = DB_CREATE       ? SQLite.OPEN_CREATE       : 0; // Flag style means 0=off
const cacheMode  = DB_PRIVATECACHE ? SQLite.OPEN_PRIVATECACHE : SQLite.OPEN_SHAREDCACHE;
export const defaultMode = readMode | createMode | cacheMode;
export const createDbMode = SQLite.OPEN_CREATE | readMode | cacheMode;

export type Connection = {
  query: (query: string, params?: Record<string, unknown>) => Promise<Array<any>>,
  exec: (sql: string) => Promise<void>;
  withTransaction: <Result>(action: () => Promise<Result>) => Promise<Result>
}

export async function withConnection<Result>(config: Config, mode: number, sqlLogger: SqlLogger, useConnection: (connection: Connection) => Promise<Result>): Promise<Result> {
  if(DB_ALLOW_LIST != null) {
    if(DB_ALLOW_LIST.includes(config.db)) {
      throw new Error(`Database ${config.db} is not present in DB_ALLOW_LIST 😭`);
    }
  }

  const db_ = await new Promise<SQLite.Database>((resolve, reject) => {
    const db = new SQLite.Database(config.db, mode, err => {
      if (err) {
        reject(err);
      } else {
        resolve(db);
      }
    });
  });

  // NOTE: Avoiding util.promisify as this seems to be causing connection failures.
  const query = (query: string, params?: Record<string, unknown>): Promise<Array<any>> => {
    return new Promise((resolve, reject) => {
      /* Pass named params:
       * db.run("UPDATE tbl SET name = $name WHERE id = $id", {
       *   $id: 2,
       *   $name: "bar"
       * });
       */
      sqlLogger(query);
      db_.all(query, params || {}, (err, data) => {
        if (err) {
          return reject(err);
        } else {
          resolve(data);
        }
      })
    })
  }

  const exec = (sql: string): Promise<void> => {
    return new Promise((resolve, reject) => {
      sqlLogger(sql);
      db_.exec(sql, err => {
        if (err) {
          reject(err);
        } else {
          resolve();
        }
      })
    })
  };

  const withTransaction = async <Result>(action: () => Promise<Result>): Promise<Result> => {
    await exec("BEGIN TRANSACTION");
    try {
      const result = await action();
      await exec("COMMIT");
      return result;
    } catch (err) {
      await exec("ROLLBACK")
      throw err;
    }
  }

  try {
    return await useConnection({ query, exec, withTransaction });
  }
  finally {
    await new Promise((resolve, reject) => {
      db_.close((err) => {
        if (err) {
          return reject(err);
        } else {
          resolve(true); // What should we resolve with if there's no data to promise?
        }
      })
    });
  }
}

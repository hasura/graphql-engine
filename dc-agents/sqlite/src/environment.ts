
export function stringToBool(x: string | null | undefined): boolean {
  return (/1|true|t|yes|y/i).test(x || '');
}

export function envToBool(envVarName: string): boolean {
  return stringToBool(process.env[envVarName]);
}

export function envToString(envVarName: string, defaultValue: string): string {
  const val = process.env[envVarName];
  return val === undefined ? defaultValue : val;
}

export function envToNum(envVarName: string, defaultValue: number): number {
  const val = process.env[envVarName];
  return val === undefined ? defaultValue : Number(val);
}

export function envToArrayOfString(envVarName: string, defaultValue: Array<string> | null = null): Array<string> | null {
  const val = process.env[envVarName];
  return val == null ? defaultValue : val.split(',');
}

export const LOG_LEVEL = envToString("LOG_LEVEL", "info");
export const PRETTY_PRINT_LOGS = envToBool('PRETTY_PRINT_LOGS');
export const METRICS = envToBool('METRICS');

// See: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin
export const PERMISSIVE_CORS = envToBool('PERMISSIVE_CORS');
export const DB_ALLOW_LIST = envToArrayOfString('DB_ALLOW_LIST');

// The default value is OPEN_READWRITE | OPEN_CREATE | OPEN_FULLMUTEX.
export const DB_READONLY = envToBool('DB_READONLY');
export const DB_CREATE = envToBool('DB_CREATE');
export const DB_PRIVATECACHE = envToBool('DB_PRIVATECACHE');

export const DEBUGGING_TAGS = envToBool('DEBUGGING_TAGS');
export const QUERY_LENGTH_LIMIT = envToNum('QUERY_LENGTH_LIMIT', Infinity);

export const MUTATIONS = envToBool('MUTATIONS');

export const DATASETS = envToBool('DATASETS');
export const DATASET_DELETE = envToBool('DATASET_DELETE');
export const DATASET_TEMPLATES = envToString('DATASET_TEMPLATES', "./dataset_templates");
export const DATASET_CLONES = envToString('DATASET_CLONES', "./dataset_clones");

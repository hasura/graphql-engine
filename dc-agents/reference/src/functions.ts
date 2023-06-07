
/**
 * This Module defines some mock functions that can be invoked.
 */

import { ArgumentValue, FunctionRequest, NamedArgument, TableName } from "@hasura/dc-api-types";
import { prettyPrintTableName, Rows } from "./query";
import { StaticData, } from "./data";

type RawScalarValue = (string | number | boolean | null)
type GetTable = (tableName: TableName) => Record<string, RawScalarValue>[] | undefined

export function respondToFunction(queryRequest: FunctionRequest, tableName: TableName, getTable: GetTable): Rows {
  const t = prettyPrintTableName(tableName);
  const f = functions[t];
  if(! f) {
    throw(Error(`Couldn't find function ${t}`));
  }
  return f(queryRequest, getTable);
}

const functions: Record<string, ((x: any, getTable: GetTable) => Rows)> = {
  '[Fibonacci]': fibonacci,
  '[SearchArticles]': search_articles,
}

function namedArguments(args: Array<NamedArgument>): Record<string, ArgumentValue> {
  return Object.fromEntries(args.map(a => [a.name, a.value]));
}

/**
 * 
 * @param q FunctionRequest with 'take' nat arg.
 * @returns List of n Fibonacci numbers
 */
function fibonacci(q: FunctionRequest): Rows {

  const argzArray = q.function_arguments;
  if(! argzArray) { throw(Error('Expecting function_arguments')); }
  const argz = namedArguments(argzArray);

  const n = argz['take'].value;
  if(! n) { throw(Error('Expecting `take` arg')); }

  let rows = [];
  let x = 0;
  let y = 1;
  let z = 1;
  for(let i = 0; i < n; i++) {
    rows.push({ Value: x });
    z = x + y;
    x = y;
    y = z;
  }
  return rows;
}

function search_articles(f: FunctionRequest, getTable: GetTable): Rows {

  const argzArray = f.function_arguments;
  if(! argzArray) { throw(Error('Expecting function_arguments')); }
  const argz = namedArguments(argzArray);

  const table = getTable(['Articles']);
  if(!table) { throw(Error('Could not find `Articles` table.')); }

  const q = argz['query']?.value;
  if(! q) { throw(Error('Expecting `query` arg')); }

  const results = table.filter(r => (new RegExp(q, "i")).test(`${r.title}`));
  return results;
}

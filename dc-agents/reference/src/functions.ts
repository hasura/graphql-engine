
/**
 * This Module defines some mock functions that can be invoked.
 */

import { ArgumentValue, FunctionName, FunctionRequestArgument, NamedArgument, TableName } from "@hasura/dc-api-types";
import { prettyPrintName, Rows } from "./query";

type RawScalarValue = (string | number | boolean | null)
type GetTable = (tableName: TableName) => Record<string, RawScalarValue>[] | undefined

export function respondToFunction(functionName: FunctionName, functionArgs: FunctionRequestArgument[], getTable: GetTable): Rows {
  const t = prettyPrintName(functionName);
  const f = functions[t];
  if(! f) {
    throw(Error(`Couldn't find function ${t}`));
  }
  return f(functionArgs, getTable);
}

const functions: Record<string, ((functionArgs: FunctionRequestArgument[], getTable: GetTable) => Rows)> = {
  '[Fibonacci]': fibonacci,
  '[SearchArticles]': search_articles,
}

function namedArguments(args: Array<NamedArgument>): Record<string, ArgumentValue> {
  return Object.fromEntries(args.map(a => [a.name, a.value]));
}

/**
 *
 * @param functionArgs Arguments, should contain the 'take' nat arg.
 * @returns List of n Fibonacci numbers
 */
function fibonacci(functionArgs: FunctionRequestArgument[]): Rows {
  const argz = namedArguments(functionArgs);

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

function search_articles(functionArgs: FunctionRequestArgument[], getTable: GetTable): Rows {
  if(! functionArgs) { throw(Error('Expecting function_arguments')); }
  const argz = namedArguments(functionArgs);

  const table = getTable(['Articles']);
  if(!table) { throw(Error('Could not find `Articles` table.')); }

  const q = argz['query']?.value;
  if(! q) { throw(Error('Expecting `query` arg')); }

  const results = table.filter(r => (new RegExp(q, "i")).test(`${r.title}`));
  return results;
}

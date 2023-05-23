
/**
 * This Module defines some mock functions that can be invoked.
 */

import { ArgumentValue, FunctionRequest, NamedArgument, TableName } from "@hasura/dc-api-types";
import { prettyPrintTableName, Rows } from "./query";

export function respondToFunction(queryRequest: FunctionRequest, tableName: TableName): Rows {
  const t = prettyPrintTableName(tableName);
  const f = functions[t];
  if(! f) {
    throw(Error(`Couldn't find function ${t}`));
  }
  return f(queryRequest);
}

const functions: Record<string, ((x: any) => Rows)> = {
  '[Fibonacci]': fibonacci,
  '[Fibbbbbbbs]': fibbbbbbbs,
  '[Fibbbbbbbs2]': fibbbbbbbs2,
  '[FunkyAdd]': funky_add,
}

function namedArguments(args: Array<NamedArgument>): Record<string, ArgumentValue> {
  return Object.fromEntries(args.map(a => [a.name, a.value]));
}

function fibonacci(q: FunctionRequest): Rows {

  const argzArray = q.function_arguments;
  if(! argzArray) { throw(Error('Expecting function_arguments')); }
  const argz = namedArguments(argzArray);

  const n = argz['take'].value;
  if(! n) { throw(Error('Expecting 0th arg')); }

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

function fibbbbbbbs(): Rows {
  return [
    { ArtistId: 0, Name: 'Joe' },
    { ArtistId: 1, Name: 'Jim' },
    { ArtistId: 1, Name: 'James' },
    { ArtistId: 2, Name: 'Jack' },
    { ArtistId: 3, Name: 'Joel' },
  ]
}

function fibbbbbbbs2(q: FunctionRequest): Rows {

  const argzArray = q.function_arguments;
  if(! argzArray) { throw(Error('Expecting function_arguments')); }
  const argz = namedArguments(argzArray);

  const n = argz['upto'].value;
  if(! argz) { throw(Error('Expecting 0th arg')); }

  let rows = [];
  let x = 0;
  let y = 1;
  let z = 1;
  for(let i = 0; i < n; i++) {
    rows.push({ ArtistId: x, Name: `Artist ${x}` });
    z = x + y;
    x = y;
    y = z;
  }
  return rows;
}

function funky_add(q: FunctionRequest): Rows {

  const argzArray = q.function_arguments;
  if(! argzArray) { throw(Error('Expecting function_arguments')); }
  const argz = namedArguments(argzArray);

  if(! argz) { throw(Error('Expecting function_arguments')); }

  const a = argz['a'].value;
  if(! a) { throw(Error('Expecting "a" arg')); }
  const b = argz['b'].value;
  if(! b) { throw(Error('Expecting "b" arg')); }
  const c = a + b;

  return [{ ArtistId: c, Name: `Artist ${c}`}];
}

// Hand-rolled equivalent of running @graphql-codegen/core with the
// @graphql-codegen/typescript plugin over an actions SDL, so the console
// doesn't need those packages as a runtime dependency just for this one
// template. Produces the same overall shape (Maybe<T>/InputMaybe<T>
// wrappers, a Scalars map, __typename? on object types) so generated code
// looks the same as before.

const BUILTIN_SCALARS: Record<string, string> = {
  ID: 'string',
  String: 'string',
  Boolean: 'boolean',
  Int: 'number',
  Float: 'number',
};

const definitionsByName = (ast: any) => {
  const byName = new Map<string, any>();
  ast.definitions.forEach((d: any) => {
    if (d.name?.value) {
      byName.set(d.name.value, d);
    }
  });
  return byName;
};

const collectCustomScalars = (ast: any, byName: Map<string, any>) => {
  const customScalars = new Set<string>();

  const visitTypeNode = (typeNode: any) => {
    let type = typeNode;
    while (type.kind !== 'NamedType') {
      type = type.type;
    }
    const typename = type.name.value;
    if (!(typename in BUILTIN_SCALARS) && !byName.has(typename)) {
      customScalars.add(typename);
    }
  };

  ast.definitions.forEach((d: any) => {
    if (d.kind === 'ScalarTypeDefinition') {
      customScalars.add(d.name.value);
    }
    if (d.fields) {
      d.fields.forEach((f: any) => {
        visitTypeNode(f.type);
        if (f.arguments) {
          f.arguments.forEach((a: any) => visitTypeNode(a.type));
        }
      });
    }
  });

  return Array.from(customScalars);
};

// Renders a (possibly List/NonNull-wrapped) type reference as TypeScript,
// e.g. `[String!]!` -> `Array<Scalars['String']['output']>`.
const renderTypeRef = (
  typeNode: any,
  byName: Map<string, any>,
  variant: 'input' | 'output',
  isNonNull = false,
): string => {
  if (typeNode.kind === 'NonNullType') {
    return renderTypeRef(typeNode.type, byName, variant, true);
  }

  const maybeWrapper = variant === 'input' ? 'InputMaybe' : 'Maybe';

  if (typeNode.kind === 'ListType') {
    const inner = renderTypeRef(typeNode.type, byName, variant, false);
    const list = `Array<${inner}>`;
    return isNonNull ? list : `${maybeWrapper}<${list}>`;
  }

  const typename = typeNode.name.value;
  let rendered: string;
  if (typename in BUILTIN_SCALARS) {
    rendered = `Scalars['${typename}']['${variant}']`;
  } else {
    const def = byName.get(typename);
    rendered =
      def?.kind === 'ScalarTypeDefinition'
        ? `Scalars['${typename}']['${variant}']`
        : typename;
  }

  return isNonNull ? rendered : `${maybeWrapper}<${rendered}>`;
};

const renderEnum = (def: any) => {
  const members = def.values
    .map((v: any) => `  ${v.name.value} = '${v.name.value}'`)
    .join(',\n');
  return `export enum ${def.name.value} {\n${members}\n}`;
};

const renderFields = (
  fields: any[],
  byName: Map<string, any>,
  variant: 'input' | 'output',
) => {
  return fields
    .map((f: any) => {
      const isRequired = f.type.kind === 'NonNullType';
      const typeStr = renderTypeRef(f.type, byName, variant);
      return `  ${f.name.value}${isRequired ? '' : '?'}: ${typeStr};`;
    })
    .join('\n');
};

const renderInputObject = (def: any, byName: Map<string, any>) => {
  return `export type ${def.name.value} = {\n${renderFields(
    def.fields,
    byName,
    'input',
  )}\n};`;
};

const renderObject = (def: any, byName: Map<string, any>) => {
  const body = renderFields(def.fields, byName, 'output');
  return `export type ${def.name.value} = {\n  __typename?: '${def.name.value}';\n${body}\n};`;
};

const renderFieldArgs = (
  parentName: string,
  field: any,
  byName: Map<string, any>,
) => {
  if (!field.arguments || field.arguments.length === 0) {
    return null;
  }
  const body = renderFields(field.arguments, byName, 'input');
  const argTypeName = `${parentName}${capitalize(field.name.value)}Args`;
  return { argTypeName, code: `export type ${argTypeName} = {\n${body}\n};` };
};

const capitalize = (s: string) =>
  s.length ? s[0].toUpperCase() + s.slice(1) : s;

// Generates a hasuraCustomTypes.ts-equivalent source string from an actions
// SDL AST. `rootFields` maps 'Mutation'/'Query' to just the action fields
// that belong to that root (mirrors how the SDL's Mutation/Query type is
// assembled from multiple `extend type` blocks upstream).
export const generateHasuraTypes = (
  ast: any,
  rootFields: { Mutation: any[]; Query: any[] },
): string => {
  const byName = definitionsByName(ast);
  const customScalars = collectCustomScalars(ast, byName);

  const chunks: string[] = [];

  chunks.push(`export type Maybe<T> = T | null;`);
  chunks.push(`export type InputMaybe<T> = Maybe<T>;`);

  const scalarLines = ['ID', 'String', 'Boolean', 'Int', 'Float']
    .map(
      (s) =>
        `  ${s}: { input: ${BUILTIN_SCALARS[s]}; output: ${BUILTIN_SCALARS[s]}; }`,
    )
    .concat(
      customScalars.map((s) => `  ${s}: { input: unknown; output: unknown; }`),
    )
    .join('\n');
  chunks.push(
    `/** All built-in and custom scalars, mapped to their actual values */\nexport type Scalars = {\n${scalarLines}\n};`,
  );

  ast.definitions.forEach((d: any) => {
    if (d.kind === 'EnumTypeDefinition') {
      chunks.push(renderEnum(d));
    }
  });

  ast.definitions.forEach((d: any) => {
    if (d.kind === 'InputObjectTypeDefinition') {
      chunks.push(renderInputObject(d, byName));
    }
  });

  ast.definitions.forEach((d: any) => {
    if (
      d.kind === 'ObjectTypeDefinition' &&
      d.name.value !== 'Mutation' &&
      d.name.value !== 'Query'
    ) {
      chunks.push(renderObject(d, byName));
    }
  });

  (['Mutation', 'Query'] as const).forEach((rootName) => {
    const fields = rootFields[rootName];
    if (!fields.length) return;

    const rootDef = { name: { value: rootName }, fields };
    chunks.push(renderObject(rootDef, byName));

    fields.forEach((f: any) => {
      const argsType = renderFieldArgs(rootName, f, byName);
      if (argsType) {
        chunks.push(argsType.code);
      }
    });
  });

  return chunks.join('\n\n') + '\n';
};

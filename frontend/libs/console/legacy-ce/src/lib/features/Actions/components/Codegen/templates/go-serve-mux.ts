import { Kind, ListTypeNode, NamedTypeNode, TypeNode } from 'graphql';
import { CodegenDerive, CodegenFile } from './types';
import { codeBlock } from './codeBlock';
import {
  DEFAULT_HASURA_ENDPOINT,
  FieldInfo,
  TypeMap,
  buildActionTypes,
  capitalize,
  getRootFieldName,
  getSampleValue,
} from './actionTypes';

const GO_SCALARS: Record<string, string> = {
  ID: 'int',
  Int: 'int',
  Float: 'float32',
  String: 'string',
  Boolean: 'bool',
};

// the Go type declared for a GraphQL named type in the generated types file
const toGoTypeName = (typename: string, typeMap: TypeMap) =>
  GO_SCALARS[typename] ??
  (typeMap.scalars.includes(typename) ? capitalize(typename) : typename);

// e.g. `[Int]!` -> `[]*int`; a nullable named type is a pointer, a nullable
// list is a (nil-able) slice
const toGoType = (
  type: TypeNode,
  typeMap: TypeMap,
  nullable = true,
): string => {
  if (type.kind === Kind.NON_NULL_TYPE) {
    return toGoType(type.type, typeMap, false);
  }
  if (type.kind === Kind.LIST_TYPE) {
    return `[]${toGoType(type.type, typeMap)}`;
  }
  return `${nullable ? '*' : ''}${toGoTypeName(type.name.value, typeMap)}`;
};

const toGoEnumConstant = (enumName: string, value: string) =>
  `${enumName}${capitalize(value)}`;

const toGoField = ({ name, type }: FieldInfo, typeMap: TypeMap) =>
  `${capitalize(name)} ${toGoType(type, typeMap)} \`json:"${name}"\``;

// placeholder Go value of a non-null type; `populate` fills in an object's
// fields and a list's first element, which is done for the action output
// itself but not for the values nested in it
const toGoValue = (
  type: ListTypeNode | NamedTypeNode,
  typeMap: TypeMap,
  populate: boolean,
): string => {
  if (type.kind === Kind.LIST_TYPE) {
    const goType = toGoType(type, typeMap, false);

    const element = populate ? toGoSample(type.type, typeMap, true) : 'nil';
    return element === 'nil'
      ? `${goType}{}`
      : codeBlock`${goType}{\n  ${element},\n}`;
  }

  const typename = type.name.value;
  const fields = typeMap.types[typename];
  if (fields) {
    // nullable fields are left out, as nil
    const values = populate
      ? fields
          .filter((f) => f.type.kind === Kind.NON_NULL_TYPE)
          .map(
            (f) =>
              `${capitalize(f.name)}: ${toGoSample(f.type, typeMap, false)}`,
          )
      : [];
    return values.length
      ? codeBlock`${typename}{\n  ${values.join(',\n')},\n}`
      : `${typename}{}`;
  }

  const enumValues = typeMap.enums[typename];
  if (enumValues) {
    return enumValues.length ? toGoEnumConstant(typename, enumValues[0]) : '""';
  }

  return getSampleValue(typename);
};

const toGoSample = (
  type: TypeNode,
  typeMap: TypeMap,
  populate: boolean,
): string => {
  if (type.kind === Kind.NON_NULL_TYPE) {
    return toGoValue(type.type, typeMap, populate);
  }
  if (
    populate &&
    type.kind === Kind.NAMED_TYPE &&
    typeMap.types[type.name.value]
  ) {
    return `&${toGoValue(type, typeMap, populate)}`;
  }
  return 'nil';
};

const typeMapToGo = (typeMap: TypeMap) => {
  const scalars = typeMap.scalars
    .map((name) => `type ${capitalize(name)} string`)
    .join('\n\n');

  const enums = Object.entries(typeMap.enums)
    .map(([name, values]) => {
      const constants = values
        .map((v) => `${toGoEnumConstant(name, v)} ${name} = "${v}"`)
        .join('\n');
      return codeBlock`
        type ${name} string

        const (
            ${constants}
        )
      `;
    })
    .join('\n\n');

  const types = Object.entries(typeMap.types)
    .map(
      ([name, fields]) => codeBlock`
        type ${name} struct {
            ${fields.map((f) => toGoField(f, typeMap)).join('\n')}
        }
      `,
    )
    .join('\n\n');

  return ['package main', scalars, enums, types].join('\n\n');
};

const generateGraphQLTypes = (
  actionName: string,
  returnType: string,
  operation: string,
) => codeBlock`

  type GraphQLRequest struct {
    Query     string               \`json:"query"\`
    Variables ${actionName}Args \`json:"variables"\`
  }
  type GraphQLData struct {
    ${getRootFieldName(operation, true)} ${returnType} \`json:"${getRootFieldName(operation)}"\`
  }
  type GraphQLResponse struct {
    Data   GraphQLData    \`json:"data,omitempty"\`
    Errors []GraphQLError \`json:"errors,omitempty"\`
  }
`;

const generateExecute = (
  actionName: string,
  operation: string,
  endpoint: string,
) => codeBlock`
  func execute(variables ${actionName}Args) (response GraphQLResponse, err error) {

    // build the request body
    reqBody := GraphQLRequest{
      Query:     \`${operation}\`,
      Variables: variables,
    }
    reqBytes, err := json.Marshal(reqBody)
    if err != nil {
      return
    }

    // make request to Hasura
    resp, err := http.Post("${endpoint}", "application/json", bytes.NewBuffer(reqBytes))
    if err != nil {
      return
    }
    defer resp.Body.Close()

    // parse the response
    respBytes, err := io.ReadAll(resp.Body)
    if err != nil {
      return
    }
    err = json.Unmarshal(respBytes, &response)
    if err != nil {
      return
    }

    // return the response
    return
  }

`;

const generateDerivedActionFunction = (
  actionName: string,
  returnType: string,
  operation: string,
) => codeBlock`
  // Auto-generated function that takes the Action parameters and must return it's response type
  func ${actionName}(args ${actionName}Args) (response ${returnType}, err error) {

    hasuraResponse, err := execute(args)

    // throw if any unexpected error happens
    if err != nil {
      return
    }

    // delegate Hasura error
    if len(hasuraResponse.Errors) != 0 {
      err = errors.New(hasuraResponse.Errors[0].Message)
      return
    }

    response = hasuraResponse.Data.${getRootFieldName(operation, true)}
    return

  }

`;

const generateActionFunction = (
  actionName: string,
  returnType: string,
  actionOutput: TypeNode,
  typeMap: TypeMap,
) => {
  const outputType =
    actionOutput.kind === Kind.NON_NULL_TYPE ? actionOutput.type : actionOutput;
  const responseAssignment = `response = ${toGoValue(outputType, typeMap, true)}`;

  return codeBlock`
    // Auto-generated function that takes the Action parameters and must return it's response type
    func ${actionName}(args ${actionName}Args) (response ${returnType}, err error) {
      ${responseAssignment}
      return response, nil
    }
  `;
};

// actionName: Name of the action
// actionsSdl: GraphQL SDL string that has the action and dependent types
// derive: Whether this action was asked to be derived from a Hasura operation
//         derive.operation contains the operation string
export const templater = (
  actionName: string,
  actionsSdl: string,
  derive: CodegenDerive,
): CodegenFile[] => {
  const { actionOutput, typeMap } = buildActionTypes(actionName, actionsSdl);
  // the handler returns the action output type itself, not a pointer to it
  const returnType = toGoType(actionOutput, typeMap, false);
  const operation = derive?.operation;

  const imports = [
    ...(operation ? ['bytes'] : []),
    'encoding/json',
    ...(operation ? ['errors'] : []),
    'io',
    'log',
    'net/http',
  ]
    .map((i) => `"${i}"`)
    .join('\n');
  const graphqlTypes = operation
    ? generateGraphQLTypes(actionName, returnType, operation)
    : '';
  const execute = operation
    ? generateExecute(
        actionName,
        operation,
        derive?.endpoint || DEFAULT_HASURA_ENDPOINT,
      )
    : '';
  const actionFunction = operation
    ? generateDerivedActionFunction(actionName, returnType, operation)
    : generateActionFunction(actionName, returnType, actionOutput, typeMap);

  const handler = codeBlock`

    package main

    import (
      ${imports}
    )

    type ActionPayload struct {
      SessionVariables map[string]interface{} \`json:"session_variables"\`
      Input            ${actionName}Args \`json:"input"\`
    }

    type GraphQLError struct {
      Message string \`json:"message"\`
    }

    ${graphqlTypes}

    func handler(w http.ResponseWriter, r *http.Request) {

      // set the response header as JSON
      w.Header().Set("Content-Type", "application/json")

      // read request body
      reqBody, err := io.ReadAll(r.Body)
      if err != nil {
        http.Error(w, "invalid payload", http.StatusBadRequest)
        return
      }

      // parse the body as action payload
      var actionPayload ActionPayload
      err = json.Unmarshal(reqBody, &actionPayload)
      if err != nil {
        http.Error(w, "invalid payload", http.StatusBadRequest)
        return
      }

      // Send the request params to the Action's generated handler function
      result, err := ${actionName}(actionPayload.Input)

      // throw if an error happens
      if err != nil {
        errorObject := GraphQLError{
          Message: err.Error(),
        }
        errorBody, _ := json.Marshal(errorObject)
        w.WriteHeader(http.StatusBadRequest)
        w.Write(errorBody)
        return
      }

      // Write the response as JSON
      data, _ := json.Marshal(result)
      w.Write(data)

    }

    ${actionFunction}
    ${execute}

    // HTTP server for the handler
    func main() {
      mux := http.NewServeMux()
      mux.HandleFunc("/${actionName}", handler)

      err := http.ListenAndServe(":3000", mux)
      log.Fatal(err)
    }
  `;

  return [
    { name: `${actionName}.go`, content: handler },
    { name: `${actionName}Types.go`, content: typeMapToGo(typeMap) },
  ];
};

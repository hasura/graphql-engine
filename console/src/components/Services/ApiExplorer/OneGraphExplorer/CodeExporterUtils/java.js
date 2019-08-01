import { isWrappingType, isListType, validate } from 'graphql';
import gql from 'graphql-tag';
import 'codemirror/mode/clike/clike';

const hasuraToJavaTypeMap = {
  numeric: 'double',
  Int: 'int',
  Boolean: 'boolean',
  bigint: 'long long int',
  String: 'String',
};

const getJavaType = (hasuraType, isList, isScalar) => {
  if (isScalar) {
    if (isList) {
      return `List <${hasuraToJavaTypeMap[hasuraType] || 'String'}>`;
    }
    return hasuraToJavaTypeMap[hasuraType] || 'String';
  }
  if (isList) {
    return `List <${hasuraType}>`;
  }
  return hasuraType;
};

let classNameMap = {};

const clearClassnameMap = () => {
  classNameMap = {};
};

let aliasMap = {};

const clearAliasMap = () => {
  aliasMap = {};
};

const getNameFromAlias = alias => {
  return aliasMap[alias] || alias;
};

const generateClassName = maybeName => {
  let suffix = '';
  let name;
  let uncasedName;
  while (true) {
    uncasedName = `${maybeName}${suffix}`;
    name = uncasedName.charAt(0).toUpperCase() + uncasedName.slice(1);
    if (!classNameMap[name]) break;
    if (suffix === '') {
      suffix = 2;
    } else {
      suffix += 1;
    }
  }
  classNameMap[name] = true;
  return name;
};

const getUnderlyingType = t => {
  let _type = t;
  let isList = false;
  while (isWrappingType(_type)) {
    if (isListType(_type)) {
      isList = true;
    }
    _type = _type.ofType;
  }
  return {
    type: _type,
    isList,
  };
};

const populateTypes = (classes, schema) => {
  const classesWithTypes = JSON.parse(JSON.stringify(classes));
  classes.forEach((c, i) => {
    if (!c.path) return;
    let currentType = schema._queryType;
    c.path.forEach(field => {
      currentType = getUnderlyingType(currentType._fields[field].type).type;
    });
    Object.keys(c.fields).forEach(f => {
      if (!c.fields[f]) {
        const fieldName = getNameFromAlias(f);
        const _type = currentType._fields[fieldName].type;
        const unWrappedType = getUnderlyingType(_type);
        classesWithTypes[i].fields[f] = getJavaType(
          unWrappedType.type,
          unWrappedType.isList,
          true
        );
      } else {
        const fieldName = getNameFromAlias(f);
        const _type = currentType._fields[fieldName].type;
        const unWrappedType = getUnderlyingType(_type);
        classesWithTypes[i].fields[f] = getJavaType(
          c.fields[f],
          unWrappedType.isList,
          false
        );
      }
    });
  });
  return classesWithTypes;
};

const generateClass = (className, selections, push, path) => {
  const currentClass = {
    name: className,
    fields: {},
    path,
  };
  for (let i = selections.length - 1; i >= 0; i--) {
    const selection = selections[i];
    let selectionName = selection.name.value;
    if (selection.alias) {
      selectionName = selection.alias.value;
      aliasMap[selectionName] = selection.name.value;
    }
    if (selection.selectionSet) {
      currentClass.fields[selectionName] = generateClass(
        generateClassName(selectionName),
        selection.selectionSet.selections,
        push,
        [...path, selection.name.value]
      );
    } else {
      currentClass.fields[selectionName] = false;
    }
  }
  push(currentClass);
  return className;
};

const generateClasses = (queryString, schema) => {
  const queryAst = gql`
    ${queryString}
  `;
  const classes = [];
  const queryDef = queryAst.definitions[0];
  const queryName = generateClassName('GraphQLResponse');
  const pushClass = c => {
    classes.push(c);
  };
  classes.push({
    name: queryName,
    fields: {
      data: generateClass(
        generateClassName('GraphQLData'),
        queryDef.selectionSet.selections,
        pushClass,
        []
      ),
    },
  });
  const classesWithTypes = populateTypes(classes, schema);
  return classesWithTypes;
};

const generateClassesCode = classes => {
  let code = '';
  const numClasses = classes.length;
  for (let i = 0; i < numClasses; i++) {
    const c = classes[i];
    let classFieldsCode = '';
    Object.keys(c.fields).forEach(f => {
      classFieldsCode = `${classFieldsCode}
  public final ${c.fields[f]} ${f};`;
    });

    const constructorCode = `
  public ${c.name}(${Object.keys(c.fields)
      .map(f => `${c.fields[f]} _${f}`)
      .join(', ')}) {
    ${Object.keys(c.fields)
      .map(f => `${f} = _${f};`)
      .join('\n    ')}
  }
    `;

    code = `${code}
public class ${c.name} {
${classFieldsCode}
${constructorCode}
}
    `;
  }
  return code;
};

const javaSnippet = {
  language: 'Java',
  prismLanguage: 'javascript',
  codeMirrorMode: 'clike',
  name: 'OkHttp',
  options: [],
  generate: config => {
    clearClassnameMap();
    clearAliasMap();
    const {
      operationDataList,
      serverUrl,
      headers,
      context: { schema },
    } = config;
    const { query, variables } = operationDataList[0];
    if (
      validate(
        schema,
        gql`
          ${query}
        `
      ).length
    ) {
      return 'Invalid Query';
    }
    const generatedClasses = generateClasses(query, schema);
    let stringifiedQuery = JSON.stringify(query);
    stringifiedQuery = stringifiedQuery.substring(
      1,
      stringifiedQuery.length - 1
    );
    let stringifiedVariables;
    try {
      stringifiedVariables = JSON.stringify(JSON.stringify(variables));
      stringifiedVariables = stringifiedVariables.substring(
        1,
        stringifiedQuery.length - 1
      );
    } catch (e) {
      console.error('Invalid variables: ', e);
    }

    let requestBodyDef;
    if (stringifiedVariables) {
      requestBodyDef = `RequestBody body = RequestBody.create(mediaType, "{\\n\\t\\"query\\": \\"${stringifiedQuery}\\",\\n\\t\\"variables\\": ${stringifiedVariables}\\n}");`;
    } else {
      requestBodyDef = `RequestBody body = RequestBody.create(mediaType, "{\\n\\t\\"query\\": \\"${stringifiedQuery}\\"\\n}");`;
    }

    let headersCode = '';
    Object.keys(headers).forEach(h => {
      headersCode += `
  .addHeader("${h}", "${headers[h]}")`;
    });

    return `
// HTTP Execution

OkHttpClient client = new OkHttpClient();

MediaType mediaType = MediaType.parse("application/json");

${requestBodyDef}

Request request = new Request.Builder()
  .url("${serverUrl}")
  .post(body)${headersCode}
  .build();

Response response = client.newCall(request).execute();
String responseString = response.body().string();

Gson g = new Gson();
GraphQLResponse r = g.fromJson(responseString, GraphQLResponse.class);


// Required classes
${generateClassesCode(generatedClasses)}
    `;
  },
};

export default javaSnippet;

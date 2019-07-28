const java = {
  language: 'Java',
  prismLanguage: 'Java',
  codeMirrorMode: 'go',
  name: 'OkHttp',
  options: [],
  generate: config => {
    const { operationDataList, serverUrl, headers } = config;
    const { query, variables } = operationDataList[0];
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
OkHttpClient client = new OkHttpClient();

MediaType mediaType = MediaType.parse("application/json");

${requestBodyDef}

Request request = new Request.Builder()
  .url("${serverUrl}")
  .post(body)${headersCode}
  .build();

Response response = client.newCall(request).execute();
    `;
  },
};

const snippets = [java];

export default snippets;

const parseRowData = (row, dataType) => {
  switch (dataType) {
    case 'request':
      switch (row.request.version) {
        case '2':
          const data = row.request.payload;
          return {
            data: data,
            headers: row.request.headers,
          };
        default:
          return {
            data: row.request,
          };
      }
    case 'response':
      let data;
      switch (row.response.version) {
        case '2':
          try {
            // Handle graphql-engine server error message
            if (row.response.data.message) {
              data = row.response.data;
            } else {
              data = JSON.parse(row.response.data.body);
            }
          } catch (e) {
            console.error(e);
            data = row.response.data.body;
          }
          return {
            data: data,
            headers: row.response.data.headers,
            status_code: row.response.data.status,
          };
        default:
          try {
            data = JSON.parse(row.response);
          } catch (e) {
            console.error(e);
            data = row.response;
          }
          return {
            data: data,
            status_code: row.status,
          };
      }
    default:
      return false;
  }
};

export default parseRowData;

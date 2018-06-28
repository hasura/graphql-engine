import fetch from 'isomorphic-fetch';

const requestActionPlain = (url, options, SUCCESS, ERROR) => {
  /*
  if (!(options.credentials)) {
    options.credentials = 'include';
  }
  */
  return dispatch => {
    const p1 = new Promise((resolve, reject) => {
      fetch(url, options).then(
        response => {
          if (response.ok) {
            return response.text().then(results => {
              if (SUCCESS) {
                dispatch({ type: SUCCESS, data: results });
              }
              resolve(results);
            });
          }
          if (response.status >= 400 && response.status < 500) {
            return response.text().then(errorMsg => {
              if (ERROR) {
                dispatch({ type: ERROR, data: errorMsg });
              }
              reject(errorMsg);
            });
          }
          if (ERROR) {
            dispatch({ type: ERROR, response });
          }
          reject();
        },
        error => {
          if (ERROR) {
            dispatch({
              type: ERROR,
              code: 'server-connection-failed',
              message: error.message,
              data: error.message,
            });
          }
          reject(error);
        }
      );
    });
    return p1;
  };
};

export default requestActionPlain;

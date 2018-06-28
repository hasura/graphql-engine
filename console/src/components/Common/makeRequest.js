import fetch from 'isomorphic-fetch';

const getCorrectExtension = contentType => {
  return contentType.split('/')[1];
};

const triggerDownload = (data, contentType) => {
  const link = document.createElement('a');
  link.href = data;
  link.download = 'filename.' + getCorrectExtension(contentType);
  link.click();
  setTimeout(() => {
    window.URL.revokeObjectURL(data);
  }, 100);
};

const handleResponse = (
  respHeader,
  response,
  dispatch,
  EVENT,
  responseObj,
  callback
) => {
  if (respHeader) {
    if (respHeader.indexOf('application/json') === -1) {
      if (respHeader.indexOf('text/plain') !== -1) {
        return response
          .text()
          .then(results => {
            responseObj.response = results;
            if (EVENT) {
              dispatch({ type: EVENT, data: responseObj });
            }
            callback(responseObj);
          })
          .catch(() => {
            responseObj.response = 'Something went wrong, please try again';
            if (EVENT) {
              dispatch({ type: EVENT, data: responseObj });
            }
            callback(responseObj);
          });
      }
      return response
        .blob()
        .then(results => {
          const isImage = respHeader.indexOf('image/') !== -1;
          const downloadUrl = URL.createObjectURL(results);
          responseObj.response = isImage
            ? downloadUrl
            : `File with content-type \`'${respHeader}\` downloaded successfully`;
          responseObj.isImage = isImage;
          triggerDownload(downloadUrl, respHeader);
          if (EVENT) {
            dispatch({ type: EVENT, data: responseObj });
          }
          callback(responseObj);
        })
        .catch(() => {
          responseObj.response = 'Something went wrong, please try again';
          if (EVENT) {
            dispatch({ type: EVENT, data: responseObj });
          }
          callback(responseObj);
        });
    }
    return response
      .json()
      .then(results => {
        responseObj.response = results;
        if (EVENT) {
          dispatch({ type: EVENT, data: responseObj });
        }
        callback(responseObj);
      })
      .catch(() => {
        responseObj.response = 'Something went wrong, please try again';
        if (EVENT) {
          dispatch({ type: EVENT, data: responseObj });
        }
        callback(responseObj);
      });
  }
  throw Error('invalid content-type');
};

const requestAction = (url, options, SUCCESS, ERROR) => {
  const fetchTime = Date.now();
  return dispatch => {
    const p1 = new Promise((resolve, reject) => {
      fetch(url, options).then(
        response => {
          const totalTime = Date.now() - fetchTime;
          const responseObj = {};
          const respHeader = response.headers.get('content-type');
          responseObj.statusCode = response.status;
          responseObj.timeElapsed = totalTime;
          responseObj.responseHeaders = response.headers;
          try {
            if (response.ok) {
              handleResponse(
                respHeader,
                response,
                dispatch,
                SUCCESS,
                responseObj,
                resolve
              );
            }
            if (response.status >= 400 && response.status <= 500) {
              handleResponse(
                respHeader,
                response,
                dispatch,
                ERROR,
                responseObj,
                reject
              );
            }
            reject(responseObj);
          } catch (e) {
            responseObj.response = e.toString();
            if (ERROR) {
              dispatch({ type: ERROR, data: responseObj });
            }
            reject(responseObj);
          }
        },
        error => {
          const responseObj = {};
          responseObj.statusCode = 500;
          responseObj.timeElapsed = 0;
          responseObj.responseHeaders = new Headers();
          responseObj.response = error;
          if (ERROR) {
            dispatch({
              type: ERROR,
              code: 'server-connection-failed',
              data: responseObj,
            });
          }
          reject(responseObj);
        }
      );
    });
    return p1;
  };
};

export default requestAction;
export { requestAction as customRequestAction };

export const signup = (email, password, successCb, errorCb) => {
  fetch(
    'https://learn.hasura.io/auth/signup',
    {
      method: 'POST',
      headers: {
        'content-type': 'application/json'
      },
      body: JSON.stringify({
        username: email,
        password
      })
    }
  )
  .then(resp => resp.json()
    .then(respObj => {
      if (resp.status === 200) {
        successCb(respObj);
        return;
      }
      if (respObj.errors && respObj.errors.length > 0) {
        errorCb({
          title: 'Error',
          message: respObj.errors[0].message
        })
        return;
      } else if (respObj.message && respObj.message.includes('unique')) {
        errorCb({
          title: 'This email is already signed up',
          message: 'Try logging in'
        })
        return;
      }
      errorCb({
        title: 'Unknown Error',
        message: 'Please try again'
      })
    })
  )
  .catch(err => {
    console.log(err);
    errorCb({
      title: 'Unexpected',
      message: 'Please try again'
    })
  })
};

export const login = (email, password, successCb, errorCb) => {
  fetch(
    'https://learn.hasura.io/auth/login',
    {
      method: 'POST',
      headers: {
        'content-type': 'application/json'
      },
      body: JSON.stringify({
        username: email,
        password
      })
    }
  )
  .then(resp => {
    console.log(resp);
    resp.json()
    .then(respObj => {
      console.log(respObj);
      if (resp.status === 200) {
        successCb(respObj);
        return;
      }
      if (respObj.error) {
        errorCb({
          title: 'Error',
          message: respObj.error
        });
        return;
      }
      errorCb({
        title: 'Unknown Error',
        message: 'Please try again'
      })
    })}
  )
  .catch(err => {
    console.log(err);
    errorCb({
      title: 'Unexpected',
      message: 'Please try again'
    })
  })
};

let logout;
export const setLogout = (l) => {
  logout = l
};
export { logout };
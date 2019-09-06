open Auth0

type state = {
  token: option(string)
};

type action =
  | SetToken(option(string));

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer((state, action) =>
  switch(action) {
  | SetToken(token) => { token: token }
  }, { token: None});
  React.useEffect2(() => {
    let token = Util.getTokenFromStorage();
    if (state.token == None && token !== None) {
      dispatch(SetToken(token))
    };
    None
  }, (None, None));
  switch(state.token) {
    | Some(_) => if (Util.isSessionValid()) { <App /> } else { <Login />}
    | None => <Login />
  };
};
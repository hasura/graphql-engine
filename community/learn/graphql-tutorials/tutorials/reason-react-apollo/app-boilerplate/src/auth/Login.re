let authClient = Auth0.createClient(Auth0.options);

[@react.component]
let make = () => {
  <div className="overlay">
    <div className="overlay-content">
      <div className="overlay-heading">
        {ReasonReact.string("Welcome to the GraphQL tutorial app")}
      </div>
      <div className="overlay-message"> 
        {ReasonReact.string("Please login to continue")}
      </div>
      <div className="overlay-action">
        <button
          id="qsLoginBtn"
          className="btn-primary btn-margin btn loginBtn"
          onClick={
            _ => {
              authClient##authorize();
            }
          }
        >
          {ReasonReact.string("Log in")}
        </button>
      </div>
    </div>
  </div>
}

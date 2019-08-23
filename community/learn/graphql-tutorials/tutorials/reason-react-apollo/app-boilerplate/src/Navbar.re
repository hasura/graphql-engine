[@react.component]
let make = () => {
  let logout = (_) => {
    Util.removeSessionFromStorage();
    ReasonReactRouter.push("/login");
  };
  <nav className="m-bottom-0 navbar navbar-default">
    <div className="container-fluid">
      <div className="navHeader navbar-header">
        <span className="navBrand navbar-brand">
          {ReasonReact.string("GraphQL Tutorial App")}
        </span>
        <ul className="nav navbar-nav navbar-right">
          <li role="presentation">
            <a role="button" href="">
              <button
                id="qsLogoutBtn"
                className="btn-margin logoutBtn btn btn-primary"
                onClick=logout
              >
                {ReasonReact.string("Log out")}      
              </button>
            </a>
          </li>
        </ul>
      </div>
    </div> 
  </nav>
}
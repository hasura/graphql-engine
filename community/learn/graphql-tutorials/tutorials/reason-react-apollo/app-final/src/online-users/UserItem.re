[@react.component]
let make = (~user) => {
  let key = switch(user##id) {
    | None => "none"
    | Some(id) => id
  };
  let name = switch(user##user) {
    | None => "null"
    | Some(u) => u##name
  };
  <div className="userInfo" key=key>
    <div className="userImg">
      <i className="far fa-user" />
    </div>
    <div className="userName">
      {ReasonReact.string(name)}
    </div>
  </div>
};
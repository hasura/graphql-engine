[@react.component]
let make = (~user) => {
  <div className="userInfo">
    <div className="userImg">
      <i className="far fa-user" />
    </div>
    <div className="userName">
      {ReasonReact.string(user##name)}
    </div>
  </div>
}
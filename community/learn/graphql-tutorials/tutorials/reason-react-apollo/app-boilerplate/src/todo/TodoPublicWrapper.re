[@react.component]
let make = () => {
  <div className="todoWrapper">
    <div className="sectionHeader">
      {ReasonReact.string("Public feed (realtime)")}
    </div>
    <TodoInput isPublic=true/>
    <TodoPublicList />
  </div>
}


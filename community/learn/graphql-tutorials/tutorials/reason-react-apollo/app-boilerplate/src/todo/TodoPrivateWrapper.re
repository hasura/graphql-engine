[@react.component]
let make = () => {
  <div className="todoWrapper">
    <div className="sectionHeader">
      {ReasonReact.string("Personal todos")}
    </div>
    <TodoInput isPublic=false />
    <TodoPrivateList />
  </div>
}


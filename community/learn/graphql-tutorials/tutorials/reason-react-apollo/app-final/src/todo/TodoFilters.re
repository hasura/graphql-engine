[@react.component]

let make = (~todoCount: int, ~currentFilter: string, ~filterFunc) => {
  let todoCountLabel = string_of_int(todoCount) ++ " item" ++ if (todoCount === 1) { "s" } else { "" };
  <div className="footerList">
    <span> {ReasonReact.string(todoCountLabel)} </span>
    <ul>
      <li onClick={(_event) => filterFunc("all")}>
        <a className={currentFilter === "all" ? "selected" : ""}>
         {ReasonReact.string("All")}
        </a>
      </li>

      <li onClick={(_event) => filterFunc("active")}>
        <a className={currentFilter === "active" ? "selected" : ""}>
          {ReasonReact.string("Active")}
        </a>
      </li>

      <li onClick={_event => filterFunc("complete")}>
        <a className={currentFilter === "complete" ? "selected" : ""}>
          {ReasonReact.string("Completed")}
        </a>
      </li>
    </ul>
  </div>
}
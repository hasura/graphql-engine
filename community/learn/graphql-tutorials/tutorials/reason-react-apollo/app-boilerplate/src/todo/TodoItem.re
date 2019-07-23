[@react.component]
let make = (~todo) => {
  let todoStyle = if (todo##is_completed) {
    " complete"
  } else {
    ""
  };

  <li>
    <div className="view">
      <div className="round">
        <input
          type_="checkbox"
          checked={todo##is_completed}
          id={string_of_int(todo##id)}
        />
        <label htmlFor={string_of_int(todo##id)}/>
      </div>
    </div>
    <div className={"labelContent" ++ todoStyle}>
      <div>
        {ReasonReact.string(todo##title)}
      </div>
    </div>
    <button className="closeBtn">
      {ReasonReact.string("x")}
    </button>
  </li>
}
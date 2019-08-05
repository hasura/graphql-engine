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
        <GraphQLQueries.ToggleMyTodoMutation>
          ...{
            (updateTodo, _) => {
              let toggleTodo = GraphQLQueries.ToggleMyTodo.make(~id=todo##id, ~isCompleted=!todo##is_completed, ());
              <input
                type_="checkbox"
                checked={todo##is_completed}
                id={string_of_int(todo##id)}
                onChange={
                  event => {
                    updateTodo(
                      ~variables=toggleTodo##variables,
                      ()
                    ) |> ignore;
                  }
                }
              />
            }
          }
        </GraphQLQueries.ToggleMyTodoMutation>
        <label htmlFor={string_of_int(todo##id)}/>
      </div>
    </div>
    <div className={"labelContent" ++ todoStyle}>
      <div>
        {ReasonReact.string(todo##title)}
      </div>
    </div>
    <GraphQLQueries.DeleteMyTodoMutation>
      ...{
        (deleteTodo, _) => {
          let removeTodo = GraphQLQueries.DeleteMyTodo.make(~id=todo##id, ());
          <button
            className="closeBtn"
            onClick={
              ev => {
                deleteTodo(
                  ~variables=removeTodo##variables,
                  ~refetchQueries=[|"getMyTodos"|],
                  ()
                ) |> ignore;
              }
            }
          >
            {ReasonReact.string("x")}
          </button>
        }
      }
    </GraphQLQueries.DeleteMyTodoMutation>
  </li>
}
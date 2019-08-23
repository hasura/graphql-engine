type state = {
  filter: string
};

// action type
type action =
  | UpdateFilter(string);

[@react.component]
let make = () => {

  // state and reducer
  let (state, dispatch) = React.useReducer((_, action) =>
    switch(action) {
      | UpdateFilter(filter) => { filter: filter }
    },
    {filter: "all"}
  );

  <GraphQLQueries.GetMyTodosQuery>
    ...{
      ({result}) => switch(result) {
        | Loading => <div>{ReasonReact.string("Loading")}</div>
        | Error(error) => {
          Js.Console.error(error);
          <div>{ReasonReact.string("Loading")}</div>
        }
        | Data(data) => {
          let filteredTodos =
            data##todos
            |> Array.to_list
            |> List.filter((todo) =>
              switch(todo##is_completed) {
                | true => state.filter === "all" || state.filter === "complete"
                | false => state.filter === "all" || state.filter === "active"
              })
            |> List.map((t) => <TodoItem todo={t} />)

          // filter callback
          let filterTodos = (f) => {
            dispatch(UpdateFilter(f))
          };

          // return JSX
          <React.Fragment>
            <div className="todoListWrapper">
              <ul>
                {ReasonReact.array(Array.of_list(filteredTodos))}
              </ul>
            </div>
            <TodoFilters
              todoCount={List.length(filteredTodos)}
              currentFilter={state.filter}
              filterFunc={filterTodos}
            />
          </React.Fragment>
        } 
      }
    }
  </GraphQLQueries.GetMyTodosQuery>
}

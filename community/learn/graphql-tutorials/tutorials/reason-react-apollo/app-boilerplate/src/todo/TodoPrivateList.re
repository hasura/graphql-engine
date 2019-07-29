// sample data
let sampleTodos = [|
  {
    "id": 1,
    "title": "This is private todo 1",
    "is_completed": true,
    "is_public": false
  },
  {
    "id": 2,
    "title": "This is private todo 2",
    "is_completed": false,
    "is_public": false
  }
|];

// state type
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

  let filteredTodos =
    sampleTodos
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

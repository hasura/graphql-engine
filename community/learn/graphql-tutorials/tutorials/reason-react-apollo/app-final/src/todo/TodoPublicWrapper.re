[@react.component]
let make = () => {
  <div className="todoWrapper">
    <div className="sectionHeader">
      {ReasonReact.string("Public feed (realtime)")}
    </div>
    <TodoInput isPublic=true/>
    <ReasonApollo.Consumer>
      ...{
        client => {
          <GraphQLQueries.NotifyNewPublicTodosSubscription>
            ...{
              ({result}) => switch(result) {
                | Error(error) => {
                  Js.Console.error(error);
                  <div> {ReasonReact.string("Error")}</div>
                }
                | Data(data) => {
                  let todos = data##todos;
                  let latestTodoId = if (Array.length(todos) > 0) { todos[0]##id } else { 0 };
                  <TodoPublicList client={client} latestTodoId={latestTodoId}/>
                }
                | Loading => {
                  <TodoPublicList client={client} latestTodoId={0}/>
                }
              }
            }
          </GraphQLQueries.NotifyNewPublicTodosSubscription>
        }
      }
    </ReasonApollo.Consumer>
  </div>
}


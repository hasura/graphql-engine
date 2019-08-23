[@react.component]
let make = (~client) => {
  let updateMyLastSeen = () => {
    let updateLastSeenMutation = GraphQLQueries.UpdateLastSeen.make(());
    let mutation = {
      "mutation": ApolloClient.gql(. updateLastSeenMutation##query),
      "variables": updateLastSeenMutation##variables
    };
    client##mutate(mutation);
    ();
  }
  React.useEffect0(
    () => {
      let timerId = Js.Global.setInterval(updateMyLastSeen, 5000);
      Some(() => Js.Global.clearInterval(timerId));
    }
  );
  <GraphQLQueries.GetOnlineUsersSubscription>
    ...{
      ({result}) => switch(result) {
        | Loading => <div>{ReasonReact.string("Loading")}</div>
        | Error(error) => {
          Js.log(error);
          <div>{ReasonReact.string("Error")}</div>
        }
        | Data(data) => {
          let onlineUsers = Array.map(u => <UserItem user={u}/>, data##online_users);
          let onlineUsersTitle = "Online users - " ++ string_of_int(Array.length(data##online_users));
          <div className="onlineUsersWrapper">
            <div className="sliderHeader">
              {ReasonReact.string(onlineUsersTitle)}
            </div>
            {ReasonReact.array(onlineUsers)}
          </div>
        }
      }
    }
  </GraphQLQueries.GetOnlineUsersSubscription >
}
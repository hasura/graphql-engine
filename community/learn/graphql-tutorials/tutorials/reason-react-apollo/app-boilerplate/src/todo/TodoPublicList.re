let sampleTodos = [
  {
    "id": 1,
    "title": "This is public todo 1",
    "user": {
      "name": "someUser1"
    }
  },
  {
    "id": 2,
    "title": "This is public todo 2",
    "user": {
      "name": "someUser2"
    }
  },
  {
    "id": 3,
    "title": "This is public todo 3",
    "user": {
      "name": "someUser3"
    }
  },
  {
    "id": 4,
    "title": "This is public todo 4",
    "user": {
      "name": "someUser4"
    }
  },
];

[@react.component]
let make = () => {

  let todoList = List.map((t) => <FeedItem todo={t} />, sampleTodos) ;

  let newTodosBanner = {
    <div className={"loadMoreSection"}>
      {ReasonReact.string("New tasks have arrived!")}
    </div>
  };

  let oldTodosButton = {
    <div className={"loadMoreSection"}>
      {ReasonReact.string("Load older tasks")}
    </div>
  };

  <React.Fragment>
    <div className="todoListWrapper">
      {newTodosBanner}
      <ul>
        {ReasonReact.array(Array.of_list(todoList))}
      </ul>
      {oldTodosButton}
    </div>
  </React.Fragment>

}
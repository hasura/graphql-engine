class FeedFetch {
  static String fetchNewNotification = """subscription fetchNewNotification {
  todos(where: {is_public: {_eq: true}}, limit: 1, order_by: {created_at: desc}) {
    id
  }
}
""";

  static String loadMoreTodos = """ query loadMoreTodos (\$oldestTodoId: Int!) {
        todos (where: { is_public: { _eq: true}, id: {_lt: \$oldestTodoId}}, limit: 7, order_by: { created_at: desc }) {
          id
          title
          created_at
          user {
            name
          }
        }
      }""";
  static String newTodos = """query newTodos (\$latestVisibleId: Int!) {
       todos(where: { is_public: { _eq: true}, id: {_gt: \$latestVisibleId}}, order_by: { created_at: desc }) {
         id
         title
         created_at
         user {
           name
         }
       }
     }""";
  static String addPublicTodo = """mutation (\$title: String!){
    insert_todos (
      objects: [{
        title: \$title,
        is_public: true
      }]
    ){
      returning {
        id      
      }
    }
  }""";
}

class TodoFetch {
  static String fetchAll = """query getTodos(\$is_public: Boolean!) {
  todos(where: { is_public: { _eq: \$is_public} }, order_by: { created_at: desc }) {
    __typename
    id
    title
    is_completed
  }
}""";
  static String fetchActive = """query getActiveTodos{
  todos(where: {is_public: {_eq: false}, is_completed: {_eq: false}}, order_by: {created_at: desc}) {
    __typename
    is_completed
    id
    title
  }
  }""";
  static String fetchCompleted = """query getCompletedTodos{
  todos(where: {is_public: {_eq: false}, is_completed: {_eq: true}}, order_by: {created_at: desc}) {
    __typename
    is_completed
    id
    title
  }
  }""";
  static String addTodo =
      """mutation addTodo(\$title: String!, \$isPublic: Boolean!) {
  action: insert_todos(objects: { title: \$title, is_public: \$isPublic }) {
    returning {
      id
      title
      is_completed
    }
  }
}""";
  static String toggleTodo =
      """mutation toggleTodo(\$id:Int!, \$isCompleted: Boolean!) {
  action: update_todos(where: {id: {_eq: \$id}}, _set: {is_completed: \$isCompleted}) {
    returning {
      is_completed
    }
  }
}""";
  static String deleteTodo = """mutation delete(\$id:Int!) {
 action: delete_todos(where: {id: {_eq: \$id}}) {
    returning {
      id
    }
  }
}""";
}

//  This file was automatically generated and should not be edited.

import Apollo

public final class AllTodosQuery: GraphQLQuery {
  public let operationDefinition =
    "query AllTodos {\n  todos {\n    __typename\n    title\n  }\n}"

  public init() {
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["query_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("todos", type: .nonNull(.list(.nonNull(.object(Todo.selections))))),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(todos: [Todo]) {
      self.init(unsafeResultMap: ["__typename": "query_root", "todos": todos.map { (value: Todo) -> ResultMap in value.resultMap }])
    }

    /// fetch data from the table: "todos"
    public var todos: [Todo] {
      get {
        return (resultMap["todos"] as! [ResultMap]).map { (value: ResultMap) -> Todo in Todo(unsafeResultMap: value) }
      }
      set {
        resultMap.updateValue(newValue.map { (value: Todo) -> ResultMap in value.resultMap }, forKey: "todos")
      }
    }

    public struct Todo: GraphQLSelectionSet {
      public static let possibleTypes = ["todos"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("title", type: .nonNull(.scalar(String.self))),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(title: String) {
        self.init(unsafeResultMap: ["__typename": "todos", "title": title])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      public var title: String {
        get {
          return resultMap["title"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "title")
        }
      }
    }
  }
}

public final class GetMyTodosQuery: GraphQLQuery {
  public let operationDefinition =
    "query getMyTodos {\n  todos(where: {is_public: {_eq: false}}, order_by: {created_at: desc}) {\n    __typename\n    id\n    title\n    created_at\n    is_completed\n  }\n}"

  public init() {
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["query_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("todos", arguments: ["where": ["is_public": ["_eq": false]], "order_by": ["created_at": "desc"]], type: .nonNull(.list(.nonNull(.object(Todo.selections))))),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(todos: [Todo]) {
      self.init(unsafeResultMap: ["__typename": "query_root", "todos": todos.map { (value: Todo) -> ResultMap in value.resultMap }])
    }

    /// fetch data from the table: "todos"
    public var todos: [Todo] {
      get {
        return (resultMap["todos"] as! [ResultMap]).map { (value: ResultMap) -> Todo in Todo(unsafeResultMap: value) }
      }
      set {
        resultMap.updateValue(newValue.map { (value: Todo) -> ResultMap in value.resultMap }, forKey: "todos")
      }
    }

    public struct Todo: GraphQLSelectionSet {
      public static let possibleTypes = ["todos"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("id", type: .nonNull(.scalar(Int.self))),
        GraphQLField("title", type: .nonNull(.scalar(String.self))),
        GraphQLField("created_at", type: .nonNull(.scalar(String.self))),
        GraphQLField("is_completed", type: .nonNull(.scalar(Bool.self))),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(id: Int, title: String, createdAt: String, isCompleted: Bool) {
        self.init(unsafeResultMap: ["__typename": "todos", "id": id, "title": title, "created_at": createdAt, "is_completed": isCompleted])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      public var id: Int {
        get {
          return resultMap["id"]! as! Int
        }
        set {
          resultMap.updateValue(newValue, forKey: "id")
        }
      }

      public var title: String {
        get {
          return resultMap["title"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "title")
        }
      }

      public var createdAt: String {
        get {
          return resultMap["created_at"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "created_at")
        }
      }

      public var isCompleted: Bool {
        get {
          return resultMap["is_completed"]! as! Bool
        }
        set {
          resultMap.updateValue(newValue, forKey: "is_completed")
        }
      }
    }
  }
}

public final class GetNewPublicTodosQuery: GraphQLQuery {
  public let operationDefinition =
    "query getNewPublicTodos($latestVisibleId: Int) {\n  todos(where: {is_public: {_eq: true}, id: {_gt: $latestVisibleId}}, order_by: {created_at: desc}) {\n    __typename\n    id\n    title\n    created_at\n    user {\n      __typename\n      name\n    }\n  }\n}"

  public var latestVisibleId: Int?

  public init(latestVisibleId: Int? = nil) {
    self.latestVisibleId = latestVisibleId
  }

  public var variables: GraphQLMap? {
    return ["latestVisibleId": latestVisibleId]
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["query_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("todos", arguments: ["where": ["is_public": ["_eq": true], "id": ["_gt": GraphQLVariable("latestVisibleId")]], "order_by": ["created_at": "desc"]], type: .nonNull(.list(.nonNull(.object(Todo.selections))))),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(todos: [Todo]) {
      self.init(unsafeResultMap: ["__typename": "query_root", "todos": todos.map { (value: Todo) -> ResultMap in value.resultMap }])
    }

    /// fetch data from the table: "todos"
    public var todos: [Todo] {
      get {
        return (resultMap["todos"] as! [ResultMap]).map { (value: ResultMap) -> Todo in Todo(unsafeResultMap: value) }
      }
      set {
        resultMap.updateValue(newValue.map { (value: Todo) -> ResultMap in value.resultMap }, forKey: "todos")
      }
    }

    public struct Todo: GraphQLSelectionSet {
      public static let possibleTypes = ["todos"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("id", type: .nonNull(.scalar(Int.self))),
        GraphQLField("title", type: .nonNull(.scalar(String.self))),
        GraphQLField("created_at", type: .nonNull(.scalar(String.self))),
        GraphQLField("user", type: .nonNull(.object(User.selections))),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(id: Int, title: String, createdAt: String, user: User) {
        self.init(unsafeResultMap: ["__typename": "todos", "id": id, "title": title, "created_at": createdAt, "user": user.resultMap])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      public var id: Int {
        get {
          return resultMap["id"]! as! Int
        }
        set {
          resultMap.updateValue(newValue, forKey: "id")
        }
      }

      public var title: String {
        get {
          return resultMap["title"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "title")
        }
      }

      public var createdAt: String {
        get {
          return resultMap["created_at"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "created_at")
        }
      }

      /// An object relationship
      public var user: User {
        get {
          return User(unsafeResultMap: resultMap["user"]! as! ResultMap)
        }
        set {
          resultMap.updateValue(newValue.resultMap, forKey: "user")
        }
      }

      public struct User: GraphQLSelectionSet {
        public static let possibleTypes = ["users"]

        public static let selections: [GraphQLSelection] = [
          GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
          GraphQLField("name", type: .nonNull(.scalar(String.self))),
        ]

        public private(set) var resultMap: ResultMap

        public init(unsafeResultMap: ResultMap) {
          self.resultMap = unsafeResultMap
        }

        public init(name: String) {
          self.init(unsafeResultMap: ["__typename": "users", "name": name])
        }

        public var __typename: String {
          get {
            return resultMap["__typename"]! as! String
          }
          set {
            resultMap.updateValue(newValue, forKey: "__typename")
          }
        }

        public var name: String {
          get {
            return resultMap["name"]! as! String
          }
          set {
            resultMap.updateValue(newValue, forKey: "name")
          }
        }
      }
    }
  }
}

public final class GetInitialPublicTodosQuery: GraphQLQuery {
  public let operationDefinition =
    "query getInitialPublicTodos($latestVisibleId: Int) {\n  todos(where: {is_public: {_eq: true}, id: {_gt: $latestVisibleId}}, order_by: {created_at: desc}, limit: 12) {\n    __typename\n    id\n    title\n    created_at\n    user {\n      __typename\n      name\n    }\n  }\n}"

  public var latestVisibleId: Int?

  public init(latestVisibleId: Int? = nil) {
    self.latestVisibleId = latestVisibleId
  }

  public var variables: GraphQLMap? {
    return ["latestVisibleId": latestVisibleId]
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["query_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("todos", arguments: ["where": ["is_public": ["_eq": true], "id": ["_gt": GraphQLVariable("latestVisibleId")]], "order_by": ["created_at": "desc"], "limit": 12], type: .nonNull(.list(.nonNull(.object(Todo.selections))))),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(todos: [Todo]) {
      self.init(unsafeResultMap: ["__typename": "query_root", "todos": todos.map { (value: Todo) -> ResultMap in value.resultMap }])
    }

    /// fetch data from the table: "todos"
    public var todos: [Todo] {
      get {
        return (resultMap["todos"] as! [ResultMap]).map { (value: ResultMap) -> Todo in Todo(unsafeResultMap: value) }
      }
      set {
        resultMap.updateValue(newValue.map { (value: Todo) -> ResultMap in value.resultMap }, forKey: "todos")
      }
    }

    public struct Todo: GraphQLSelectionSet {
      public static let possibleTypes = ["todos"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("id", type: .nonNull(.scalar(Int.self))),
        GraphQLField("title", type: .nonNull(.scalar(String.self))),
        GraphQLField("created_at", type: .nonNull(.scalar(String.self))),
        GraphQLField("user", type: .nonNull(.object(User.selections))),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(id: Int, title: String, createdAt: String, user: User) {
        self.init(unsafeResultMap: ["__typename": "todos", "id": id, "title": title, "created_at": createdAt, "user": user.resultMap])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      public var id: Int {
        get {
          return resultMap["id"]! as! Int
        }
        set {
          resultMap.updateValue(newValue, forKey: "id")
        }
      }

      public var title: String {
        get {
          return resultMap["title"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "title")
        }
      }

      public var createdAt: String {
        get {
          return resultMap["created_at"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "created_at")
        }
      }

      /// An object relationship
      public var user: User {
        get {
          return User(unsafeResultMap: resultMap["user"]! as! ResultMap)
        }
        set {
          resultMap.updateValue(newValue.resultMap, forKey: "user")
        }
      }

      public struct User: GraphQLSelectionSet {
        public static let possibleTypes = ["users"]

        public static let selections: [GraphQLSelection] = [
          GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
          GraphQLField("name", type: .nonNull(.scalar(String.self))),
        ]

        public private(set) var resultMap: ResultMap

        public init(unsafeResultMap: ResultMap) {
          self.resultMap = unsafeResultMap
        }

        public init(name: String) {
          self.init(unsafeResultMap: ["__typename": "users", "name": name])
        }

        public var __typename: String {
          get {
            return resultMap["__typename"]! as! String
          }
          set {
            resultMap.updateValue(newValue, forKey: "__typename")
          }
        }

        public var name: String {
          get {
            return resultMap["name"]! as! String
          }
          set {
            resultMap.updateValue(newValue, forKey: "name")
          }
        }
      }
    }
  }
}

public final class GetOldPublicTodosQuery: GraphQLQuery {
  public let operationDefinition =
    "query getOldPublicTodos($oldestTodoId: Int!) {\n  todos(where: {is_public: {_eq: true}, id: {_lt: $oldestTodoId}}, limit: 7, order_by: {created_at: desc}) {\n    __typename\n    id\n    title\n    created_at\n    user {\n      __typename\n      name\n    }\n  }\n}"

  public var oldestTodoId: Int

  public init(oldestTodoId: Int) {
    self.oldestTodoId = oldestTodoId
  }

  public var variables: GraphQLMap? {
    return ["oldestTodoId": oldestTodoId]
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["query_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("todos", arguments: ["where": ["is_public": ["_eq": true], "id": ["_lt": GraphQLVariable("oldestTodoId")]], "limit": 7, "order_by": ["created_at": "desc"]], type: .nonNull(.list(.nonNull(.object(Todo.selections))))),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(todos: [Todo]) {
      self.init(unsafeResultMap: ["__typename": "query_root", "todos": todos.map { (value: Todo) -> ResultMap in value.resultMap }])
    }

    /// fetch data from the table: "todos"
    public var todos: [Todo] {
      get {
        return (resultMap["todos"] as! [ResultMap]).map { (value: ResultMap) -> Todo in Todo(unsafeResultMap: value) }
      }
      set {
        resultMap.updateValue(newValue.map { (value: Todo) -> ResultMap in value.resultMap }, forKey: "todos")
      }
    }

    public struct Todo: GraphQLSelectionSet {
      public static let possibleTypes = ["todos"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("id", type: .nonNull(.scalar(Int.self))),
        GraphQLField("title", type: .nonNull(.scalar(String.self))),
        GraphQLField("created_at", type: .nonNull(.scalar(String.self))),
        GraphQLField("user", type: .nonNull(.object(User.selections))),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(id: Int, title: String, createdAt: String, user: User) {
        self.init(unsafeResultMap: ["__typename": "todos", "id": id, "title": title, "created_at": createdAt, "user": user.resultMap])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      public var id: Int {
        get {
          return resultMap["id"]! as! Int
        }
        set {
          resultMap.updateValue(newValue, forKey: "id")
        }
      }

      public var title: String {
        get {
          return resultMap["title"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "title")
        }
      }

      public var createdAt: String {
        get {
          return resultMap["created_at"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "created_at")
        }
      }

      /// An object relationship
      public var user: User {
        get {
          return User(unsafeResultMap: resultMap["user"]! as! ResultMap)
        }
        set {
          resultMap.updateValue(newValue.resultMap, forKey: "user")
        }
      }

      public struct User: GraphQLSelectionSet {
        public static let possibleTypes = ["users"]

        public static let selections: [GraphQLSelection] = [
          GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
          GraphQLField("name", type: .nonNull(.scalar(String.self))),
        ]

        public private(set) var resultMap: ResultMap

        public init(unsafeResultMap: ResultMap) {
          self.resultMap = unsafeResultMap
        }

        public init(name: String) {
          self.init(unsafeResultMap: ["__typename": "users", "name": name])
        }

        public var __typename: String {
          get {
            return resultMap["__typename"]! as! String
          }
          set {
            resultMap.updateValue(newValue, forKey: "__typename")
          }
        }

        public var name: String {
          get {
            return resultMap["name"]! as! String
          }
          set {
            resultMap.updateValue(newValue, forKey: "name")
          }
        }
      }
    }
  }
}

public final class RemoveTodoMutation: GraphQLMutation {
  public let operationDefinition =
    "mutation removeTodo($id: Int!) {\n  delete_todos(where: {id: {_eq: $id}}) {\n    __typename\n    affected_rows\n  }\n}"

  public var id: Int

  public init(id: Int) {
    self.id = id
  }

  public var variables: GraphQLMap? {
    return ["id": id]
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["mutation_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("delete_todos", arguments: ["where": ["id": ["_eq": GraphQLVariable("id")]]], type: .object(DeleteTodo.selections)),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(deleteTodos: DeleteTodo? = nil) {
      self.init(unsafeResultMap: ["__typename": "mutation_root", "delete_todos": deleteTodos.flatMap { (value: DeleteTodo) -> ResultMap in value.resultMap }])
    }

    /// delete data from the table: "todos"
    public var deleteTodos: DeleteTodo? {
      get {
        return (resultMap["delete_todos"] as? ResultMap).flatMap { DeleteTodo(unsafeResultMap: $0) }
      }
      set {
        resultMap.updateValue(newValue?.resultMap, forKey: "delete_todos")
      }
    }

    public struct DeleteTodo: GraphQLSelectionSet {
      public static let possibleTypes = ["todos_mutation_response"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("affected_rows", type: .nonNull(.scalar(Int.self))),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(affectedRows: Int) {
        self.init(unsafeResultMap: ["__typename": "todos_mutation_response", "affected_rows": affectedRows])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      /// number of affected rows by the mutation
      public var affectedRows: Int {
        get {
          return resultMap["affected_rows"]! as! Int
        }
        set {
          resultMap.updateValue(newValue, forKey: "affected_rows")
        }
      }
    }
  }
}

public final class ToggleTodoMutation: GraphQLMutation {
  public let operationDefinition =
    "mutation toggleTodo($id: Int!, $isCompleted: Boolean!) {\n  update_todos(where: {id: {_eq: $id}}, _set: {is_completed: $isCompleted}) {\n    __typename\n    affected_rows\n  }\n}"

  public var id: Int
  public var isCompleted: Bool

  public init(id: Int, isCompleted: Bool) {
    self.id = id
    self.isCompleted = isCompleted
  }

  public var variables: GraphQLMap? {
    return ["id": id, "isCompleted": isCompleted]
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["mutation_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("update_todos", arguments: ["where": ["id": ["_eq": GraphQLVariable("id")]], "_set": ["is_completed": GraphQLVariable("isCompleted")]], type: .object(UpdateTodo.selections)),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(updateTodos: UpdateTodo? = nil) {
      self.init(unsafeResultMap: ["__typename": "mutation_root", "update_todos": updateTodos.flatMap { (value: UpdateTodo) -> ResultMap in value.resultMap }])
    }

    /// update data of the table: "todos"
    public var updateTodos: UpdateTodo? {
      get {
        return (resultMap["update_todos"] as? ResultMap).flatMap { UpdateTodo(unsafeResultMap: $0) }
      }
      set {
        resultMap.updateValue(newValue?.resultMap, forKey: "update_todos")
      }
    }

    public struct UpdateTodo: GraphQLSelectionSet {
      public static let possibleTypes = ["todos_mutation_response"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("affected_rows", type: .nonNull(.scalar(Int.self))),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(affectedRows: Int) {
        self.init(unsafeResultMap: ["__typename": "todos_mutation_response", "affected_rows": affectedRows])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      /// number of affected rows by the mutation
      public var affectedRows: Int {
        get {
          return resultMap["affected_rows"]! as! Int
        }
        set {
          resultMap.updateValue(newValue, forKey: "affected_rows")
        }
      }
    }
  }
}

public final class ClearCompletedMutation: GraphQLMutation {
  public let operationDefinition =
    "mutation clearCompleted {\n  delete_todos(where: {is_completed: {_eq: true}, is_public: {_eq: false}}) {\n    __typename\n    affected_rows\n  }\n}"

  public init() {
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["mutation_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("delete_todos", arguments: ["where": ["is_completed": ["_eq": true], "is_public": ["_eq": false]]], type: .object(DeleteTodo.selections)),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(deleteTodos: DeleteTodo? = nil) {
      self.init(unsafeResultMap: ["__typename": "mutation_root", "delete_todos": deleteTodos.flatMap { (value: DeleteTodo) -> ResultMap in value.resultMap }])
    }

    /// delete data from the table: "todos"
    public var deleteTodos: DeleteTodo? {
      get {
        return (resultMap["delete_todos"] as? ResultMap).flatMap { DeleteTodo(unsafeResultMap: $0) }
      }
      set {
        resultMap.updateValue(newValue?.resultMap, forKey: "delete_todos")
      }
    }

    public struct DeleteTodo: GraphQLSelectionSet {
      public static let possibleTypes = ["todos_mutation_response"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("affected_rows", type: .nonNull(.scalar(Int.self))),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(affectedRows: Int) {
        self.init(unsafeResultMap: ["__typename": "todos_mutation_response", "affected_rows": affectedRows])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      /// number of affected rows by the mutation
      public var affectedRows: Int {
        get {
          return resultMap["affected_rows"]! as! Int
        }
        set {
          resultMap.updateValue(newValue, forKey: "affected_rows")
        }
      }
    }
  }
}

public final class AddTodoMutation: GraphQLMutation {
  public let operationDefinition =
    "mutation addTodo($todo: String!, $isPublic: Boolean!) {\n  insert_todos(objects: {title: $todo, is_public: $isPublic}) {\n    __typename\n    affected_rows\n    returning {\n      __typename\n      id\n      title\n      created_at\n      is_completed\n    }\n  }\n}"

  public var todo: String
  public var isPublic: Bool

  public init(todo: String, isPublic: Bool) {
    self.todo = todo
    self.isPublic = isPublic
  }

  public var variables: GraphQLMap? {
    return ["todo": todo, "isPublic": isPublic]
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["mutation_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("insert_todos", arguments: ["objects": ["title": GraphQLVariable("todo"), "is_public": GraphQLVariable("isPublic")]], type: .object(InsertTodo.selections)),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(insertTodos: InsertTodo? = nil) {
      self.init(unsafeResultMap: ["__typename": "mutation_root", "insert_todos": insertTodos.flatMap { (value: InsertTodo) -> ResultMap in value.resultMap }])
    }

    /// insert data into the table: "todos"
    public var insertTodos: InsertTodo? {
      get {
        return (resultMap["insert_todos"] as? ResultMap).flatMap { InsertTodo(unsafeResultMap: $0) }
      }
      set {
        resultMap.updateValue(newValue?.resultMap, forKey: "insert_todos")
      }
    }

    public struct InsertTodo: GraphQLSelectionSet {
      public static let possibleTypes = ["todos_mutation_response"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("affected_rows", type: .nonNull(.scalar(Int.self))),
        GraphQLField("returning", type: .nonNull(.list(.nonNull(.object(Returning.selections))))),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(affectedRows: Int, returning: [Returning]) {
        self.init(unsafeResultMap: ["__typename": "todos_mutation_response", "affected_rows": affectedRows, "returning": returning.map { (value: Returning) -> ResultMap in value.resultMap }])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      /// number of affected rows by the mutation
      public var affectedRows: Int {
        get {
          return resultMap["affected_rows"]! as! Int
        }
        set {
          resultMap.updateValue(newValue, forKey: "affected_rows")
        }
      }

      /// data of the affected rows by the mutation
      public var returning: [Returning] {
        get {
          return (resultMap["returning"] as! [ResultMap]).map { (value: ResultMap) -> Returning in Returning(unsafeResultMap: value) }
        }
        set {
          resultMap.updateValue(newValue.map { (value: Returning) -> ResultMap in value.resultMap }, forKey: "returning")
        }
      }

      public struct Returning: GraphQLSelectionSet {
        public static let possibleTypes = ["todos"]

        public static let selections: [GraphQLSelection] = [
          GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
          GraphQLField("id", type: .nonNull(.scalar(Int.self))),
          GraphQLField("title", type: .nonNull(.scalar(String.self))),
          GraphQLField("created_at", type: .nonNull(.scalar(String.self))),
          GraphQLField("is_completed", type: .nonNull(.scalar(Bool.self))),
        ]

        public private(set) var resultMap: ResultMap

        public init(unsafeResultMap: ResultMap) {
          self.resultMap = unsafeResultMap
        }

        public init(id: Int, title: String, createdAt: String, isCompleted: Bool) {
          self.init(unsafeResultMap: ["__typename": "todos", "id": id, "title": title, "created_at": createdAt, "is_completed": isCompleted])
        }

        public var __typename: String {
          get {
            return resultMap["__typename"]! as! String
          }
          set {
            resultMap.updateValue(newValue, forKey: "__typename")
          }
        }

        public var id: Int {
          get {
            return resultMap["id"]! as! Int
          }
          set {
            resultMap.updateValue(newValue, forKey: "id")
          }
        }

        public var title: String {
          get {
            return resultMap["title"]! as! String
          }
          set {
            resultMap.updateValue(newValue, forKey: "title")
          }
        }

        public var createdAt: String {
          get {
            return resultMap["created_at"]! as! String
          }
          set {
            resultMap.updateValue(newValue, forKey: "created_at")
          }
        }

        public var isCompleted: Bool {
          get {
            return resultMap["is_completed"]! as! Bool
          }
          set {
            resultMap.updateValue(newValue, forKey: "is_completed")
          }
        }
      }
    }
  }
}

public final class UpdateLastSeenMutation: GraphQLMutation {
  public let operationDefinition =
    "mutation updateLastSeen($now: timestamptz!) {\n  update_users(where: {}, _set: {last_seen: $now}) {\n    __typename\n    affected_rows\n  }\n}"

  public var now: String

  public init(now: String) {
    self.now = now
  }

  public var variables: GraphQLMap? {
    return ["now": now]
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["mutation_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("update_users", arguments: ["where": [:], "_set": ["last_seen": GraphQLVariable("now")]], type: .object(UpdateUser.selections)),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(updateUsers: UpdateUser? = nil) {
      self.init(unsafeResultMap: ["__typename": "mutation_root", "update_users": updateUsers.flatMap { (value: UpdateUser) -> ResultMap in value.resultMap }])
    }

    /// update data of the table: "users"
    public var updateUsers: UpdateUser? {
      get {
        return (resultMap["update_users"] as? ResultMap).flatMap { UpdateUser(unsafeResultMap: $0) }
      }
      set {
        resultMap.updateValue(newValue?.resultMap, forKey: "update_users")
      }
    }

    public struct UpdateUser: GraphQLSelectionSet {
      public static let possibleTypes = ["users_mutation_response"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("affected_rows", type: .nonNull(.scalar(Int.self))),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(affectedRows: Int) {
        self.init(unsafeResultMap: ["__typename": "users_mutation_response", "affected_rows": affectedRows])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      /// number of affected rows by the mutation
      public var affectedRows: Int {
        get {
          return resultMap["affected_rows"]! as! Int
        }
        set {
          resultMap.updateValue(newValue, forKey: "affected_rows")
        }
      }
    }
  }
}

public final class NotifyNewPublicTodosSubscription: GraphQLSubscription {
  public let operationDefinition =
    "subscription NotifyNewPublicTodos {\n  todos(where: {is_public: {_eq: true}}, limit: 1, order_by: {created_at: desc}) {\n    __typename\n    id\n    created_at\n  }\n}"

  public init() {
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["subscription_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("todos", arguments: ["where": ["is_public": ["_eq": true]], "limit": 1, "order_by": ["created_at": "desc"]], type: .nonNull(.list(.nonNull(.object(Todo.selections))))),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(todos: [Todo]) {
      self.init(unsafeResultMap: ["__typename": "subscription_root", "todos": todos.map { (value: Todo) -> ResultMap in value.resultMap }])
    }

    /// fetch data from the table: "todos"
    public var todos: [Todo] {
      get {
        return (resultMap["todos"] as! [ResultMap]).map { (value: ResultMap) -> Todo in Todo(unsafeResultMap: value) }
      }
      set {
        resultMap.updateValue(newValue.map { (value: Todo) -> ResultMap in value.resultMap }, forKey: "todos")
      }
    }

    public struct Todo: GraphQLSelectionSet {
      public static let possibleTypes = ["todos"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("id", type: .nonNull(.scalar(Int.self))),
        GraphQLField("created_at", type: .nonNull(.scalar(String.self))),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(id: Int, createdAt: String) {
        self.init(unsafeResultMap: ["__typename": "todos", "id": id, "created_at": createdAt])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      public var id: Int {
        get {
          return resultMap["id"]! as! Int
        }
        set {
          resultMap.updateValue(newValue, forKey: "id")
        }
      }

      public var createdAt: String {
        get {
          return resultMap["created_at"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "created_at")
        }
      }
    }
  }
}

public final class GetOnlineUsersSubscription: GraphQLSubscription {
  public let operationDefinition =
    "subscription GetOnlineUsers {\n  online_users(order_by: {user: {name: asc}}) {\n    __typename\n    id\n    user {\n      __typename\n      name\n    }\n  }\n}"

  public init() {
  }

  public struct Data: GraphQLSelectionSet {
    public static let possibleTypes = ["subscription_root"]

    public static let selections: [GraphQLSelection] = [
      GraphQLField("online_users", arguments: ["order_by": ["user": ["name": "asc"]]], type: .nonNull(.list(.nonNull(.object(OnlineUser.selections))))),
    ]

    public private(set) var resultMap: ResultMap

    public init(unsafeResultMap: ResultMap) {
      self.resultMap = unsafeResultMap
    }

    public init(onlineUsers: [OnlineUser]) {
      self.init(unsafeResultMap: ["__typename": "subscription_root", "online_users": onlineUsers.map { (value: OnlineUser) -> ResultMap in value.resultMap }])
    }

    /// fetch data from the table: "online_users"
    public var onlineUsers: [OnlineUser] {
      get {
        return (resultMap["online_users"] as! [ResultMap]).map { (value: ResultMap) -> OnlineUser in OnlineUser(unsafeResultMap: value) }
      }
      set {
        resultMap.updateValue(newValue.map { (value: OnlineUser) -> ResultMap in value.resultMap }, forKey: "online_users")
      }
    }

    public struct OnlineUser: GraphQLSelectionSet {
      public static let possibleTypes = ["online_users"]

      public static let selections: [GraphQLSelection] = [
        GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
        GraphQLField("id", type: .scalar(String.self)),
        GraphQLField("user", type: .object(User.selections)),
      ]

      public private(set) var resultMap: ResultMap

      public init(unsafeResultMap: ResultMap) {
        self.resultMap = unsafeResultMap
      }

      public init(id: String? = nil, user: User? = nil) {
        self.init(unsafeResultMap: ["__typename": "online_users", "id": id, "user": user.flatMap { (value: User) -> ResultMap in value.resultMap }])
      }

      public var __typename: String {
        get {
          return resultMap["__typename"]! as! String
        }
        set {
          resultMap.updateValue(newValue, forKey: "__typename")
        }
      }

      public var id: String? {
        get {
          return resultMap["id"] as? String
        }
        set {
          resultMap.updateValue(newValue, forKey: "id")
        }
      }

      /// An object relationship
      public var user: User? {
        get {
          return (resultMap["user"] as? ResultMap).flatMap { User(unsafeResultMap: $0) }
        }
        set {
          resultMap.updateValue(newValue?.resultMap, forKey: "user")
        }
      }

      public struct User: GraphQLSelectionSet {
        public static let possibleTypes = ["users"]

        public static let selections: [GraphQLSelection] = [
          GraphQLField("__typename", type: .nonNull(.scalar(String.self))),
          GraphQLField("name", type: .nonNull(.scalar(String.self))),
        ]

        public private(set) var resultMap: ResultMap

        public init(unsafeResultMap: ResultMap) {
          self.resultMap = unsafeResultMap
        }

        public init(name: String) {
          self.init(unsafeResultMap: ["__typename": "users", "name": name])
        }

        public var __typename: String {
          get {
            return resultMap["__typename"]! as! String
          }
          set {
            resultMap.updateValue(newValue, forKey: "__typename")
          }
        }

        public var name: String {
          get {
            return resultMap["name"]! as! String
          }
          set {
            resultMap.updateValue(newValue, forKey: "name")
          }
        }
      }
    }
  }
}
---
title: "Query Variables"
metaTitle: "Passing GraphQL Variables in Queries | GraphQL iOS Apollo Tutorial"
metaDescription: "An Example of passing variables in GraphQL context and usage of Apollo GraphQL Mutation variables in iOS app."
---

import GithubLink from '../../src/GithubLink.js'

What is a variable in GraphQL context?
-------------------------------------
GraphQL has a first-class way to factor dynamic values out of the query, and pass them as a separate dictionary. These values are called variables. In our case, we are defining the object to be inserted as a mutation.

So let's define the graphql mutation to be used.

Open `Todo/TodoVC.swift` and add the following code at the end of file:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/ios-apollo/app-final/Todo/Todovc.swift" text="Todo/Todovc.swift" />

```swift
// Add Todo to Cloud
    private func addTodoMutationCloud(title: String){
        apollo.perform(mutation: AddTodoMutation(todo: title, isPublic: false)) { (result, error) in
            guard let data = result?.data else { return }
            let newTodo = data.insertTodos!.returning.compactMap({GetMyTodosQuery.Data.Todo(id: $0.id, title: $0.title, createdAt: $0.createdAt, isCompleted: $0.isCompleted)})
            self.todos.insert(contentsOf: newTodo, at: 0)
            self.filteredTodos.insert(contentsOf: newTodo, at: 0)
            
            // Update view
            DispatchQueue.main.async {
                let indexPaths = (0 ..< newTodo.count).map { IndexPath(row: $0, section: 0) }
                self.todoTable.beginUpdates()
                self.todoTable.insertRows(at: indexPaths , with: .automatic)
                self.todoTable.endUpdates()
            }
            
        }
    }
```

What does this mutation do?
---------------------------
The mutation inserts into `todos` table with the $objects variable being passed as one todo type.

Awesome! We have defined our first graphql mutation.
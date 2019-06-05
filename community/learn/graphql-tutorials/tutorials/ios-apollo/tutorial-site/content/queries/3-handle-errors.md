---
title: "Handle errors"
metaTitle: "Apollo  Error Handling | GraphQL iOS Apollo Tutorial"
metaDescription: "We will handle the GraphQL loading and error states in iOS app using the props - loading and error that Apollo returned "
---

As we saw in the previous step, Apollo returned `error` and `results` object in completion handler. 

Now let's go back to the `todoQueryCloud` function that you wrote in the previous step.

```swift

  private func todoQueryCloud(){
        apollo.fetch(query: GetMyTodosQuery()){ (result, error) in
            if ((error) != nil) {
                if SessionManager.shared.logout() {
                    self.performSegue(withIdentifier: "loginVC", sender: self)
                }
                return
            }
            guard let data = result?.data else { return }
            self.todos = data.todos
            self.filteredTodos = data.todos
            DispatchQueue.main.async {
                self.setupUI()
                self.todoTable.reloadData()
            }
        }
    }

```

When this function is executed in the `viewWillAppear`, we handle the completion of it from the `error` or `result` objects.

### Error state in Apollo iOS
Now, the query could also end up in an `error` state due to various reasons. Sometimes the graphql query could be wrong, or the server isn't responding. Whatever may be the reason, the user facing UI should show something to convey that an error has occurred. Here we are checking if there was a error, was that because of an invalid token? And if so, we trigger logout and send the user to login screen.
In this error state, typically you can send these error messages to third-party services to track what went wrong.

All said and done, these are two important states that need to be handled inside your component. What you have written above is basic, but sufficient for this tutorial.

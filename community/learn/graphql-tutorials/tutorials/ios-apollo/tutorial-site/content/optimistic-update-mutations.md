---
title: "Optimistic UI updates after mutations"
metaTitle: "Optimistic UI updates after mutations | GraphQL iOS Apollo Tutorial"
metaDescription: "We will use the Apollo Client Optimistic Response to perform UI updates after a GraphQL mutation in the iOS app. It uses apollo.watcher for queries"
---

We can notice that there is a lag when users create a todo.
We can also create UIs where the UI updates optimistically, assuming
that the mutation will be successful.

To enable toggling between completed states, and to delete todos let's
use optimistic updates when we run mutations!

We will learn the following concepts:

- Creating a GraphQL mutation
- Using the apollo.watcher for queries
- Capturing finished/error states

Let's get started!

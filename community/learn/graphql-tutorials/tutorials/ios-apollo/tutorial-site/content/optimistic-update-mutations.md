---
title: "Optimistic UI updates after mutations"
---

We can notice that there is a lag when users create a todo.
We can also create UIs where the UI updates optimistically, assuming
that the mutation will be successful.

To enable toggling between completed states, and to delete todos let's
use optimistic updates when we run mutations!

We will learn the following concepts:

- Creating a GraphQL mutation
- Using the apollo.wachter for queries
- Capturing finished/error states

Let's get started!

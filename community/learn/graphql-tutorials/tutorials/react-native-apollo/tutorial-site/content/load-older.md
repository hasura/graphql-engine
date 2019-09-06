---
title: "Loading older todos"
metaTitle: "Loading older todos | GraphQL React Native Apollo Tutorial"
metaDescription: "You will learn how to load older todos added by other people to see an example of pagination in GraphQL."
---

It might not be always ideal to load all the todos in the database. You would want to have some kind of pagination to enforce fetching data incrementally.

In this app, we will implement the basic functionality of loading older todos on a button click.

The ideal workflow to implement this feature would be:

1. Start with loading only "n" items
2. On "load more", fetch "n" more items older than the oldest local item
3. Append the newly loaded items to apollo cache

You will learn:

1. Ideas behind implementing pagination in GraphQL
2. To update apollo cache whenever required

Lets do it.
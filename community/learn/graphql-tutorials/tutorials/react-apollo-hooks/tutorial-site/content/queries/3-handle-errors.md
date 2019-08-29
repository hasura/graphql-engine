---
title: "Handle loading/errors"
metaTitle: "Apollo useQuery React hook Error Handling | GraphQL React Apollo Hooks Tutorial"
metaDescription: "We will handle the GraphQL loading and error states in React app using the Apollo useQuery React hook properties - loading and error "
---

As we saw in the previous step, Apollo returned a result object with properties . Among them `loading` and `error` are common ones that you will need to handle in your app.

Now let's go back to the `useQuery` React hook that you wrote in the previous step.

```javascript

  const TodoPrivateListQuery = () => {
  const { loading, error, data } = useQuery(GET_MY_TODOS);

  if (loading) {
    return <div>Loading...</div>;
  }
  if (error) {
    console.error(error);
    return <div>Error!</div>;
  }
  return <TodoPrivateList todos={data.todos} />;
};

```

### Apollo Query Loading State
When this component mounts, the GraphQL query sent in the background may not have been completed. But we need to handle that temporary state of no data and hence we return some useful text during `loading` state. 
In this loading state, typically you can do fancy things like displaying a loading spinner.

### Apollo Query Error State
Now, the query could also end up in an `error` state due to various reasons. Sometimes the graphql query could be wrong, or the server isn't responding. Whatever may be the reason, the user facing UI should show something to convey that an error has occurred. 
In this error state, typically you can send these error messages to third-party services to track what went wrong.

All said and done, these are two important states that need to be handled inside your component. What you have written above is basic, but sufficient for this tutorial.

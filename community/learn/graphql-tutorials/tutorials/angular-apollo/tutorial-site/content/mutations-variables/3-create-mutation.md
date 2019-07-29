---
title: "Run mutation, update cache"
metaTitle: "Apollo Mutate Method | GraphQL Angular Apollo Tutorial"
metaDescription: "We will use the Apollo Client Mutate method from apollo-angular in Angular app as an example to insert new data and update cache locally using readQuery and writeQuery."
---

### Apollo Angular Mutate Method 
Now let's do the integration part. Open `src/app/Todo/TodoInput.ts` and add the following code below the other imports:

```typescript
import { Apollo } from 'apollo-angular';
```

`Apollo` is being imported from `apollo-angular` and the graphql query we defined above to fetch the todo data.

Now, we will add the mutate method passing our graphql mutation constant that we imported. Add the following code:

```typescript
export class TodoInput {
    @Input('isPublic') isPublic: any = false;
  + todoInput: any= '';
  + loading: boolean = true;
    
  + constructor(private apollo: Apollo) {}

    addTodo(e) {
      e.preventDefault();
  +    this.apollo.mutate({
  +      mutation: ADD_TODO,
  +      variables: {
  +        todo: this.todoInput,
  +        isPublic: this.isPublic 
  +      },
  +    }).subscribe(({ data, loading }) => {
  +      this.loading = loading;
  +      this.todoInput = '';
  +    },(error) => {
  +      console.log('there was an error sending the query', error);
  +    });
    }
}
```

The mutate function optionally takes variables, optimisticResponse, refetchQueries, and update; You are going to make use of the `update` function later.

Now let's handle the form submit and input value to invoke the mutation.

```html
  <form class="formInput" (submit)="addTodo($event)">
    <input 
      class="input"
      placeholder="What needs to be done?"
+      [(ngModel)]="todoInput"
+      [ngModelOptions]="{standalone: true}" 
    />
    <i class="inputMarker fa fa-angle-right"></i>
  </form>
```

The mutation has been integrated and the new todos will be inserted into the database. But the UI doesn't know that a new todo has been added. We need a way to tell Apollo Client to update the query for the list of todos.

### Apollo Angular Mutation Update
The `update` function comes in handy to update the cache for this mutation. It comes with utility functions such as `readQuery` and `writeQuery` that helps in reading from and writing to the cache.

Let's implement `update` for the above mutation.

We need to fetch the current list of todos from the cache. So let's import the query that we used in the previous steps.

```typescript
import {GET_MY_TODOS} from './TodoPrivateList';
```

Let's define the update function to read and write to cache.

```typescript
addTodo(e) {
      e.preventDefault();
      this.apollo.mutate({
        mutation: ADD_TODO,
        variables: {
          todo: this.todoInput,
          isPublic: this.isPublic 
        },
      +  update: (cache, {data}) => {
          
      +    if(this.isPublic) return null;
      +    
      +    const existingTodos : any = cache.readQuery({
      +      query: GET_MY_TODOS
      +    });

      +    const newTodo = data.insert_todos.returning[0];
      +    cache.writeQuery({
      +      query: GET_MY_TODOS,
      +      data: {todos: [newTodo, ...existingTodos.todos]}
      +    });
        },
        
      }).subscribe(({ data, loading }) => {
        this.loading = loading;
        this.todoInput = '';
        console.log('got data ', data);
      },(error) => {
        console.log('there was an error sending the query', error);
      });
    }
```

Let's dissect what's happening in this code snippet.

Our goals were simple:

- Make a mutation to insert the new todo in the database.
- Once the mutation is done, we need to update the cache to update the UI.

The update function is used to update the cache after a mutation occurs.
It receives the result of the mutation (data) and the current cache (store) as arguments. You will then use these arguments to manage your cache so that the UI will be up to date.

### readQuery and writeQuery

cache.readQuery
---------------

Unlike `client.query`, readQuery will never make a request to your GraphQL server. It will always read from the cache. So we make a read request to the cache to get the current list of todos.

cache.writeQuery
----------------

We have already done the mutation to the graphql server using the mutate function. Our goal was to update the UI. This is where writeQuery comes to the rescue. writeQuery will allow you to change data in your local cache, but it is important to remember that they will not change any data on your server (exactly what we need).

  Any subscriber to the Apollo Client store will instantly see this update and render new UI accordingly.

We concatenate our new todo from our mutation with the list of existing todos and write the query back to the cache with cache.writeQuery

Now, the TodoPrivateList component will get the updated todo list as it is automatically subscribed to the store.

Great! That was actually easy :)
<template>
  <div>
    <div class="todoListwrapper">
      <div class="loadMoreSection" v-if="newTodosCount" v-on:click="loadMoreClicked">
        New tasks have arrived! ({{ newTodosCount }})
      </div>
      <TodoItem 
        v-bind:todos="todos" 
        v-bind:type="type" 
      />
      <div class="loadMoreSection" v-if="olderTodosAvailable" v-on:click="loadOlderClicked">
        Load older tasks
      </div>
    </div>
  </div>
</template>

<script>
import TodoItem from "../components/TodoItem";
import TodoFilters from "../components/TodoFilters";
import { GET_NEW_PUBLIC_TODOS, GET_OLD_PUBLIC_TODOS, NOTIFY_NEW_PUBLIC_TODOS } from "../TodoQueries";
export default {
  components: {
    TodoItem, TodoFilters
  },
  data: function() {
    return {
      olderTodosAvailable: true,
      newTodosCount: 0,
      limit: 7,
      todos: [],
      type: "public"
    }
  },
  mounted() {
    const client = this.$apolloProvider.clients.defaultClient;
    const that = this;
    client
      .query({
        query: GET_OLD_PUBLIC_TODOS
      })
      .then(data => {
        this.todos = data.data.todos;
        // start a subscription
        client
          .subscribe({
            query: NOTIFY_NEW_PUBLIC_TODOS,
          })
          .subscribe({
            next(data) {
              if (data.data.todos.length) {
                // check if the received todo is already present
                if(data.data.todos[0].id !== that.todos[0].id) {
                  that.newTodosCount = that.newTodosCount + data.data.todos.length;
                }
              }
            },
            error(err) {
              console.error("err", err);
            }
          });
      });
  },
  methods: {
    loadMoreClicked: function() {
      const client = this.$apolloProvider.clients.defaultClient;
      this.newTodosCount = 0;
      client
        .query({
          query: GET_NEW_PUBLIC_TODOS,
          variables: {
            latestVisibleId: this.todos.length ? this.todos[0].id : null
          }
        })
        .then(data => {
          if (data.data.todos.length) {
            const mergedTodos = data.data.todos.concat(this.todos);
            // update state with new todos
            this.todos = mergedTodos;
          }
        });
    },
    loadOlderClicked: function() {
      const client = this.$apolloProvider.clients.defaultClient;
      client
        .query({
          query: GET_OLD_PUBLIC_TODOS,
          variables: {
            oldestTodoId: this.todos.length
              ? this.todos[this.todos.length - 1].id
              : null
          }
        })
        .then(data => {
          if (data.data.todos.length) {
            const mergedTodos = this.todos.concat(data.data.todos);
            // update state with new todos
            this.todos = mergedTodos;
          } else {
            this.olderTodosAvailable = false;
          }
        });
    },
  }
}
</script>

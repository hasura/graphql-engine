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
import gql from "graphql-tag";
const NOTIFY_NEW_PUBLIC_TODOS = gql`
  subscription notifyNewPublicTodos {
    todos(
      where: { is_public: { _eq: true }} 
      order_by: { created_at: desc }
      limit: 1
    ) {
      id
      title
      created_at
    }
  }
`;
const GET_OLD_PUBLIC_TODOS = gql`
  query getOldPublicTodos($oldestTodoId: Int) {
    todos(
      where: { is_public: { _eq: true }, id: { _lt: $oldestTodoId } }
      limit: 7
      order_by: { created_at: desc }
    ) {
      id
      title
      created_at
      is_public
      user {
        name
      }
    }
  }
`;
const GET_NEW_PUBLIC_TODOS = gql`
  query getNewPublicTodos($latestVisibleId: Int!) {
    todos(
      where: { is_public: { _eq: true }, id: { _gt: $latestVisibleId } }
      order_by: { created_at: desc }
    ) {
      id
      title
      created_at
      is_public
      user {
        name
      }
    }
  }
`;
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
    const that = this;
    this.$apollo
      .query({
        query: GET_OLD_PUBLIC_TODOS
      })
      .then(data => {
        this.todos = data.data.todos;
        // start a subscription
        this.$apollo
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
      this.newTodosCount = 0;
      this.$apollo
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
      this.$apollo
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

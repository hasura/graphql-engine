<template>
  <div>
    <div v-if="$apollo.queries.todos.loading">Loading...</div>
    <div v-if="error">{{ error }}</div>
    <div class="todoListwrapper">
      <TodoItem 
        v-bind:todos="filteredTodos" 
        v-bind:type="type" 
      />
    </div>
    <TodoFilters 
      v-bind:remainingTodos="remainingTodos" 
      v-bind:filterResults="filterResults" 
      v-bind:filterType="filterType"
      v-bind:type="type"
      v-bind:clearCompleted="clearCompleted"
    />
  </div>
</template>

<script>
import TodoItem from "../components/TodoItem";
import TodoFilters from "../components/TodoFilters";
import gql from "graphql-tag";
export const GET_MY_TODOS = gql`
  query getMyTodos {
    todos(
      where: { is_public: { _eq: false } }
      order_by: { created_at: desc }
    ) {
      id
      title
      created_at
      is_completed
    }
  }
`;
export default {
  components: {
    TodoItem, TodoFilters
  },
  data() {
    return {
      type: "private",
      filterType: "all",
      todos: [],
      error: null
    }
  },
  computed: {
    remainingTodos: function() {
      const activeTodos = this.todos !== undefined ? this.todos.filter((todo) => todo.is_completed !== true) : []
      return activeTodos.length
    },
    filteredTodos: function() {
      if (this.filterType === 'all') {
        return this.todos
      } else if(this.filterType === 'active') {
        return this.todos.filter((todo) => todo.is_completed !== true);
      } else if (this.filterType === 'completed') {
        return this.todos.filter((todo) => todo.is_completed === true);
      }
    }
  },
  methods: {
    filterResults: function(type) {
      if(type === 'active') {
        this.filterType = "active";
      } else if(type === 'completed') {
        this.filterType = "completed";
      } else {
        this.filterType = "all";
      }
    },
    clearCompleted: function() {
      const isOk = window.confirm("Are you sure?");
      if (isOk) {
        // Remove all the todos that are completed
        const CLEAR_COMPLETED = gql`
          mutation clearCompleted {
            delete_todos(where: {is_completed: {_eq: true}, is_public: {_eq: false}}) {
              affected_rows
            }
          }
        `;
        this.$apollo
          .mutate({
            mutation: CLEAR_COMPLETED,
            update: (store, { data: { delete_todos } }) => {
              if (delete_todos.affected_rows) {
                const data = store.readQuery({
                  query: GET_MY_TODOS,
                });
                data.todos = data.todos.filter((todo) => todo.is_completed !== true);
                store.writeQuery({
                  query: GET_MY_TODOS,
                  data
                });
              }
            },
          })
          .catch(error => {
            console.error(error);
          });
      }
    }
  },
  apollo: {
    todos: {
      // graphql query
      query: GET_MY_TODOS,
      error(error) {
        this.error = JSON.stringify(error.message);
      }
    },
  },
}
</script>

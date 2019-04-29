<template>
  <ul>
    <li v-for="todo in todos" v-bind:key="todo.id">
      <div v-if="todo.is_public" class="userInfoPublic">
        @{{ todo.user.name }}
      </div>
      <div class="view" v-if="type === 'private'">
          <div v-if="todo.is_completed" class="round">
            <input
              type="checkbox"
              id="todo.id"
              checked="true"
            />
            <label 
              v-on:click="handleTodoToggle(todo)"
              htmlFor="todo.id"></label>
          </div>
          <div v-else class="round">
            <input
              type="checkbox"
              id="todo.id"
            />
            <label 
              v-on:click="handleTodoToggle(todo)"
              htmlFor="todo.id"></label>
          </div>
      </div>
      <div class="labelContent">
          <strike class="todoLabel" v-if="todo.is_completed">
            <div>
              {{ todo.title }}
            </div>
          </strike>
          <div v-else>
            {{ todo.title }}
          </div>
      </div>
      <button v-if="type === 'private'" v-on:click="handleTodoDelete(todo)" class="closeBtn"> x </button>
    </li>
  </ul>
</template>

<script>
  import gql from 'graphql-tag';
  import { GET_MY_TODOS } from "./TodoPrivateList.vue";
  const TOGGLE_TODO = gql`
    mutation update_todos($id: Int!, $isCompleted: Boolean!) {
      update_todos(where: { id: { _eq: $id } }, _set: { is_completed: $isCompleted }) {
        affected_rows
      }
    }
  `;
  const REMOVE_TODO = gql`
    mutation delete_todos($id: Int!) {
      delete_todos(where: { id: { _eq: $id } }) {
        affected_rows
      }
    }
  `;
  export default {
    props: ['todos', 'type'],
    methods: {
      handleTodoToggle: function (todo) {
        // update todo data in db here
        this.$apollo.mutate({
          mutation: TOGGLE_TODO,
          variables: {
           id: todo.id,
           isCompleted: !todo.is_completed
          },
          update: (store, { data: { update_todos } }) => {
            if (update_todos.affected_rows) {
              if (this.type === "private") {
                // Read the data from our cache for this query.
                const data = store.readQuery({
                  query: GET_MY_TODOS,
                });
                const toggledTodo = data.todos.find(t => t.id === todo.id);
                toggledTodo.is_completed = !todo.is_completed;
                store.writeQuery({
                  query: GET_MY_TODOS,
                  data
                });
              } 
            }
          },
          optimisticResponse: {
            __typename: 'Mutation',
            update_todos: {
              __typename: 'todos',
              id: todo.id,
              is_completed: !todo.is_completed,
              affected_rows: 1
            },
          }
        });
      },
      handleTodoDelete: function(todo) {
        // delete todo from db
        this.$apollo.mutate({
          mutation: REMOVE_TODO,
          variables: {
           id: todo.id,
          },
          update: (store, { data: { delete_todos } }) => {
            if (delete_todos.affected_rows) {
              if (this.type === "private") {
                const data = store.readQuery({
                  query: GET_MY_TODOS
                });
                data.todos = data.todos.filter(t => {
                  return t.id !== todo.id;
                });
                store.writeQuery({
                  query: GET_MY_TODOS,
                  data
                });
              }
            }
          },
        });
      }
    }
  }

</script>

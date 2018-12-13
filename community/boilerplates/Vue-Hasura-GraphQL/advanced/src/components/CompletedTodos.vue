<template>
  <div class="row">
    <div class="col-sm-10 offset-sm-1">
      <table class="table table-hover table-success" v-if="todos.length > 0">
        <thead>
          <tr>
            <th class="table-dark" colspan="4" style="text-align:center">COMPLETED TODOS</th>
          </tr>
          <tr>
            <th scope="col" style="text-align:center;">Description</th>
            <th scope="col" style="text-align:center;">Created At</th>
            <th scope="col" style="text-align:center;">Updated At</th>
            <th scope="col" style="text-align:center;"> Actions </th>
          </tr>
        </thead>
        <tbody>
          <tr
            v-for="todo in todos"
            :key="todo.id">
            <td align="center">{{ todo.text }}</td>
            <td align="center">{{ todo.created_at | moment("MM/DD/YYYY hh:mm A") }}</td>
            <td align="center" v-if="todo.updated_at">{{ todo.updated_at | moment("MM/DD/YYYY hh:mm A") }}</td>
            <td align="center" v-else>N/A</td>
            <td align="center">
              <b-button size="sm" variant="danger" @click="deleteTodo(todo.id)">
                Delete
              </b-button>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
</template>

<script>

import { ALL_COMPLETED_TODOS, DELETE_TODO } from '@/graphql'

export default {
  name: 'CompletedTodos',
  data () {
    return {
      todos: []
    }
  },
  methods: {
    deleteTodo (todoId) {
      this.$apollo
        .mutate({
          mutation: DELETE_TODO,
          variables: {
            id: todoId
          }
        })
        .then(response => {
          // do nothing
        })
    }
  },
  apollo: {
    $subscribe: {
      todosQuery: {
        query: ALL_COMPLETED_TODOS,
        result (data) {
          this.todos = data.data.todos
        }
      }
    }
  }
}

</script>

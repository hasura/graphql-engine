<template>
  <div class="row">
    <div class="col-sm-10 offset-sm-1">
      <table class="table table-hover table-warning table-striped" v-if="todos.length > 0">
        <thead>
          <tr>
            <th class="table-dark" colspan="4" style="text-align:center">PENDING TODOS</th>
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
              <b-button size="sm" variant="success" @click="updateTodo(todo.id)">
                Mark as done
              </b-button>
              <b-button size="sm" variant="danger" @click="deleteTodo(todo.id)">
                Delete
              </b-button>
            </td>
          </tr>
        </tbody>
      </table>
      <table class="table table-hover table-warning table-striped" v-else>
        <thead>
          <tr>
            <th class="table-dark" colspan="4" style="text-align:center">Horray! No pending tasks. grab a cup of coffee and enjoy!</th>
          </tr>
        </thead>
      </table>
    </div>
  </div>
</template>

<script>

import { ALL_PENDING_TODOS, UPDATE_TODO, DELETE_TODO } from '@/graphql'

export default {
  name: 'PendingTodos',
  data () {
    return {
      todos: []
    }
  },
  methods: {
    updateTodo (id) {
      var update = new Date()
      update = update.toISOString()
      this.$apollo
        .mutate({
          mutation: UPDATE_TODO,
          variables: {
            id: id,
            updated: update
          }
        })
        .then(response => {
          // do nothing
        })
    },
    deleteTodo (id) {
      this.$apollo
        .mutate({
          mutation: DELETE_TODO,
          variables: {
            id: id
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
        query: ALL_PENDING_TODOS,
        result (data) {
          this.todos = data.data.todos
        }
      }
    }
  }
}

</script>

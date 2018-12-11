<template>
  <section class="section">
    <div class="container">
      <div class="columns">
        <div class="column is-9">
            <h2 class="title">Todos</h2>

          <table class="table is-striped is-narrow is-hoverable is-fullwidth">
            <thead>
              <tr>
                <th>ID</th>
                <th>Text</th>
                <th>Is Completed</th>
              </tr>
            </thead>
            <tbody>
              <tr
                v-for="todo in todos"
                :key="todo.id">
                    <td>{{ todo.id }}</td>
                    <td>{{ todo.text }}</td>
                    <td>{{ todo.is_completed }}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
  </section>
</template>

<script>
import { TODOS_QUERY } from '@/graphql'

export default {
  name: 'AllTodos',
  data () {
    return {
      todos: []
    }
  },
  apollo: {
    /**
      fetch all todos. (This is an example to fetch. This will work only if
      you have respective table and graphql client setup). Visit basic boilerplate
      for more info https://github.com/pradeepgangwar/Vue-Graphql/tree/master/basic
    */
    todos: {
      query: TODOS_QUERY
    }
  }
}
</script>

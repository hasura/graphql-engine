<template>
  <div class="container">
    <new-todo></new-todo>
    <br>
    <pending-todos></pending-todos>
    <br>
    <completed-todos></completed-todos>
  </div>
</template>

<script>

import { GET_USER_BY_ID } from '@/graphql'
import NewTodo from './NewTodo'
import PendingTodos from './PendingTodos'
import CompletedTodos from './CompletedTodos'

export default {
  name: 'Dashboard',
  data () {
    return {
      users: [],
      id: this.$route.params.id
    }
  },
  components: {
    NewTodo,
    PendingTodos,
    CompletedTodos
  },
  apollo: {
    users: {
      query: GET_USER_BY_ID,
      variables () {
        return {
          userid: this.id
        }
      }
    }
  }
}

</script>

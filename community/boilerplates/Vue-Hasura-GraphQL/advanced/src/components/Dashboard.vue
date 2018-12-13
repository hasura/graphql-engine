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

import NewTodo from './NewTodo'
import PendingTodos from './PendingTodos'
import CompletedTodos from './CompletedTodos'

export default {
  name: 'Dashboard',
  props: ['auth'],
  data () {
    this.auth.handleAuthentication()
    return {}
  },
  components: {
    NewTodo,
    PendingTodos,
    CompletedTodos
  }
}

</script>

<style>
  .spinner {
    position: absolute;
    display: flex;
    justify-content: center;
    height: 100vh;
    width: 100vw;
    background-color: white;
    top: 0;
    bottom: 0

;
    left: 0;
    right: 0;
  }
</style>

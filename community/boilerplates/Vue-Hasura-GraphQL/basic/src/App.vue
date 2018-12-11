<template>
  <div id="app">
    <nav-bar></nav-bar>
    <br>
    <router-view></router-view>
  </div>
</template>

<script>

import AddUser from './components/AddUser'
import PendingTodos from './components/PendingTodos'
import NavBar from './components/NavBar'

export default {
  name: 'app',
  components: {
    AddUser,
    PendingTodos,
    NavBar
  }
}
</script>

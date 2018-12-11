<template>
  <div id="app">
    <nav class="navbar is-primary" role="navigation" aria-label="main navigation">
      <div class="container">
        <div class="navbar-brand">
          <router-link class="navbar-item" to="/"> Todo App</router-link>

          <button class="button navbar-burger">
            <span></span>
            <span></span>
            <span></span>
          </button>
        </div>
      </div>
    </nav>
    <router-view/>
    <all-todos></all-todos>
  </div>
</template>

<script>
import AllTodos from './components/AllTodos'

export default {
  name: 'app',
  components: {
    AllTodos
  }
}
</script>

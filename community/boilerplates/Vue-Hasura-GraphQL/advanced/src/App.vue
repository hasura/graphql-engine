<template>
  <div id="app">
    <nav class="navbar navbar-expand-lg navbar-light bg-light">
      <a class="navbar-brand" href="#">ToDo Application</a>
      <div>
        <b-button size="sm" variant="primary" v-if="!authenticated" @click="login()">
          Login
        </b-button>
        <b-button size="sm" variant="primary" v-if="authenticated" @click="logout()">
          Logout
        </b-button>
      </div>
    </nav>
    <br>
    <router-view
      :auth="auth"
      :authenticated="authenticated">
    </router-view>
  </div>
</template>

<script>

import PendingTodos from './components/PendingTodos'
import AuthService from './Auth/AuthService'
const auth = new AuthService()
const { login, logout, authenticated, authNotifier } = auth

export default {
  name: 'app',
  data () {
    authNotifier.on('authChange', authState => {
      this.authenticated = authState.authenticated
    })
    return {
      auth,
      authenticated
    }
  },
  methods: {
    login,
    logout
  },
  components: {
    PendingTodos
  }
}

</script>

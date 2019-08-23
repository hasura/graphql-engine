<template>
  <div class="overlay">
    <div class="overlay-content">
      <div class="overlay-heading">
        Welcome to the GraphQL tutorial app
      </div>
      <div class="overlay-message">
        Please login to continue
      </div>
      <div class="overlay-action">
        <button v-on:click="handleLogin" class="login-btn">Log in</button>
      </div>
    </div>

  </div>
</template>



<script>
export default {
  name: "home",
  methods: {
    handleLoginEvent(data) {
      this.isAuthenticated = data.loggedIn;
      this.$router.push('/home');
    },
    handleLogin() {
      this.$auth.login();
    }
  },
  data() {
    return {
      isAuthenticated: false,
    };
  },
};
</script>

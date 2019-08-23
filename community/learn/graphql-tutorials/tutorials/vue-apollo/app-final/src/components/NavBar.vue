<template>
  <div>
    <nav class="navbar navbar-expand-md navbar-light bg-light removeMarBottom">
      <div class="navbar-brand navBrand">GraphQL Tutorial App</div>
      <div class="collapse navbar-collapse navBarAlign" id="navbarNav">
        <button v-on:click="logout" v-if="isAuthenticated" class="login-btn">Log out</button>
      </div>
    </nav>
  </div>
</template>

<script>
export default {
  name: "NavBar",
  beforeCreate() {
    this.$auth.renewTokens();
  },
  methods: {
    logout() {
      this.$auth.logOut();
      this.$router.push({ path: "/" });
    },
    handleLoginEvent(data) {
      this.isAuthenticated = data.loggedIn;
    }
  },
  data() {
    return {
      isAuthenticated: false
    };
  }
};
</script>

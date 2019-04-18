<template>
  <div class="nav-container">
    <nav class="navbar navbar-expand-md navbar-light bg-light">
      <div class="container">
        <div class="navbar-brand"></div>
        <button
          class="navbar-toggler"
          type="button"
          data-toggle="collapse"
          data-target="#navbarNav"
          aria-controls="navbarNav"
          aria-expanded="false"
          aria-label="Toggle navigation"
        >
          <span class="navbar-toggler-icon"></span>
        </button>

        <div class="collapse navbar-collapse" id="navbarNav">
          <ul class="navbar-nav mr-auto">
            <li class="nav-item">
              <router-link to="/" class="nav-link">Home</router-link>
            </li>
          </ul>
          <ul class="navbar-nav d-none d-md-block">
            <li v-if="!isAuthenticated" class="nav-item">
              <button
                id="qsLoginBtn"
                class="btn btn-primary btn-margin"
                @click.prevent="login"
              >Login</button>
            </li>

            <li class="nav-item dropdown" v-if="isAuthenticated">
              <a
                class="nav-link dropdown-toggle"
                href="#"
                id="profileDropDown"
                data-toggle="dropdown"
              >
                <img :src="profile.picture" alt="User's profile picture" class="nav-user-profile">
              </a>
              <div class="dropdown-menu dropdown-menu-right">
                <div class="dropdown-header">{{ profile.name }}</div>
                <router-link to="/profile" class="dropdown-item dropdown-profile">
                  <span class="icon icon-profile"></span> Profile
                </router-link>
                <a id="qsLogoutBtn" href="#" class="dropdown-item" @click.prevent="logout">
                  <span class="icon icon-power"></span> Log out
                </a>
              </div>
            </li>
          </ul>

          <ul class="navbar-nav d-md-none" v-if="!isAuthenticated">
            <button class="btn btn-primary btn-block" @click="login">Log in</button>
          </ul>

          <ul class="navbar-nav d-md-none" v-if="isAuthenticated">
            <li class="nav-item">
              <span class="user-info">
                <img :src="profile.picture" alt="User's profile picture" class="nav-user-profile d-inline-block">
                <h6 class="d-inline-block">{{ profile.name }}</h6>
              </span>
            </li>
            <li>
              <span class="icon icon-profile"></span>
              <router-link to="/profile">Profile</router-link>
            </li>

            <li>
              <span class="icon icon-power"></span>
              <a id="qsLogoutBtn" href="#" class @click.prevent="logout">Log out</a>
            </li>
          </ul>
        </div>
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
    login() {
      this.$auth.login();
    },
    logout() {
      this.$auth.logOut();
      this.$router.push({ path: "/" });
    },
    handleLoginEvent(data) {
      this.isAuthenticated = data.loggedIn;
      this.profile = data.profile;
    }
  },
  data() {
    return {
      isAuthenticated: false,
      profile: {}
    };
  }
};
</script>

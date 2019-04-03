<template>
  <div id="app">
    <nav-bar/>
    <div class="container mt-5">
      <router-view/>
    </div>
  </div>
</template>

<script>
import "jquery";
import "samples-bootstrap-theme";
import "samples-bootstrap-theme/dist/css/auth0-theme.css";

import NavBar from "./components/NavBar";

export default {
  components: {
    NavBar
  },
  async created() {
    try {
      await this.$auth.renewTokens();
    } catch {
      // Supress the 'not logged in' error as we can illegitimately get that
      // when processing the callback url
    }
  }
};
</script>

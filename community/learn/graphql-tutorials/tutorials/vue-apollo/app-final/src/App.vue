<template>
  <div id="app">
    <nav-bar/>
    <div>
      <router-view/>
    </div>
  </div>
</template>

<script>
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

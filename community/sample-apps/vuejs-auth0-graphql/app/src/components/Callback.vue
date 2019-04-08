<template>
  <div class="spinner">
    <img src="../assets/loading.svg" alt="Loading">
  </div>
</template>

<script>
export default {
  methods: {
    handleLoginEvent(data) {
      // this.$router.push(data.state.target || "/");
      window.location.href = data.state.target || "/";
    }
  },
  created() {
    this.$auth.handleAuthentication();
  }
};
</script>

<style scoped>
.spinner {
  position: absolute;
  display: flex;
  justify-content: center;
  height: 100vh;
  width: 100vw;
  background-color: white;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
}
</style>

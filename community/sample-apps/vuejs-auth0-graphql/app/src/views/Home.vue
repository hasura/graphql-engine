<template>
  <div>
    <div class="spinner" v-if="isLoading">
      <img src="../assets/loading.svg" alt="Loading">
    </div>
    <div class="text-center hero" v-if="!isLoading">
      <img class="mb-3 app-logo" src="/logo.png" alt="Vue.js logo">
      <div v-if="!isAuthenticated">
        <h1 class="mb-4">
          Login to view articles
        </h1>
        <p class="lead">
          This is a sample application that demonstrates an authentication flow for an SPA, using
          <a
            href="https://vuejs.org"
          >Vue.js</a> and making a authenticated GraphQL query to <a href="https://github.com/hasura/graphql-engine">Hasura GraphQL Engine</a>
        </p>
      </div>
      <div v-if="isAuthenticated">
        <h1 class="mb-4">
          Articles written by me
        </h1>
        <div v-for="a in article" :key="a.id">
          {{a.id}}. {{ a.title }}
        </div> 
      </div>
    </div>

  </div>
</template>



<script>
import gql from 'graphql-tag'
export default {
  name: "home",
  methods: {
    handleLoginEvent(data) {
      this.isAuthenticated = data.loggedIn;
      this.isLoading = false;
    }
  },
  beforeCreate() {
    this.isLoading = true;
  },
  mounted() {
    this.isLoading = false;
  },
  data() {
    return {
      isAuthenticated: false,
      isLoading: true
    };
  },
  apollo: {
    // Simple query that will update the 'article' vue property
    article: gql`query {
      article {
        id
        title
      }
    }`,
  },
};
</script>

<style lang="scss" scoped>
.next-steps {
  .fa-link {
    margin-right: 5px;
  }
}
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

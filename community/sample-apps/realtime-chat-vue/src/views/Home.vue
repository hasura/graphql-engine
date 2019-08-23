<template>
  <div class="home">
    <ChatWrapper :userId="userId" :username="username"/>
    <footer class="App-footer">
          <div class="hasura-logo">
            <img src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_black.svg" @click="windowOpen" alt="Powered by Hasura"/>
            &nbsp; | &nbsp;
            <a href="https://realtime-chat.demo.hasura.app/console" target="_blank" rel="noopener noreferrer">
              Backend
            </a>
            &nbsp; | &nbsp;
            <a href="https://github.com/hasura/graphql-engine/tree/master/community/examples/realtime-chat-vue" target="_blank" rel="noopener noreferrer">
              Source
            </a>
            &nbsp; | &nbsp;
            <a href="https://dev.to/hasurahq/realtime-chat-app-with-vue-and-hasura-57db" target="_blank" rel="noopener noreferrer">
              Blogpost
            </a>
            <a href="https://dev.to/hasurahq/vue-and-graphql-with-hasura-video-course-3mpp" target="_blank" rel="noopener noreferrer">
              Vue and GraphQL course
            </a>
          </div>
          <div class="footer-small-text"><span>(The database resets every 24 hours)</span></div>
              <button
            class="btn btn-outline-secondary"
            type="submit"
            @click="logOut"
          >
            Log Out
          </button>
        </footer>
  </div>
</template>

<script>
// @ is an alias to /src
import ChatWrapper from "@/components/ChatWrapper.vue";
import { TokenService } from '../services/storage';

export default {
  name: "home",
  components: {
    ChatWrapper
  },
  props:{
    userId: {
      type: String,
      default() {
        const savedData = JSON.parse(TokenService.getToken())
        return savedData && `${savedData.userId}`
      }
    },
    username: {
      type: String,
      default(){
        const savedData = JSON.parse(TokenService.getToken())
        return this.$route.query.username || savedData && savedData.username
      }
    }
  },
  methods: {
    windowOpen(){
      window.open("https://hasura.io")
    },
    logOut() {
      TokenService.removeToken()
      this.$router.go();
    }
  },
  created(){
    setInterval(
      async () => {
        await this.$apollo.mutate({
          mutation: require('../graphql/emitOnlineEvent.gql'),
          variables: {
            userId: this.userId
          }
        })
      },
      3000
    )
  }
};
</script>

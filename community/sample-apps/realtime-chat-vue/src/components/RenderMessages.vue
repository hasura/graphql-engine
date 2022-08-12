<template>
  <div id="chatbox">    
    <MessageList :messages="messages" username="vnovick"/>
    <div class="bottom"></div>
  </div>
</template>

<script>
import Banner from './Banner'
import MessageList from './MessageList'

  export default {
    name: "RenderMessages",
    components: {
      Banner,
      MessageList
    },
    data() {
      return {
        messages: [],
        newMessages: [],
        bottom: false
      }
    },
    created(){
      window.addEventListener("scroll", this.handleScroll);
    },
    updated(){
      this.scrollToBottom();
    },
    methods: {
      handleBannerClick(){
        alert("Banner Clicked")
      },
      scrollToBottom() {
        document.getElementById('lastMessage').scrollIntoView({ behavior: "instant" });
      },
      handleScroll(e) {
        const windowHeight = "innerHeight" in window ? window.innerHeight : document.documentElement.offsetHeight;
        const body = document.getElementById("chatbox");
        const html = document.documentElement;
        const docHeight = Math.max(body.scrollHeight, body.offsetHeight, html.clientHeight,  html.scrollHeight, html.offsetHeight);
        const windowBottom = windowHeight + window.pageYOffset;
        if (windowBottom >= docHeight) {
          this.bottom = true
        } else {
          if (this.bottom) {
            this.bottom = false
          }
        }
      }
    },
    apollo: {
      messages: {
        query: require('../graphql/fetchMessages.gql'),
        loadingKey: "loading",
        variables(){
          return {
            last_received_id: -1,
            last_received_ts: "2018-08-21T19:58:46.987552+00:00"
          }
        },
        update(data){
          const receivedmessages = data.message
          return receivedmessages
        },
        fetchPolicy: 'cache-and-network',
        subscribeToMore: {
          document: require('../graphql/subscribeToNewMessages.gql'),
          updateQuery: (previousResult, { subscriptionData }) => {
            if (previousResult) {
              return {
                message: [
                  ...previousResult.message,
                  ...subscriptionData.data.message
                ]
              }
            }
          }
        },
        error(){
          alert("Error occured")
        }
      }
    }
  }
</script>

<style scoped>
  .bottom {
    margin-bottom: 100px;
  }
</style>
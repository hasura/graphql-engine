<template>
  <div class="typingIndicator">
    {{ text }}
  </div>
</template>

<script>
  export default {
    name: "TypingIndicator",
    data(){
      return {
        text: ''
      }
    },
    props: ["userId", "username"],
    apollo: {
      $subscribe: {
        userTyping: {
          query: require('../graphql/getUserTyping.gql'),
          variables() {
            return {
              selfId: this.userId
            }
          },
          result({ data, loading, error }) {
            if (data.user_typing.length !== 0) {
              this.text = `${data.user_typing[0].username} is typing...`
            } else {
              this.text = ""
            }
          }
        }
      }
    }
  }
</script>

<style scoped>

</style>
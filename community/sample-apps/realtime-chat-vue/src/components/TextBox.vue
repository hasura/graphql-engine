<template>
  <div>
    <form @submit="sendMessage">
      <div class="textboxWrapper">
        <TypingIndicator :userId="userId" :username="username"/>
        <input
          id="textbox"
          class="textbox typoTextbox"
          v-model="text"
          autoFocus={true}
          autoComplete="off"
        />
        <button
          class="sendButton typoButton"
          @click.prevent="sendMessage"
        > Send </button>
      </div>
    </form>
  </div>
</template>

<script>
import TypingIndicator from './TypingIndicator'
  export default {
    name: "Textbox",
    props: ["userId","username"],
    data(){
      return {
        text: ''
      }
    },
    components: {
      TypingIndicator
    },
    watch: {
      text: function(value){
        const textLength = value.length;
        if ((textLength !== 0 && textLength % 5 === 0) || textLength === 1) {
          this.emitTypingEvent();
        }
      }
    },
    methods: {
      emitTypingEvent(){
        if(this.userId) {
          this.$apollo.mutate({
          mutation: require('../graphql/emitTypingEvent.gql'),
          variables: {
              userId: this.userId
          }
          })
        }
      },
      sendMessage(event){
        if (this.text === '') return
        this.$apollo.mutate({
          mutation: require('../graphql/insertMessage.gql'),
          variables: {
            message: {
              username: this.username,
              text: this.text
            }
          }
        })
      }
    }
  }
</script>

<style scoped>

</style>
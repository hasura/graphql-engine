<template>
  <div v-bind:class="listClassName">
    <div class="message" v-for="(m, key) in messages" v-bind:key="key">
              <div class="messageNameTime">
                <div class="messageName">
                  <b>{{m.username}}</b>
                </div>
                <div class="messsageTime">
                  <i>{{getMessageTime(m.timestamp)}} </i>
                </div>
              </div>
              <div class="messageText">
                {{m.text }}
              </div>
            </div>
      <div
        id="lastMessage"
      >
      </div>
    </div>
</template>

<script>
import moment from 'moment'

  export default {
    name: "MessageList",
    props: {
      messages: {
        type: Array,
        default(){
          return []
        }
      },
      isNew: {
        type: Boolean,
        default: false
      },
      username: {
        type: String
      }
    },
    methods: {
      getMessageTime(timestamp){
        return moment(timestamp).fromNow()
      }
    },
    computed: {
      listClassName(){
        return this.isNew ? "messageWrapperNew" : "messageWrapper"
      }
    }
  }
</script>

<style scoped>
  #lastMessage {
    height: 0;
  }
</style>
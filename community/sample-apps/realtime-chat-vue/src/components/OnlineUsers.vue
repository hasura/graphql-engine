<template>
  <div :class="[isMobileView ? 'mobileonlineUsers visible-xs' : 'onlineUsers hidden-xs']">
    <div>
        <p
          :class="[isMobileView ? 'mobileuserListHeading' : 'userListHeading']"
          @click="toggleMobileView"
        >
          Online Users ({{!usersOnline ? 0 : usersOnline.length}}) 
          <i v-if="isMobileView" :class="[showMobileView ? 'fa fa-angle-down': 'fa fa-angle-up']"></i>
        </p>
        <ul v-if="(isMobileView && this.showMobileView) || !isMobileView" :class="[isMobileView ? 'mobileUserList' :'userList']">
          <li v-for="(user, key) in usersOnline" v-bind:key="key">
            {{user.username}}
          </li>
        </ul>
    </div>
  </div>
</template>

<script>
  export default {
    name: "OnlineUsers",
    data(){
      return {
        usersOnline: [],
        text: '',
        showMobileView: false
      }
    },
    methods: {
      toggleMobileView(){
        this.showMobileView = !this.showMobileView;
      }
    },
    props: ["userId", "isMobileView"],
    apollo: {
      $subscribe: {
        users: {
          query: require('../graphql/fetchOnlineUsersSubscription.gql'),
          result({ data, loading, error}) {
            if (error) {
              this.text = "Error loading online"
            }
            if (loading) {
              this.text = "Loading..."
            }
            this.usersOnline = data.user_online
          }
        }
      }
    }
  }
</script>

<style scoped>

</style>
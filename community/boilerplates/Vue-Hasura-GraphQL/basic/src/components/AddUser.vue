<template>
  <div class="row">
    <div class="col-sm-4 offset-sm-4 text-center">
      <b-form @submit.prevent="adduser" @keyup="checkuser">

        <b-form-group id="username"
                      label="Your Username:"
                      label-for="usernameinput">
          <br>
          <b-form-input id="usernameinput"
                        type="text"
                        v-model="username"
                        required
                        placeholder="Enter username">
          </b-form-input>
        </b-form-group>

        <b-button v-bind:href="'user/' + userid + '/todos'" variant="primary" v-if="userExists">Login</b-button>
        <b-button type="submit" variant="primary" v-else>Signup</b-button>
        <br>
        <span class="text-center" v-if="userExists"> This usrname exists! Login instead. </span>
      </b-form>
    </div>
  </div>
</template>

<script>
import { ADD_USER, GET_USER_BY_NAME } from '@/graphql'

export default {
  name: 'AddUser',
  data () {
    return {
      username: '',
      userid: '',
      userExists: false
    }
  },
  methods: {
    checkuser () {
      this.$apollo
        .query({
          query: GET_USER_BY_NAME,
          variables: {
            name: this.username
          }
        })
        .then(response => {
          if (response.data.users.length !== 0) {
            this.userExists = true
            this.userid = response.data.users[0].id
          } else {
            this.userExists = false
          }
        })
    },
    adduser () {
      this.$apollo
        .mutate({
          mutation: ADD_USER,
          variables: {
            username: this.username
          }
        })
        .then(response => {
          var userid = response.data.insert_users.returning[0].id
          this.$router.replace(`/user/${userid}/todos`)
        })
    }
  }
}
</script>

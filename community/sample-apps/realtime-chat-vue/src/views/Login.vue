<template>
<ApolloMutation
  :mutation="require('../graphql/AddUserMutation.gql')"
  :variables="{
    username
  }"
  @done="login"
>
<template slot-scope="{ mutate, loading, error }">
  <div class="container-fluid minHeight">
    <div class="bgImage"></div>
    <div>
      <div class="headerWrapper">
        <div class="headerDescription">
          Realtime Chat App
        </div>
      </div>
      <div class="mainWrapper">
        <div class="col-md-5 col-sm-6 col-xs-12 noPadd">
          <div class="appstackWrapper">
            <div class="appStack">
              <div class="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                <i class="em em---1" />
              </div>
              <div class="col-md-11 col-sm-11 col-xs-10 noPadd">
                <div class="description">
                  Try out a realtime app that uses
                </div>
                <div class="appStackIconWrapper">
                  <div class="col-md-4 col-sm-4 col-xs-4 noPadd">
                    <div class="appStackIcon">
                      <img
                        class="img-responsive"
                        v-bind:src="vueLogo"
                        alt="React logo"
                      />
                    </div>
                  </div>
                  <div class="col-md-8 col-sm-8 col-xs-8 noPadd">
                    <div class="appStackIcon">
                      <img
                        class="img-responsive"
                        v-bind:src="graphql"
                        alt="GraphQL logo"
                      />
                    </div>
                  </div>
                </div>
              </div>
            </div>
            <div class="appStack">
              <div class="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                <i class="em em-rocket" />
              </div>
              <div class="col-md-11 col-sm-11 col-xs-10 noPadd">
                <div class="description">Powered by</div>
                <div class="appStackIconWrapper">
                  <div class="col-md-4 col-sm-4 col-xs-4 noPadd">
                    <div class="appStackIcon">
                      <img
                        class="img-responsive"
                        v-bind:src="apolloLogo"
                        alt="apollo logo"
                      />
                    </div>
                  </div>
                  <div class="col-md-4 col-sm-4 col-xs-4 noPadd">
                    <div class="appStackIcon">
                      <img
                        class="img-responsive"
                        v-bind:src="hasuraLogo"
                        alt="Hasura logo"
                      />
                    </div>
                  </div>
                </div>
              </div>
            </div>
            <div class="appStack">
              <div class="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                <i class="em em-sunglasses" />
              </div>
              <div class="col-md-11 col-sm-11 col-xs-10 noPadd">
                <div class="description removePaddBottom">
                  Explore the Hasura GraphQL backend and try out some queries &
                  mutations
                </div>
              </div>
            </div>
            <div class="appStack removePaddBottom">
              <div class="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth">
                <i class="fas fa-check-square checkBox"></i>
              </div>
              <div class="col-md-11 col-sm-11 col-xs-10 noPadd">
                <div class="description removePaddBottom">
                  What you get...
                </div>
                <div class="addPaddTop">
                  <div
                    class="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth"
                  >
                    <i class="em em-hammer_and_wrench"></i>
                  </div>
                  <div class="col-md-11 col-sm-11 col-xs-10 noPadd">
                    <div class="description removePaddBottom">
                      Source code
                    </div>
                  </div>
                </div>
                <div class="addPaddTop">
                  <div
                    class="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth"
                  >
                    <i class="em em-closed_lock_with_key"></i>
                  </div>
                  <div class="col-md-11 col-sm-11 col-xs-10 noPadd">
                    <div class="description removePaddBottom">
                      Access to GraphQL Backend
                    </div>
                  </div>
                </div>
                <div class="addPaddTop">
                  <div
                    class="col-md-1 col-sm-1 col-xs-2 removePaddLeft flexWidth"
                  >
                    <i class="em em-zap" />
                  </div>
                  <div class="col-md-11 col-sm-11 col-xs-10 noPadd">
                    <div class="description removePaddBottom">
                      Full Tutorial
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div class="formGroupWrapper">
            <div class="input-group inputGroup">
              <input
                type="text"
                class="form-control"
                placeholder="Enter your username"
                v-model="username"
              />
              <div class="input-group-append groupAppend">
                <button
                  class="btn btn-outline-secondary"
                  type="submit"
                  v-on:click.prevent="validateUserAndInsert(mutate)"
                  :disabled="loading || username === ''"
                >
                  {{ loading ? "Please wait ..." : "Get Started" }}
                </button>
              </div>
                <div v-if="error" class="alert alert-danger addPaddTop">
                  {{error && "Error occurred. Probably user already exists"}}
                </div>
            </div>
          </div>
          <div class="footer">
            Built with
            <i class="fas fa-heart" />
            by
            <a
              href="https://hasura.io/"
              target="_blank"
              rel="noopener noreferrer"
            >
              Hasura
            </a>
          </div>
        </div>
        <div class="tutorialImg col-md-6 col-sm-6 col-xs-12 hidden-xs noPadd">
          <img class="img-responsive" v-bind:src="rightImg" alt="View" />
        </div>
      </div>
    </div>
  </div>
  </template>
</ApolloMutation>
</template>

<script>
import { TokenService } from '../services/storage';
const vueLogo = require("../images/Vue-logo.png");
const graphql = require("../images/graphql.png");
const hasuraLogo = require("../images/green-logo-white.svg");
const apolloLogo = require("../images/apollo.png");
const rightImg = require("../images/chat-app.png");

export default {
  name: "Login",
  data: () => ({
    username: "",
    rightImg,
    apolloLogo,
    hasuraLogo,
    graphql,
    vueLogo,
    loading: false
  }),
  methods: {
    validateUserAndInsert(insert_user) {
      if (this.username.match(/^[a-z0-9_-]{3,15}$/g)) {
        insert_user()
      } else {
        alert(
          "Invalid username. Spaces and special characters not allowed. Max 15 charachters. Please try again"
        );
        this.username = "";
      }
    },
    login({ data }){
      const userId = data.insert_user.returning[0].id
      TokenService.saveToken(JSON.stringify({
        userId,
        username: this.username
      }))
      this.$router.push({ path: `/chat/${userId}`, query: {
        username: this.username
      } })
    }
  }
};
</script>

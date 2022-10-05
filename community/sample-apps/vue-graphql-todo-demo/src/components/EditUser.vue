<template>
  <div>
    <h3>Edit User</h3>
    <div class="form">
      <fieldset>
        <input type="text" placeholder="Name" v-model="user.name" />
        <input type="text" placeholder="Email" v-model="user.email" />
      </fieldset>
      <button type="submit" class="method-btn" @click="submit">Update</button>
    </div>
  </div>
</template>
<script>
import gql from "graphql-tag";
import Toasters from "./../mixins/toasters";

export default {
  name: "EditUser",
  mixins: [Toasters],
  data() {
    return {
      user: {},
    };
  },
  mounted() {
    this.user = this.$route.params.user;
    if (!this.user) {
      this.$router.push("/");
    }
  },
  methods: {
    submit(e) {
      e.preventDefault();
      let name = this.user.name;
      let email = this.user.email;
      let user_id = this.user.user_id;
      this.$apollo.mutate({
        mutation: gql`
          mutation updateUsers($user_id: Int!, $name: String, $email: String) {
            update_users(
              where: { user_id: { _eq: $user_id } }
              _set: { email: $email, name: $name }
            ) {
              returning {
                user_id
              }
            }
          }
        `,
        variables: {
          user_id,
          name,
          email,
        },
        refetchQueries: ["getUsers"],
      });
      let message = "User updated successfully.";
      this.$router.push({
        name: "UsersList",
        params: { message },
      });
    },
  },
};
</script>
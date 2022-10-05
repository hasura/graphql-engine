<template>
  <div>
    <h3>Add User</h3>
    <div class="form">
      <fieldset>
        <input type="text" placeholder="Name" v-model="name" />
        <input type="text" placeholder="Email" v-model="email" />
      </fieldset>
      <button type="submit" class="method-btn" @click="submit">Add</button>
    </div>
  </div>
</template>
<script>
import gql from "graphql-tag";
import Toasters from "./../mixins/toasters";

export default {
  name: "AddUser",
  mixins: [Toasters],
  data() {
    return {
      name: "",
      email: "",
    };
  },

  methods: {
    submit() {
      let name = this.name;
      let email = this.email;
      this.$apollo.mutate({
        mutation: gql`
          mutation addUser($name: String, $email: String) {
            insert_users(objects: [{ name: $name, email: $email }]) {
              returning {
                user_id
              }
            }
          }
        `,
        variables: {
          name,
          email,
        },
        refetchQueries: ["getUsers"],
      });
      let message = "User added successfully.";
      this.$router.push({
        name: "UsersList",
        params: { message },
      });
    },
  },
};
</script>
<style>
h3 {
  text-align: center;
  font-size: x-large;
}
.form {
  max-width: 300px;
  margin: 10px auto;
  padding: 10px 20px;
  background: #f4f7f8;
  border-radius: 8px;
}

h1 {
  margin: 0 0 30px 0;
  text-align: center;
}

input[type="text"],
input[type="password"],
input[type="date"],
input[type="datetime"],
input[type="email"],
input[type="number"],
input[type="search"],
input[type="tel"],
input[type="time"],
input[type="url"],
textarea,
select {
  background: rgba(255, 255, 255, 0.1);
  border: none;
  font-size: 16px;
  height: auto;
  margin: 0;
  outline: 0;
  padding: 15px;
  width: 100%;
  background-color: #e8eeef;
  color: #8a97a0;
  box-shadow: 0 1px 0 rgba(0, 0, 0, 0.03) inset;
  margin-bottom: 30px;
}

input[type="radio"],
input[type="checkbox"] {
  margin: 0 4px 8px 0;
}

select {
  padding: 6px;
  height: 32px;
  border-radius: 2px;
}

.method-btn {
  padding: 19px 39px 18px 39px;
  color: #fff;
  background-color: #4bc970;
  font-size: 18px;
  text-align: center;
  font-style: normal;
  border-radius: 5px;
  width: 100%;
  border: 1px solid #3ac162;
  border-width: 1px 1px 3px;
  box-shadow: 0 -1px 0 rgba(255, 255, 255, 0.1) inset;
  margin-bottom: 10px;
}

fieldset {
  margin-bottom: 30px;
  border: none;
}

legend {
  font-size: 1.4em;
  margin-bottom: 10px;
}

label {
  display: block;
  margin-bottom: 8px;
}

label.light {
  font-weight: 300;
  display: inline;
}

.number {
  background-color: #5fcf80;
  color: #fff;
  height: 30px;
  width: 30px;
  display: inline-block;
  font-size: 0.8em;
  margin-right: 4px;
  line-height: 30px;
  text-align: center;
  text-shadow: 0 1px 0 rgba(255, 255, 255, 0.2);
  border-radius: 100%;
}
.action {
  color: #fff;
  background-color: #4bc970;
  font-size: 18px;
  text-align: center;
  font-style: normal;
  border-radius: 5px;
  border: 1px solid #3ac162;
  border-width: 1px 1px 3px;
  box-shadow: 0 -1px 0 rgba(255, 255, 255, 0.1) inset;
  margin-bottom: 10px;
  width: 50%;
}
.delete-btn {
  background-color: red;
  border: 1px solid red;
}
.edit-btn {
  background-color: blue;
  border: 1px solid blue;
}
@media screen and (min-width: 480px) {
  .form {
    max-width: 480px;
  }
}
</style>

<template>
  <div>
    <h3>Users</h3>
    <router-link to="/add" class="add-user">Add User</router-link>
    <table>
      <thead>
        <tr>
          <th>User Id</th>
          <th>Name</th>
          <th>Email</th>
          <th>Action</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="user in userList" :key="user.user_id">
          <td data-column="user_id">{{ user.user_id }}</td>
          <td data-column="name">{{ user.name }}</td>
          <td data-column="email">{{ user.email }}</td>
          <td data-column="delete">
            <button @click="editUserData(user)" class="action edit-btn">
              Edit
            </button>
            <button @click="deleteUser(user.user_id)" class="action delete-btn">
              Delete
            </button>
          </td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script>
import gql from "graphql-tag";
import Toasters from "./../mixins/toasters";

export default {
  name: "UsersList",
  mixins: [Toasters],
  data() {
    return {
      name: "",
      email: "",
      editUser: {},
      edit: false,
    };
  },
  apollo: {
    userList: {
      query: gql`
        query getUsers {
          users {
            user_id
            name
            email
          }
        }
      `,
      update: (data) => {
        return data.users;
      },
    },
  },
  mounted() {
    let message = this.$route.params.message;
    if (message) {
      this.success(message);
    }
  },
  methods: {
    deleteUser(id) {
      let user_id = id;
      this.$apollo.mutate({
        mutation: gql`
          mutation deleteUser($user_id: Int!) {
            delete_users(where: { user_id: { _eq: $user_id } }) {
              returning {
                user_id
              }
            }
          }
        `,
        variables: {
          user_id,
        },
        refetchQueries: ["getUsers"],
      });
      setTimeout(() => {
        this.success("Users deleted successfully");
      }, 2000);
    },
    editUserData(user) {
      this.$router.push({
        name: "EditUser",
        params: { user },
      });
    },
  },
};
</script>
<style scoped>
h3 {
  text-align: center;
  font-size: x-large;
}
table {
  width: 750px;
  border-collapse: collapse;
  margin: 50px auto;
}

/* Zebra striping */
tr:nth-of-type(odd) {
  background: #eee;
}

th {
  background: #3498db;
  color: white;
  font-weight: bold;
}

td,
th {
  padding: 10px;
  border: 1px solid #ccc;
  text-align: left;
  font-size: 18px;
}

/* 
Max width before this PARTICULAR table gets nasty
This query will take effect for any screen smaller than 760px
and also iPads specifically.
*/
@media only screen and (max-width: 760px),
  (min-device-width: 768px) and (max-device-width: 1024px) {
  table {
    width: 100%;
  }

  /* Force table to not be like tables anymore */
  table,
  thead,
  tbody,
  th,
  td,
  tr {
    display: block;
  }

  /* Hide table headers (but not display: none;, for accessibility) */
  thead tr {
    position: absolute;
    top: -9999px;
    left: -9999px;
  }

  tr {
    border: 1px solid #ccc;
  }

  td {
    /* Behave  like a "row" */
    border: none;
    border-bottom: 1px solid #eee;
    position: relative;
    padding-left: 50%;
  }

  td:before {
    /* Now like a table header */
    position: absolute;
    /* Top/left values mimic padding */
    top: 6px;
    left: 6px;
    width: 45%;
    padding-right: 10px;
    white-space: nowrap;
    /* Label the data */
    content: attr(data-column);

    color: #000;
    font-weight: bold;
  }
}
*,
*:before,
*:after {
  -moz-box-sizing: border-box;
  -webkit-box-sizing: border-box;
  box-sizing: border-box;
}

body {
  font-family: "Nunito", sans-serif;
  color: #384047;
}
.add-user {
  color: #fff;
  background-color: #4bc970;
  font-size: 18px;
  text-align: center;
  font-style: normal;
  border-radius: 5px;
  border: 1px solid #3ac162;
  border-width: 1px 1px 3px;
  box-shadow: 0 -1px 0 rgb(255 255 255 / 10%) inset;
  margin-bottom: 10px;
  margin-left: 68%;
  text-decoration: none;
  font-weight: 600;
  padding: 10px 10px 10px 10px;
}
</style>
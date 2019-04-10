<template>
  <div class="formInput">
    <input 
	class="input" 
	placeholder="What needs to be done?" 
	v-model="newTodo"
	@keyup.enter="addTodo"
    />
    <i class="downArrow fa fa-angle-down" />
  </div>
</template>

<script>
  import { ADD_TODO, GET_MY_TODOS } from "../TodoQueries"
  export default {
    props: ['type'],
    data() {
      return {
        newTodo: '',
      }
    },
    methods: {
      addTodo: function () {
        const title = this.newTodo && this.newTodo.trim()
        const client = this.$apolloProvider.clients.defaultClient;
        const isPublic = this.type === "public" ? true : false;
        client.mutate({
          mutation: ADD_TODO,
          variables: {
            todo: title,
            isPublic: isPublic
          },
          update: (store, { data: { insert_todos } }) => {
            // Read the data from our cache for this query.
            try {
              if (this.type === "private") {
                const data = store.readQuery({
                  query: GET_MY_TODOS
                });
                const insertedTodo = insert_todos.returning;
                data.todos.splice(0, 0, insertedTodo[0]);
                store.writeQuery({
                  query: GET_MY_TODOS,
                  data
                });
              }
            } catch (e) {
              console.error(e);
            }
          },
        });
        this.newTodo = ''
      },
    }
  }
</script>

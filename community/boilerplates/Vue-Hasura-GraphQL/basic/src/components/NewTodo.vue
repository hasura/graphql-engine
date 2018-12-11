<template>
  <div class="row">
    <div class="col-sm-4 offset-sm-4 text-center">
      <b-form @submit.prevent="addtodo">

        <b-form-group id="todo"
                      label="Add New Todo:"
                      label-for="todoinput">
          <br>
          <b-form-input id="todoinput"
                        type="text"
                        v-model="todoinput"
                        required
                        placeholder="Add new todo">
          </b-form-input>
        </b-form-group>
        <!-- <b-form-group id="isPublic">
          <b-form-checkbox-group v-model="selected" id="public">
            <b-form-checkbox value="true">Public</b-form-checkbox>
          </b-form-checkbox-group>
        </b-form-group> -->

        <b-button type="submit" variant="primary">Submit</b-button>
      </b-form>
    </div>
  </div>
</template>

<script>
import { ADD_TODO } from '@/graphql'

export default {
  name: 'NewTodo',
  data () {
    return {
      selected: false,
      todoinput: ''
      // options: [
      //   { text: 'True', value: 'true' },
      //   { text: 'False', value: 'false' }
      // ]
    }
  },
  methods: {
    addtodo () {
      this.$apollo
        .mutate({
          mutation: ADD_TODO,
          variables: {
            desc: this.todoinput,
            userid: this.$route.params.id
            // public: this.selected
          }
        })
        .then(response => {
          // Lets Do something
        })
    }
  }
}
</script>

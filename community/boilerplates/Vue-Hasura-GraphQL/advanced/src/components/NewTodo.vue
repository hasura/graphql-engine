<template>
  <div class="row">
    <div class="col-sm-4 offset-sm-4 text-center">
      <b-form @submit.prevent="onSubmit">

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
    }
  },
  methods: {
    onSubmit (evt) {
      evt.preventDefault()
      this.$apollo
        .mutate({
          mutation: ADD_TODO,
          variables: {
            desc: this.todoinput,
            userid: localStorage.getItem('sub')
          }
        })
        .then(response => {
          // Lets Do something
        })
    }
  }
}
</script>

<template>
  <v-list-tile class="todo-item" :class="{ 'editing': editing }">
    <v-list-tile-action>
      <v-checkbox
        :input-value="todo.is_completed"
        @change="toggleTodo(todo)"
        color="primary"
        v-if="!editing"
      ></v-checkbox>
      <v-icon
        color="primary"
        v-else
      >edit</v-icon>
    </v-list-tile-action>
    <template v-if="!editing">
      <v-list-tile-content
        :class="{ 'primary--text': todo.is_completed }"
        @dblclick="editing = true"
      >
        {{ todo.text }}
      </v-list-tile-content>
      <v-list-tile-action>
        <v-btn
          @click="removeTodo(todo)"
          color="red lighten-3"
          flat
          icon
        >
          <v-icon>close</v-icon>
        </v-btn>
      </v-list-tile-action>
    </template>
    <v-text-field
      :value="todo.text"
      @blur="doneEdit"
      @keyup.enter="doneEdit"
      @keyup.esc="cancelEdit"
      clearable
      color="primary"
      flat
      hide-details
      maxlength="1023"
      ref="input"
      solo
      v-else
      v-focus="editing"
    ></v-text-field>
  </v-list-tile>
</template>

<script>
import { mapActions } from 'vuex'

export default {
  props: ['todo'],
  data () {
    return {
      editing: false
    }
  },
  directives: {
    focus (el, { value }, { context }) {
      if (value) {
        context.$nextTick(() => {
          context.$refs.input.focus()
        })
      }
    }
  },
  methods: {
    ...mapActions([
      'editTodo',
      'removeTodo',
      'toggleTodo'
    ]),
    doneEdit (e) {
      const value = e.target.value.trim()
      const { todo } = this
      if (!value) {
        this.removeTodo(todo)
      } else if (this.editing) {
        this.editTodo({
          todo,
          value
        })
        this.editing = false
      }
    },
    cancelEdit () {
      this.editing = false
    }
  }
}
</script>

<style lang="stylus">
.todo-item
  .v-list__tile
    height: auto
    padding-top: 12px
    padding-bottom: 12px
  &.editing .v-list__tile
    height: 48px
</style>

<template>
  <div class="todoWrapper">
    <TodoInput type="public" />
    <TodoPublicList type="public" />
  </div>
</template>

<script>
import TodoInput from "../components/TodoInput";
import TodoPublicList from "../components/TodoPublicList";
export default {
  components: {
    TodoInput, TodoPublicList
  },
}
</script>

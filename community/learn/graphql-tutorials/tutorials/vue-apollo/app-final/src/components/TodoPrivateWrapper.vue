<template>
  <div class="todoWrapper">
    <TodoInput type="private" />
    <TodoPrivateList type="private" />
  </div>
</template>

<script>
import TodoInput from "../components/TodoInput";
import TodoPrivateList from "../components/TodoPrivateList";
export default {
  components: {
    TodoInput, TodoPrivateList
  }
}
</script>

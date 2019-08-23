<template>
  <div class="footerList">
    <span>{{ remainingTodos }} items </span>
    <ul>
      <li v-on:click="filterResults('all')">
        <a v-if="filterType === 'all' " class="selected">All</a>
        <a v-else>All</a>
      </li>
      <li v-on:click="filterResults('active')">
        <a v-if="filterType === 'active'" class="removePaddLeft selected">Active</a>
        <a v-else class="removePaddLeft">Active</a>
      </li>
      <li v-on:click="filterResults('completed')">
        <a v-if="filterType === 'completed' " class="removePaddLeft selected">Completed</a>
        <a v-else class="removePaddLeft">Completed</a>
      </li>
    </ul>
    <button v-if="type === 'private'" class="clearComp" v-on:click="clearCompleted">
      Clear Completed
    </button>
  </div>
</template>

<script>
  export default {
    props: ['type', 'remainingTodos', 'filterResults', 'filterType', 'clearCompleted'],
  }
</script>

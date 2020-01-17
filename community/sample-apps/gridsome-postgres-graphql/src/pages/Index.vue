<template>
  <Layout>
    
    <h1>Authors</h1>

    <div v-if="$page.hasura.author.length">
      <div class="author" v-for="author in $page.hasura.author" :key="author.id">
        <p>{{ author.name }}</p>
      </div>
    </div>
    <div v-else>
      <p>No authors found. Add one using Hasura Console</p>
    </div>

  </Layout>
</template>

<page-query>
query {
  hasura {
    author {
      id
      name
    }
  }
}
</page-query>

<script>
export default {
  metaInfo: {
    title: 'Authors'
  }
}
</script>

<style>
.home-links a {
  margin-right: 1rem;
}
</style>

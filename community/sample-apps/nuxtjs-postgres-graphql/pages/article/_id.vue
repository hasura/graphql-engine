<template>
  <div v-if="article">
    <div v-for="item in article" :key="item.id">
      <h3>{{ item.title }}</h3>
      <p>{{ item.content }}</p>
      <p>
        <nuxt-link to="/">
          Home page
        </nuxt-link>
      </p>
    </div>
  </div>
</template>

<script>
import article from '~/apollo/queries/fetchArticle'

export default {
  apollo: {
    article: {
      query: article,
      prefetch: ({ route }) => ({ id: route.params.id }),
      variables() {
        return { id: this.$route.params.id }
      }
    }
  },
  head() {
    return {
      title: 'Articles by Author'
    }
  }
}
</script>


<template>
  <q-page class="flex">
    <div style="width: 500px; max-width: 90vw;">
      <q-list>
        <q-list-header inset>Articles</q-list-header>
        <q-item v-for="item in article" :key="item.id">
          <q-item-side icon="book" inverted color="grey-6" />
          <q-item-main>
            <q-item-tile label>{{ item.title }}</q-item-tile>
          </q-item-main>
      </q-item>
      </q-list>
    </div>
  </q-page>
</template>

<style>
</style>

<script>
import gql from 'graphql-tag'
const articleQuery = gql`
  query articleQuery($authorId: Int!) {
    article(where:{author_id: {_eq: $authorId}}) {
      id
      title
      content
    }
  }`
export default {
  data () {
    return {
      authorId: this.$route.params.authorId
    }
  },
  name: 'Articles',
  apollo: {
    // Simple query that will update the 'article' vue property
    article: {
      query: articleQuery,
      prefetch: false,
      variables () {
        return { authorId: this.authorId }
      }
    }
  },
  watch: {
    '$route.params.authorId': {
      handler: function (authorId) {
        if (this.$apollo.queries.article) { this.$apollo.queries.article.refetch({ authorId: authorId }) }
      },
      deep: true,
      immediate: true
    }
  }
}
</script>

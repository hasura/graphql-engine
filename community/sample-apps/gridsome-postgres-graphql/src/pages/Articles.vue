<template>
  <Layout>
    <h1>Articles</h1>
    <div v-if="$page.hasura.article.length">
	    <div class="articles" v-for="article in $page.hasura.article" :key="article.id">
	      <p>{{ article.title }} by {{ article.author.name }}</p>
	    </div>
    </div>
    <div v-else>
    	<p>No articles found</p>
    </div>
  </Layout>
</template>

<page-query>
query {
  hasura {
    article {
      id
      title
      author {
      	name
  	  }
    }
  }
}
</page-query>

<script>
export default {
  metaInfo: {
    title: 'Articles'
  }
}
</script>

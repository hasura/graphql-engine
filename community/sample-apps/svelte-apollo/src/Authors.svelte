<script context="module">
  import gql from 'graphql-tag';
  import { client } from './apollo';

  const AUTHOR_LIST = gql`
    query {
      author(order_by: [{name: asc}]) {
        name
      }
    }
  `;
  export async function preload() {
    return {
      authorCache: await client.query({ query: AUTHOR_LIST })
    };
  }

</script>

<script>
  import { restore, query } from 'svelte-apollo';
  
  export let authorCache;
  restore(client, AUTHOR_LIST, authorCache.data);

  const authors = query(client, { query: AUTHOR_LIST});
  
</script>

<ul>
  {#await $authors}
    <li>Loading...</li>
  {:then result}
    {#each result.data.author as author (author.id)}
      <li>{author.name}</li>
    {:else}
      <li>No authors found</li>
    {/each}
  {:catch error}
    <li>Error loading authors: {error}</li>
  {/await}
</ul>


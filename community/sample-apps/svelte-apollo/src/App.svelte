<script>
	import ApolloClient from 'apollo-client';
	import { client } from './apollo';
	import { setClient } from 'svelte-apollo';
	import Articles, { preload as articlePreload } from './Articles.svelte';
	import Authors, { preload as authorPreload } from './Authors.svelte';
	import AddAuthor from './AddAuthor.svelte';
	import AuthorsSubscription from './AuthorsSubscription.svelte';

	// Approximate sapper preload
	const articlePreloading = articlePreload();
	const authorPreloading = authorPreload();

	setClient(client);
</script>

<style>
	h1 {
		color: purple;
	}
</style>

<section>
	<h2>Articles (simple query)</h2>

	{#await articlePreloading}
		<p>Preloading articles....</p>
	{:then preloaded}
		<Articles {...preloaded} />
	{:catch error}
		<p>Error preloading articles: {error}</p>
	{/await}

	<h2>Authors (simple query with cache updates)</h2>

	{#await authorPreloading}
		<p>Preloading authors....</p>
	{:then preloaded}
		<Authors {...preloaded} />
		<h2>Add Author (mutation)</h2>
		<AddAuthor {...preloaded} />
	{:catch error}
		<p>Error preloading authors: {error}</p>
	{/await}

	<h2>Authors (subscription)</h2>
	<AuthorsSubscription />
</section>

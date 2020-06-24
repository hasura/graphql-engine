async function fetchGraphQL(text, variables) {
  const response = await fetch(
    "https://[MY_HASURA_ENDPOINT_ROOT]/v1/relay",
    {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        query: text,
        variables,
      }),
    }
  );

  return await response.json();
}

export default fetchGraphQL;

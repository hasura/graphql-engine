export const uploadedFileData = `# will be ignored by the allow-list
type Starship {
  id: ID!
  name: String!
  length(unit: LengthUnit = METER): Float
}

# will be ignored by the allow-list
scalar parsec

# will be ignored by the allow-list
enum Episode {
  NEWHOPE
  EMPIRE
  JEDI
}

# will be stored in the allow-list
query getAuthors{
  author {
    id
    name
  }
}

query getAuthors{
  author {
    id
    name
    address
  }
}

query getAuthors{
  author {
    id
    name
    age
  }
}

query {
  student {
    id
    name
    age
  }
}

query {
  student {
    id
    name
    roll
  }
}

query {
  student {
    id
    name
    address
  }
}

fragment frag on Starship {
  name
}

# will be stored in the allow-list
query getArticles {
  article {
    id
    title
  }
}

# will be stored in the allow-list after patching in the fragment 
query getArticle {
  article {
    id
    title
    ...frag
  }
}

# will be stored in the allow-list
mutation addArticles {
  insert_articles {
    id
    title
  }
}
`;

import gql from 'graphql-tag'

// Define and export all the graphql mutations and queries here

// This is a sample query (To see all query and mutation examples
// switch to basic repo). https://github.com/pradeepgangwar/Vue-Graphql/tree/master/basic
export const TODOS_QUERY = gql`
  query todosQuery {
    todos {
      id
      text
      is_completed
      created_at
    }
  }
`

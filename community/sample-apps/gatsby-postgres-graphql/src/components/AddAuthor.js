import React, { useState } from "react";
import { useMutation } from "@apollo/react-hooks";
import { gql } from "apollo-boost";
import { GET_AUTHORS } from "./AuthorList";

const ADD_AUTHOR = gql`
  mutation insert_author($name: String!) {
    insert_author(objects: { name: $name }) {
      returning {
        id
        name
      }
    }
  }
`;

const AddAuthor = () => {
  const [author, setAuthor] = useState("");
  const [insert_author, { loading, error }] = useMutation(ADD_AUTHOR, {
    update: (cache, { data }) => {
      setAuthor("");
      const existingAuthors = cache.readQuery({
        query: GET_AUTHORS
      });

      // Add the new author to the cache
      const newAuthor = data.insert_author.returning[0];
      cache.writeQuery({
        query: GET_AUTHORS,
        data: {author: [newAuthor, ...existingAuthors.author]}
      });
    }
  });

  if (loading) return "loading...";
  if (error) return `error: ${error.message}`;

  const handleSubmit = event => {
    event.preventDefault();
    insert_author({
      variables: {
        name: author
      }
    });
  };

  return (
    <form onSubmit={handleSubmit}>
      <label htmlFor="author">
        Add Author:
        <input
          name="author"
          value={author}
          onChange={event => setAuthor(event.target.value)}
        />
      </label>
      <button type="submit">ADD</button>
    </form>
  );
};

export default AddAuthor;

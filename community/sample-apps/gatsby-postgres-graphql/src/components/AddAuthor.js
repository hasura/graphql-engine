import React, { useState } from "react";
import { useMutation } from "@apollo/react-hooks";
import { gql } from "apollo-boost";

const ADD_AUTHOR = gql`
  mutation insert_author($name: String!) {
    insert_author(objects: { name: $name }) {
      returning {
        name
      }
    }
  }
`;

const AddAuthor = () => {
  const [author, setAuthor] = useState("");
  const [insert_author, { loading, error }] = useMutation(ADD_AUTHOR, {
    onCompleted: () => setAuthor("")
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

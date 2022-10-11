import React from "react";

import AddAuthor from "../components/AddAuthor";
import AuthorList from "../components/AuthorList";

const Index = () => (
  <div>
    <h1>My Authors</h1>
    <AddAuthor />
    <AuthorList />
  </div>
);

export default Index;
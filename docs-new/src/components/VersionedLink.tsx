import React from "react";
import Link from "@docusaurus/Link";

const VersionedLink = ({ to, children }) => (
  <Link to={`/latest${to}`}>
    {children}
  </Link>
);

export default VersionedLink;

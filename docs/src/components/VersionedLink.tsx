import React from "react";
import Link from "@docusaurus/Link";

const VersionedLink = ({ to, ...props }) => <Link to={`/latest${to}`} {...props} />;

export default VersionedLink;

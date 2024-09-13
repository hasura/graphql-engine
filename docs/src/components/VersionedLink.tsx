import React from "react";
import Link from "@docusaurus/Link";

const VersionedLink = ({ to, ...props }) => <Link to={`${to}`} {...props} />;

export default VersionedLink;

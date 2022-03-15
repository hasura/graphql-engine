import React, { useState, useEffect } from "react";
import Link from "@docusaurus/Link";
import { usePluginData } from '@docusaurus/useGlobalData';
import {DEFAULT_PLUGIN_ID} from '@docusaurus/constants';

const VersionedLink = ({ to, children }) => (
  <Link to={`/latest${to}`}>
    {children}
  </Link>
);

export default VersionedLink;

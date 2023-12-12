import React from 'react';
import Navbar from '@theme-original/Navbar';
import {DDNBanner} from "@site/src/components/BannerDismissable/DDNBanner";
export default function NavbarWrapper(props) {
  return (
    <>
      <DDNBanner/>
      <Navbar {...props} />
    </>
  );
}

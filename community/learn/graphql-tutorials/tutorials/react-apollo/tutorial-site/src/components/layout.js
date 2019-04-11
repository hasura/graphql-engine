import React from "react";
import styled from "react-emotion";
import { MDXProvider } from "@mdx-js/tag";
import ThemeProvider from "./themeProvider";
import mdxComponents from "./mdxComponents";
import Sidebar from "./sidebar";
import RightSidebar from "./rightSidebar";

const Wrapper = styled('div')`
  display: flex;
  justify-content: space-between;

  @media only screen and (max-width: 767px) {
    display: block;
  }
`;

const Content = styled('main')`
  display: flex;
  flex-grow: 1;
  margin: 0px 88px;
  margin-top: 3rem;

  @media only screen and (max-width: 1023px) {
    padding-left: 0;
    margin: 0 10px;
    margin-top: 3rem;
  }
`;

const MaxWidth = styled('div')`

  @media only screen and (max-width: 50rem) {
    width: 100%;
    position: relative;
  }
`;

const Layout = ({ children, location }) => (
  <ThemeProvider location={location}>
    <MDXProvider components={mdxComponents}>
      <Wrapper>
        <div className={'hidden-xs'}>
          <Sidebar location={location} />
        </div>
        <Content>
          <MaxWidth>{children}</MaxWidth>
        </Content>
        <div className={'hidden-xs'}>
          <RightSidebar location={location} />
        </div>
      </Wrapper>
    </MDXProvider>
  </ThemeProvider>
);

export default Layout;

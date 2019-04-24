import React from "react";
import { StaticQuery, graphql } from "gatsby";
import styled from "react-emotion";
import { ExternalLink } from "react-feather";
import Link from "./link";
import './styles.css';
import { forcedNavOrder } from '../config';

const Logo = styled(props => (
  <h1 {...props}>
    <Link to="/">
      <span>Hasura</span>
      <span>MDX</span>
    </Link>
  </h1>
))`
  padding: 0.75rem 0 0.75rem 2rem;
  margin-bottom: 0.5rem;
  font-size: 1.25rem;
  border-bottom: 1px solid #ede7f3;

  a {
    text-decoration: none;
  }

  span:first-child {
    color: #663399;
    color: black;
    margin-right: 0.35rem;
  }

  span:last-child {
    color: #f9ac00;
  }
`;

const Sidebar = styled('aside')`
  width: 100%;
  /* background-color: rgb(245, 247, 249); */
  /* border-right: 1px solid #ede7f3; */
  height: 100vh;
  overflow: auto;
  position: fixed;
  padding-left: 24px;
  position: -webkit-sticky;
  position: -moz-sticky;
  position: sticky;
  top: 0;
  padding-right: 0;
  background-color: #372476;
  /* Safari 4-5, Chrome 1-9 */
  background: linear-gradient(#372476, #3b173b);
  background: -webkit-gradient(linear, 0% 0%, 0% 100%, from(#372476), to(#3b173b));
  /* Safari 5.1, Chrome 10+ */
  background: -webkit-linear-gradient(top, #372476, #3b173b);
  /* Firefox 3.6+ */
  background: -moz-linear-gradient(top, #372476, #3b173b);
  /* IE 10 */
  background: -ms-linear-gradient(top, #372476, #3b173b);
  /* Opera 11.10+ */
  background: -o-linear-gradient(top, #372476, #3b173b);
  @media only screen and (max-width: 767px) {
    padding-left: 0px;
    background-color: #372476;
    background: #372476;
  }
  @media (min-width: 767px) and (max-width:1023px)
  {
    padding-left: 0;
  }
  @media only screen and (max-width: 1023px) {
    width: 100%;
    position: relative;
    height: auto;
  }
`;

// eslint-disable-next-line no-unused-vars
const ListItem = styled(({ className, active, level, ...props }) => {
  if (level === 0) {
    return (
      <li className={className}>
        <Link {...props} />
      </li>
    );
  } else if (level === 1) {
    const customClass = active ? 'active' : '';
    return (
      <li className={'subLevel ' + customClass}>
        <Link {...props} />
      </li>
    );
  } else {
    return (
      <li className={className}>
        <Link {...props} />
      </li>
    );
  }
})`
  list-style: none;

  a {
    color: #fff;
    text-decoration: none;
    font-weight: ${({ level }) => (level === 0 ? 700 : 400)};
    padding: 0.45rem 0 0.45rem ${props => 2 + (props.level || 0) * 1}rem;
    display: block;
    position: relative;

    &:hover {
      background-color: #542683;
    }

    ${props =>
      props.active &&
      `
      color: #fff;
      background-color: #473485;
    `} // external link icon
    svg {
      float: right;
      margin-right: 1rem;
    }
  }
`;

const Divider = styled(props => (
  <li {...props}>
    <hr />
  </li>
))`
  list-style: none;
  padding: 0.5rem 0;

  hr {
    margin: 0;
    padding: 0;
    border: 0;
    border-bottom: 1px solid #ede7f3;
  }
`;

const SidebarLayout = ({ location }) => (
  <StaticQuery
    query={graphql`
      query {
        allMdx {
          edges {
            node {
              fields {
                slug
                title
              }
            }
          }
        }
      }
    `}
    render={({ allMdx }) => {
      const navItems = allMdx.edges
        .map(({ node }) => node.fields.slug)
        .filter(slug => slug !== "/")
        .sort()
        .reduce(
          (acc, cur) => {
            if (forcedNavOrder.find(url => url === cur)) {
              return { ...acc, [cur]: [cur] };
            }

            const prefix = cur.split("/")[1];

            if (prefix && forcedNavOrder.find(url => url === `/${prefix}`)) {
              return { ...acc, [`/${prefix}`]: [...acc[`/${prefix}`], cur] };
            } else {
              return { ...acc, items: [...acc.items, cur] };
            }
          },
          { items: [] }
        );

      const nav = forcedNavOrder
        .reduce((acc, cur) => {
          return acc.concat(navItems[cur]);
        }, [])
        .concat(navItems.items)
        .map(slug => {
          const { node } = allMdx.edges.find(
            ({ node }) => node.fields.slug === slug
          );

          let isActive = false;
          if(location && (location.pathname === node.fields.slug || location.pathname === (process.env.GATSBY_PATH_PREFIX + node.fields.slug)) ) {
            isActive = true;
          }

          return (
            <ListItem
              key={node.fields.slug}
              to={`${node.fields.slug}`}
              level={node.fields.slug.split("/").length - 2}
              // active={location? location.pathname === ('/graphql/react' + node.fields.slug): false}
              active={isActive}
            >
              {node.fields.title}
            </ListItem>
          );
        });

      return (
        <Sidebar>
          {/* <Logo /> */}
          <ul className={'sideBarUL'}>
            {nav}
            <Divider />
            <ListItem to="https://docs.hasura.io/">
              Hasura Docs
              <ExternalLink size={14} />
            </ListItem>
            <ListItem to="https://graphql.org/learn">
              GraphQL Docs
              <ExternalLink size={14} />
            </ListItem>
          </ul>
        </Sidebar>
      );
    }}
  />
);

export default SidebarLayout;

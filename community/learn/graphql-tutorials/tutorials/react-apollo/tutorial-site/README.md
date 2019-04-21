## To run for development

```bash
npm i -g gatsby-cli
gatsby develop
```


----------------------------------------------------------------------


## Gatsby Tutorial Starter Kit

- Uses `gatsby-mdx` boilerplate
- Theme similar to gitbook
- Parses markdown files from `content` folder and generates UI with sidebar navigation, markdown content rendered, table of contents hash navigation on the right and with previous, next functionality.

## Set up and configure

- Tutorial content in markdown goes into `content` folder.
- Modify `gatsby-config.js` for site metadata like title, description and github tutorial location.
- Modify `src/config.js` for left sidebar navigation order. It should be in the format "/<filename.md>"
- For sub nesting in left sidebar, create a folder with the same name as the top level .md filename and the sub navigation is auto-generated. Currently supports only one level of nesting.

## Environment Variables

`GATSBY_SITE_TITLE` - title tag for the current tutorial
`GATSBY_SITE_DESCRIPTION` - description tag for the current tutorial
`GATSBY_DOCS_LOCATION` - github url for Edit on Github functionality
`GATSBY_PATH_PREFIX` - url prefix at which the tutorial will be hosted
`GATSBY_HEADER_TITLE` - title that appears on the top header

## Develop

```
$ npm run develop
```

## Build

```
$ npm run build
```


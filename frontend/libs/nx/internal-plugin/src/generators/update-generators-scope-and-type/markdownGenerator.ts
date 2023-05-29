import { TagDefNew, tagDefs, tagGroups, TagGroupMap } from '../../TagConsts';
import { Tree } from '@nrwl/devkit';

const wrapWithPipes = (str: string) => `|${str}|`;
export function generateDepConstrainMdTable(
  group: TagDefNew<any>[string],
  prefix: string,
  importImageUrl: string
) {
  let lines: string[] = [];

  const orderItems = Object.keys(group);

  const header = wrapWithPipes(
    ` ![Can row import colum](${importImageUrl}) |` +
      orderItems.map(it => ` \`${prefix}${it}\` `).join('|')
  );
  const dividerHeader = wrapWithPipes(
    `---|` + orderItems.map(it => `:---:`).join('|')
  );

  const rows = Object.entries(group).map(([source, { canImport }]) => {
    return wrapWithPipes(
      `\`${prefix}${source}\` | ${orderItems
        .map(tag => (canImport[tag] ? '✅' : '⛔'))
        .map(it => ` ${it} `)
        .join('|')}`
    );
  });

  lines = [...lines, header, dividerHeader, ...rows];

  return lines.join('\n');
}

export function generateMarkdownDocumentationForTags<T extends TagGroupMap>(
  tagGroupMap: T,
  tagDefs: TagDefNew<T>,
  importImageUrl: string
) {
  return Object.entries(tagGroupMap)
    .map(
      ([
        group,
        {
          description,
          tags,
          shouldAlwaysBePresent,
          allowMultiplePerProject,
          prefix,
        },
      ]) => {
        return `## \`${group}\` tag group

${description}

### Tag list :

${tags
  .map(
    tag => `#### \`${prefix}${tag}\`

${tagDefs[group][tag].description}
`
  )
  .join('\n\n')}

### Constraints

- ${
          allowMultiplePerProject
            ? `There can be multiple \`${group}\` tag per library.`
            : `There can be only one \`${group}\` tag per library.`
        }

- ${
          shouldAlwaysBePresent
            ? `A \`${group}\` tag should be present on all libraries.`
            : `A \`${group}\` tag is an optional tag.`
        }

Here is the import rule matrix :

${generateDepConstrainMdTable(tagDefs[group], prefix, importImageUrl)}
      `;
      }
    )
    .join('\n\n');
}

export function generateFullMdDocAboutTags<T extends TagGroupMap>(
  tagGroupMap: T,
  tagDefs: TagDefNew<T>,
  importImageUrl: string
) {
  return `# Tags in the workspace and their organisation

In this workspace, we use tags to organise the libraries and application to ensure have strong boundaries between projects.

There is ${Object.keys(tagGroupMap).length} tag groups : ${Object.keys(
    tagGroupMap
  ).join(', ')}.

You should use the internal generators to create libraries and application, they will have selectors for the tags groups.

Next here, you can find all of the tags that we have, along side the explanation and the import rule matrix.

${generateMarkdownDocumentationForTags(tagGroupMap, tagDefs, importImageUrl)}

## FAQ

> How big/small a library should be ?

A library can be as little as a single function to as big as a full page. You can find the [tradeoffs from the Nx docs](https://nx.dev/more-concepts/creating-libraries).

  `;
}

export async function markdownGenerator(tree: Tree) {
  const mdFile = generateFullMdDocAboutTags(
    tagGroups,
    tagDefs,
    './can-import-icon.png'
  );

  tree.write('/docs/tags.md', mdFile);
}

# Cloud + Core Issue Template

Recently we made a call to migrate our docs tech stack from Sphinx (RST) to Docusaurus (MDX).

The crucial part of this process is actually transforming the `.rst` files to `.mdx` files.

Docs is pretty large (`76 directories, 402 files`), so we would love your help! 💪

### How to contribute?
We created a [migration guide](https://hge-docs-migration.netlify.app/) to help ease the transition.

We used an auto transform tool [Pandoc](https://pandoc.org) to transform reStructiredText to GitHub flavoured markdown and added the resultant markdown content in respective `.txt` files.

Below is the steps to complete the migration cylcle for each doc file.

1. Convert file from `.rst` to `.mdx`. 

  1.1. Convert the transformed `.rst.txt` to `.mdx` extension for the file to be able to show up in sidebar and UI.

  1.2. Use the [migration guide](https://hge-docs-migration.netlify.app/) for syntactical and usage reference.

  1.3. Validate content and cleanup misc divs that are populated during auto transform and any invalid mdx data.

  1.4. The misc populated divs exist to ease adding metadata and any sphinx directives that can't be auto transformed to markdown. Please cross referene the actual RST file for content validation.

2. Verify content correctness and preview in UI.
3. Submit a PR! 🎉

### Available Doc Files
We will periodically add new batches as we complete the existing ones here, so you don't have to search through super long list.

--- Add 5-7 directories at a time?

- [ ] [Core](https://github.com/hasura/graphql-engine/tree/master/docs-new/docs/graphql/core/) @KRRISH96 https://github.com/hasura/graphql-engine/pull/xyz


### Completed Doc Files
The completed ones will be moved here periodically to have the available ones easy visibility.

- [x] [core/actions/create](https://github.com/hasura/graphql-engine/tree/master/docs-new/docs/graphql/core/actions/create.mdx) @KRRISH96 https://github.com/hasura/graphql-engine/pull/xyz


### Reach out to us!

Feel free to reach if you have questions or need help getting started. You can leave comments here or you can tag me(@KRRISH96) in your PR if you need any help or you're not sure about something!

---

To avoid duplicate work please comment on which part you want to work on (as long as nobody else is working on it) so we can mark it as taken.
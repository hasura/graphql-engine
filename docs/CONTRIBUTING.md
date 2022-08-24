# Contributing to Docs

This markdown file will walk you through the process of making a docs contribution to the Hasura GraphQL Engine OSS
repo. If you'd like to view it with a bit more helpful context, check out our
[live docs wiki](https://hasura.io/docs/wiki) complete with this document, a style guide, and other helpful tips for
using our tools.

## Overview

Docs contributions take many forms. However, they all follow the same workflow. Below, you'll find all you need to get
your local development environment set up and - before you know it - you'll be helping make our product clearer for
users.

### Mise-en-place

Like
[Uncle Tony](https://i0.wp.com/jamesabrock.com/wp-content/uploads/2018/06/IMG_0013-1024x682.jpg?fit=676%2C450&ssl=1)
said:

> “Mise-en-place is the religion of all good line cooks...”

Docs believes the same is true for engineers and all contributors to documentation.

As such, there's a few steps needed to get your 'station' in order and to ready your local development environment for
the fast-paced world of documentation.

We use Yarn as our package-manager- / project-manager-of-choice. Please make sure you follow
[their installation instructions](https://classic.yarnpkg.com/lang/en/docs/install/) if not already on your system.

Two `hasura/graphql-engine` repositories exist: one is our OSS repo whereas the other is a private repository only
available to employees at Hasura. We utilize these two repos so we can maintain open-source contributions and also
provide an undisturbed repository to which the community has access. As changes are merged into our private repository,
they're reflected in the OSS version as well.

**This guide is written referencing the `graphql-engine-mono` repo available only to Hasurians.** If you're a community
member, you can follow the same instructions by replacing the references to `graphql-engine-mono` with `graphql-engine`.

Like to automate things? So do we. This snippet runs through the prerequisites and fires up the docs development server.
Simply navigate to the directory you want the `hasura-graphql-engine-mono` repo to live, open your terminal, copy /
paste, hit enter, and save literally seconds:

```bash
git clone https://github.com/hasura/graphql-engine-mono.git
cd graphql-engine-mono/docs
yarn install
yarn start
```

Alternatively, take it step-by-step:

#### Clone the repository

```bash
git clone https://github.com/hasura/graphql-engine-mono.git
```

#### Install docs dependencies

```bash
cd graphql-engine-mono/docs && yarn install
```

We also highly recommend testing locally at this point. Start the development server by running the following command:

```bash
yarn start
```

If the server doesn't start, check the errors logging to the terminal. You can also check the
[common errors section](#common-errors) at the bottom of this page. Hit up Sean Park-Ross on
[Slack](https://hasurahq.slack.com/team/U03J31UJ8V7) (for Hasurians) or
[Discord](https://discord.com/users/981277708564721694) (for our community members) / Rob Dominguez on
[Slack](https://hasurahq.slack.com/team/U03H7ABDMF1) (for Hasurians) or [Discord](https://discord.com/users/5592) (for
our community members) if you need help troubleshooting.

Otherwise, congrats! You're ready to start slinging docs 🤙

## Workflow

### Step 1: Create a new branch

If it's been a while since you've pulled from `main`, make sure it's up-to-date before creating a new branch for this
contribution. You can simply run `git pull` while on `main` to update your local repository to the current state of the
remote on GitHub. Then, utilize the command below the next paragraph to create your branch.

If this is a fresh clone, there shouldn't be any need to `pull` changes down from GitHub. You can create a new branch
and simultaneously switch to it using the command below. We follow a convention that includes your name + `docs` + a
kebab-cased description of what the branch will focus on. For example: `rob/docs/add-more-bourdain-references`.

```bash
git checkout -b <your-name>/docs/<short-update-description>
```

Once this command is run, you should be on your new branch!

### Step 2: Start the development server

You'll want to start the development server so you can see all your changes live. Especially the first time, the
development server will take _some_ time to spin up. However, Docusaurus caches content...so this gets better with
repetition.

```bash
yarn start
```

### Step 3: Make your local changes

We have a [comprehensive-yet-continually-evolving style guide](/style/index.mdx). We ask that you reference it as you
begin formatting your docs contribution.

If you don't want to go piece-by-piece in the style guide, find a page of documentation you think works well and use it
as a template.

Docusaurus utilizes a file-based routing system. You can see more about
[working with Docusaurus](/working-with-docusaurus.mdx) here.

Most files you'll be authoring will be `.mdx`. It's like markdown on steroids and allows utilization of `jsx`
components. For more information about the syntax of markdown,
[check out these docs](https://www.markdownguide.org/basic-syntax/).

### Step 4: Stage your local changes

After your changes have been made, you'll need to stage them using Git. The following command will add all modifications
you've made to Git's staging area:

```bash
git add .
```

### Step 5: Create a commit

With your changes staged, you'll need to create a commit with a commit message. Prefix the message with `docs:` and a
imperative verb followed by a description of the changes present in the commit:

```bash
git commit -m “docs: update / fix / add <feature>”
```

The size of commits isn't of massive importance for docs contributions. If you're here for a quick fix, chances are
there will only be one commit. Alternatively, if you're working on a large contribution documenting a new feature and
need to modify / add / delete multiple files, it helps to have more atomic commits reflective of each set of actions.

### Step 6: Test your local changes

We don't have unit tests for docs. However, Docusaurus will (generally) let you know when it's not happy with something
you did. As such, our testing process consists of doing a build and serving from the `build` folder to make sure
everything functions correctly. To start your build, from the `/docs` directory, run:

```bash
yarn build
```

After a bit, you should see output similar to this:

```bash
# Lots of other output above this...
[SUCCESS] Generated static files in build.
[INFO] Use `npm run serve` command to test your build locally.
✨  Done in 35.84s.
```

If you don't see an output similar to the above (especially one that's missing `SUCCESS`), something went wrong. There
should be errors logged in the terminal; check the [common errors section](#common-errors) at the bottom of this page
for more info.

Once you achieve a successful build, serve the `build` folder using the following command:

```bash
yarn serve
```

All you're doing here is checking to ensure your pages render as expected. Go through any changes you made and ensure
there's no missing images, broken links, or malformations in your code snippets (especially `gql`!).

### Step 7: Push your commit(s)

With everything rendering as expected, it's time to push this contribution online! Run the following to push your
changes up to GitHub:

```bash
git push origin <name-of-your-branch>
```

### Step 8: Compare and pull request

Head to the `hasura-graphql-engine-mono` repo and you should see a `Compare & pull request` button - click it to create
you PR 🤙

Follow the instructions in the PR template and don't forget to add the `c/docs` label.

Finally, ping us on [#team-docs](https://hasurahq.slack.com/archives/C015EA71MU0) for speedy review 🚀

If you're including a significant feature update or bug fix that should be noted on the next release, please be sure to
update `CHANGELOG.md` in the root directory of the repo as well.

## Common Errors

We all make mistakes! Here's the most common ones we encounter. This is not an exhaustive list, but if you find lots of
errors getting thrown, chances are the issue is one of the items below. These are rank-ordered according to what we
encounter most often.

### Broken markdown links

With Docusaurus's file-based routing system, markdown links commonly break due to one of two circumstances:

- Filename changes

  The url path for a particular page is dependent upon its filename. If you change the name of a file, any link
  referencing it will break. The error thrown to the console will alert you to which page contains the error and to
  which file is being referenced. If there are multiple broken links, it will list out each.

- File location changes

  If you change the name of a file or folder (e.g., `modelling/index.mdx` to `modeling/index.mdx`), any links
  referencing this route will break. Often, simply resolving these paths isn't enough...Docusaurus will still be looking
  for these files. If this is the case, kill the server (`CTRL+c`) and start it up again by running `yarn start`.

Regardless of which of the above circumstances caused this error, you'll see output similar to this within the server
console:

```bash
Error: Docs markdown link couldn't be resolved: (/styles/index.mdx) in ~/hasura/graphql-engine-mono/docs/wiki/contributions.mdx for version current
    at Array.forEach (<anonymous>)
client (webpack 5.70.0) compiled with 1 error
```

### Broken image links

Assets are stored in `static/img/<feature-folder>`. Just like the file-based routing Docusaurus uses to render pages,
file paths are important for images as well. However, instead of any output to the console, the page will simply crash
and present a message indicating which image's path cannot be resolved.

![Broken image](/img/wiki/broken-image.png)

Use the erroneous image path to identify the error, facepalm, and then fix your typo.

We use the PNG image format. When you create a PR, our automated CI processes take care of image optimization
automatically 🚀

### Improperly imported components

This is a sneaky one. If you forget to import a component - such as the `<Thumbnail />` component used for images -
there won't be any errors thrown by the server. However, if you check your browser's developer tools, you'll notice the
quite prominent error. Thus, when something just isn't appearing...check the browser's console.

Simply import the missing component at the top of the page and viola 🤌

### Formatting code blocks

Perhaps the biggest pitfall we've encountered with Docusaurus is how delicate `.mdx` files can be with regard to
placement of code blocks and other text. If you've included a code block using common markdown syntax, and things aren't
rendering as you think they should, try giving the block a bit of breathing room with a single empty line above and
below it.

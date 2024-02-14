import { formatFiles, Tree } from '@nrwl/devkit';
import { eslintScopeUpdater } from './eslintScopeUpdater';
import { markdownGenerator } from './markdownGenerator';

export default async function (tree: Tree) {
  // Todo: add the generator updater once the generator are migrated here
  await eslintScopeUpdater(tree);
  await markdownGenerator(tree);
  await formatFiles(tree);
}

import {
  formatFiles,
  readProjectConfiguration,
  visitNotIgnoredFiles,
  Tree,
} from '@nrwl/devkit';
import * as ts from 'typescript';
import * as path from 'path';
import { MigratePathToRelativeGeneratorSchema } from './schema';

export function transformInputText(
  node: ts.StringLiteral,
  fileFolder: string
): string {
  const importString = node.text;
  if (!importString.startsWith('@/')) {
    return node.getText();
  }
  const replacedValue = importString.replace(
    '@/',
    'libs/console/legacy-ce/src/lib/'
  );
  let shortestPath = path.relative(fileFolder, replacedValue);
  if (shortestPath.length === 0) {
    shortestPath = '.';
  } else if (!shortestPath.startsWith('..')) {
    shortestPath = './' + shortestPath;
  }
  return `'${shortestPath}'`;
}

export async function lookAtFiles(tree: Tree, sourceRoot: string) {
  const { tsquery } = await import('@phenomnomnominal/tsquery');
  visitNotIgnoredFiles(tree, sourceRoot, filePath => {
    let fileContent = tree.read(filePath, 'utf-8');

    if (!fileContent?.includes("'@/")) {
      return;
    }
    const fileFolder = path.dirname(filePath);
    fileContent = tsquery.replace(
      fileContent,
      'ImportDeclaration > StringLiteral',
      node => {
        if (!ts.isStringLiteral(node)) {
          return node.getText();
        }
        return transformInputText(node, fileFolder);
      }
    );

    tree.write(filePath, fileContent);
  });
}

export default async function (
  tree: Tree,
  options: MigratePathToRelativeGeneratorSchema
) {
  const project = readProjectConfiguration(tree, 'console-legacy-ce');
  if (!project) {
    throw new Error('Project not found.');
  }
  if (!project.sourceRoot) {
    throw new Error('Source root not found.');
  }
  await lookAtFiles(tree, project.sourceRoot);
  await formatFiles(tree);
}

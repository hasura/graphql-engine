/**
 * This file sets you up with structure needed for an ESLint rule.
 *
 * It leverages utilities from @typescript-eslint to allow TypeScript to
 * provide autocompletions etc for the configuration.
 *
 * Your rule's custom logic will live within the create() method below
 * and you can learn more about writing ESLint rules on the official guide:
 *
 * https://eslint.org/docs/developer-guide/working-with-rules
 *
 * You can also view many examples of existing rules here:
 *
 * https://github.com/typescript-eslint/typescript-eslint/tree/master/packages/eslint-plugin/src/rules
 */

import { ESLintUtils } from '@typescript-eslint/utils';
import {
  normalizePath,
  ProjectGraphExternalNode,
  ProjectGraphProjectNode,
  workspaceRoot,
} from '@nrwl/devkit';
import {
  findProjectUsingImport,
  findProject,
  getSourceFilePath,
  getTargetProjectBasedOnRelativeImport,
  isAbsoluteImportIntoAnotherProject,
} from '@nrwl/eslint-plugin-nx/src/utils/runtime-lint-utils';
import { AST_NODE_TYPES, TSESTree } from '@typescript-eslint/utils';
import { TargetProjectLocator } from 'nx/src/utils/target-project-locator';
import { readProjectGraph } from '@nrwl/eslint-plugin-nx/src/utils/project-graph-utils';

// NOTE: The rule will be available in ESLint configs as "@nrwl/nx/workspace/additional-dep-constraints"
export const RULE_NAME = 'additional-dep-constraints';

type MessageId = 'onlyImportedBy';

type Options = [{}];

export const rule = ESLintUtils.RuleCreator(() => __filename)<
  Options,
  MessageId
>({
  name: RULE_NAME,
  meta: {
    type: 'problem',
    docs: {
      description: ``,
      recommended: 'error',
    },
    schema: [],
    messages: {
      onlyImportedBy:
        'A project tagged with "{{tag}}" are the only ones that can import project tags with "{{tag}}.',
    },
  },
  defaultOptions: [{}],
  create(context) {
    /**
     * Globally cached info about workspace
     */
    const projectPath = normalizePath(
      (global as any).projectPath || workspaceRoot
    );
    const fileName = normalizePath(context.getFilename());

    const { projectGraph, projectRootMappings } = readProjectGraph(RULE_NAME);

    if (!projectGraph) {
      return {};
    }

    const workspaceLayout = (global as any).workspaceLayout;

    if (!(global as any).targetProjectLocator) {
      (global as any).targetProjectLocator = new TargetProjectLocator(
        projectGraph.nodes,
        projectGraph.externalNodes
      );
    }
    const targetProjectLocator = (global as any)
      .targetProjectLocator as TargetProjectLocator;

    function run(
      node:
        | TSESTree.ImportDeclaration
        | TSESTree.ImportExpression
        | TSESTree.ExportAllDeclaration
        | TSESTree.ExportNamedDeclaration
    ) {
      // Ignoring ExportNamedDeclarations like:
      // export class Foo {}
      if (!node.source) {
        return;
      }

      // accept only literals because template literals have no value
      if (node.source.type !== AST_NODE_TYPES.Literal) {
        return;
      }

      const imp = node.source.value as string;

      const sourceFilePath = getSourceFilePath(fileName, projectPath);

      const sourceProject = findProject(
        projectGraph,
        projectRootMappings,
        sourceFilePath
      );
      // If source is not part of an nx workspace, return.
      if (!sourceProject) {
        return;
      }
      // check for relative and absolute imports
      const isAbsoluteImportIntoAnotherProj =
        isAbsoluteImportIntoAnotherProject(imp, workspaceLayout);
      let targetProject: ProjectGraphProjectNode | ProjectGraphExternalNode;

      if (isAbsoluteImportIntoAnotherProj) {
        targetProject = findProject(projectGraph, projectRootMappings, imp);
      } else {
        targetProject = getTargetProjectBasedOnRelativeImport(
          imp,
          projectPath,
          projectGraph,
          projectRootMappings,
          sourceFilePath
        );
      }

      targetProject =
        targetProject ||
        findProjectUsingImport(
          projectGraph,
          targetProjectLocator,
          sourceFilePath,
          imp
        );

      // If target is not part of an nx workspace, return.
      if (!targetProject) {
        return;
      }

      // We don't care if it's an internal link, Nx deals with it for us
      if (sourceProject === targetProject) {
        return;
      }

      // We only check between libs
      if (targetProject.type !== 'lib') {
        return;
      }

      if (!sourceProject.data.tags?.includes('meta:legacy')) {
        if (targetProject.data.tags?.includes('meta:legacy')) {
          context.report({
            node,
            messageId: 'onlyImportedBy',
            data: {
              tag: 'legacy',
            },
          });
        }
      }

      if (sourceProject.data.tags?.includes('meta:package')) {
        if (!targetProject.data.tags?.includes('meta:package')) {
          context.report({
            node,
            messageId: 'onlyImportedBy',
            data: {
              tag: 'package',
            },
          });
        }
      }
    }

    return {
      ImportDeclaration(node: TSESTree.ImportDeclaration) {
        run(node);
      },
      ImportExpression(node: TSESTree.ImportExpression) {
        run(node);
      },
      ExportAllDeclaration(node: TSESTree.ExportAllDeclaration) {
        run(node);
      },
      ExportNamedDeclaration(node: TSESTree.ExportNamedDeclaration) {
        run(node);
      },
    };
  },
});

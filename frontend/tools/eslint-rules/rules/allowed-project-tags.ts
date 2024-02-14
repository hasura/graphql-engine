/**
 * This file sets you up with with structure needed for an ESLint rule.
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

import type { AST } from 'jsonc-eslint-parser';
import { ESLintUtils } from '@typescript-eslint/utils';

// NOTE: The rule will be available in ESLint configs as "@nrwl/nx/workspace/allowed-project-tags"
export const RULE_NAME = 'allowed-project-tags';
type MessageIds = 'tagRequired' | 'onlyOne' | 'unknownTag';

type TagData = {
  prefix: string;
  allowedTags: string[];
  allowMultiplePerProject: boolean;
  enforceAtLeastOne: boolean;
};
type Options = [
  {
    tags: Record<string, TagData>;
  }
];

type MessageArgs = {
  tag: string;
  possibilities?: string;
  found?: string;
};

export const rule = ESLintUtils.RuleCreator(() => __filename)<
  Options,
  MessageIds
>({
  name: RULE_NAME,
  meta: {
    type: 'problem',
    docs: {
      description: ``,
      recommended: 'error',
    },
    schema: [
      {
        type: 'object',
        additionalProperties: false,
        properties: {
          tags: {
            type: 'object',
            patternProperties: {
              '.*': {
                type: 'object',
                required: ['prefix', 'allowedTags'],
                properties: {
                  prefix: {
                    type: 'string',
                  },
                  allowMultiplePerProject: {
                    type: 'boolean',
                    default: false,
                  },
                  enforceAtLeastOne: {
                    type: 'boolean',
                    default: false,
                  },
                  allowedTags: {
                    type: 'array',
                    items: {
                      type: 'string',
                    },
                  },
                },
              },
            },
          },
        },
      },
    ],
    messages: {
      tagRequired:
        'A {{tag}} tag is required. It must be one of {{possibilities}}.',
      onlyOne: 'Only one {{tag}} tag should be applied',
      unknownTag:
        'Type "{{found}}" is not in the list of possible {{tag}}. Possible types are {{possibilities}}',
    },
  },
  defaultOptions: [
    {
      tags: {},
    },
  ],
  create(context, options) {
    if (
      !(context.parserServices as any).isJSON ||
      !context.getFilename().endsWith('/project.json')
    ) {
      return {};
    }

    return {
      ['JSONExpressionStatement > JSONObjectExpression'](
        node: AST.JSONObjectExpression
      ) {
        for (let property of node.properties) {
          if (!isTagJsonObjectExpression(property)) {
            continue;
          }
          const tags = extractStringArrayFromAst(property);
          // console.log('found tags: ', tags);

          const reporter: ReportError = (code, data) =>
            context.report({
              loc: property.value.loc,
              messageId: code,
              data,
            });

          validateTags(tags, context.options[0], reporter);
        }
      },
    };
  },
});

type ReportError = (message: MessageIds, data?: MessageArgs) => void;

function formatPrettyList(items: string[]): string {
  return items.map(tag => `"${tag}"`).join(', ');
}

function validateTags(
  tags: string[],
  options: Options[0],
  reporter: ReportError
) {
  Object.entries(options.tags).forEach(([tagGroupName, tagConfig]) =>
    validateTagsForGroup(tagGroupName, tagConfig, tags, reporter)
  );
}

function validateTagsForGroup(
  tagGroupName: string,
  tagConfig: TagData,
  tags: string[],
  reporter: ReportError
) {
  const tagFound = tags
    .filter(it => it.startsWith(tagConfig.prefix))
    .map(scope => scope.substring(tagConfig.prefix.length));

  if (tagConfig.enforceAtLeastOne) {
    if (tagFound.length === 0) {
      reporter('tagRequired', {
        tag: tagGroupName,
        possibilities: formatPrettyList(tagConfig.allowedTags),
      });
    }
  }

  if (tagConfig.allowMultiplePerProject) {
    if (tagFound.length > 1) {
      reporter('onlyOne', {
        tag: tagGroupName,
        possibilities: formatPrettyList(tagConfig.allowedTags),
      });
    }
  }

  checkIfAllTagsExists(
    tagFound,
    tagConfig.allowedTags,
    reporter,
    'unknownTag',
    tagGroupName
  );
}

function checkIfAllTagsExists(
  tags: string[],
  allowedTags: string[],
  reporter: ReportError,
  messageId: MessageIds,
  tagGroupName: string
) {
  if (!tags.every(tag => allowedTags.includes(tag))) {
    const unknownTags = tags.filter(tag => !allowedTags.includes(tag));

    for (let tag of unknownTags) {
      reporter(messageId, {
        found: tag,
        possibilities: formatPrettyList(allowedTags),
        tag: tagGroupName,
      });
    }
  }
}

function extractStringArrayFromAst(node: AST.JSONProperty): string[] {
  if (node.value.type !== 'JSONArrayExpression') {
    return [];
  }

  return node.value.elements.map(element => {
    if (element.type !== 'JSONLiteral') {
      return undefined;
    }
    return `${element.value}`;
  });
}

function isTagJsonObjectExpression(node: AST.JSONProperty): boolean {
  return node.key.type === 'JSONLiteral' && node.key.value === 'tags';
}

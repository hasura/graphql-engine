import React, { useEffect } from 'react';
import { Flex } from '@radix-ui/themes';
import { DropdownButton, DropdownMenu, Text } from '@hasura/shared/ui';
import { parseQueryString } from './utils';
import { getLSItem } from '@hasura/shared/utils';
import { LS_KEYS } from '@hasura/shared/types';

interface Operation {
  name: string;
  query: string;
}
const quickOperations: Operation[] = [
  {
    name: 'Introspection query',
    query: `query IntrospectionQuery {
        __schema {
          queryType { name }
          mutationType { name }
          subscriptionType { name }
          types {
            ...FullType
          }
          directives {
            name
            description
            locations
            args {
              ...InputValue
            }
          }
        }
      }

      fragment FullType on __Type {
        kind
        name
        description
        fields(includeDeprecated: true) {
          name
          description
          args {
            ...InputValue
          }
          type {
            ...TypeRef
          }
          isDeprecated
          deprecationReason
        }
        inputFields {
          ...InputValue
        }
        interfaces {
          ...TypeRef
        }
        enumValues(includeDeprecated: true) {
          name
          description
          isDeprecated
          deprecationReason
        }
        possibleTypes {
          ...TypeRef
        }
      }

      fragment InputValue on __InputValue {
        name
        description
        type { ...TypeRef }
        defaultValue
      }

      fragment TypeRef on __Type {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                    ofType {
                      kind
                      name
                    }
                  }
                }
              }
            }
          }
        }
      }`,
  },
];

interface QuickAddProps {
  onAdd: (operation: Operation) => void;
}

export const QuickAdd = (props: QuickAddProps) => {
  const { onAdd } = props;

  const [graphiqlQueries, setGraphiqlQueries] = React.useState<Operation[]>([]);

  useEffect(() => {
    const graphiQlLocalStorage = getLSItem(LS_KEYS.graphiqlQuery);
    if (graphiQlLocalStorage) {
      try {
        setGraphiqlQueries(parseQueryString(graphiQlLocalStorage));
      } catch (error) {
        console.error(error);
      }
    }
  }, []);

  return (
    <Flex justify="end">
      <DropdownButton
        mode="default"
        size="sm"
        items={[...quickOperations, ...graphiqlQueries].map((operation) => {
          return (
            <DropdownMenu.Item
              key={operation.name}
              onSelect={() => onAdd(operation)}
            >
              <div>
                <Text as="p" weight="medium" wrap="nowrap">
                  {operation.name}
                </Text>
                <Text as="p">{operation.query?.slice(0, 40)}...</Text>
              </div>
            </DropdownMenu.Item>
          );
        })}
      >
        Quick Add
      </DropdownButton>
    </Flex>
  );
};

import React, { useEffect } from 'react';
import { Button } from '../../../../new-components/Button';
import { DropdownMenu } from '../../../../new-components/DropdownMenu';
import { FaChevronDown } from 'react-icons/fa';
import { parseQueryString } from './utils';
import { getLSItem, LS_KEYS } from '../../../../utils/localStorage';

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
    <div className="flex justify-end">
      <DropdownMenu
        options={{
          content: {
            className: 'z-[101]',
          },
        }}
        zIndex="z-[102]"
        items={[
          [...quickOperations, ...graphiqlQueries].map(operation => (
            <div
              key={operation.name}
              onClick={() => onAdd(operation)}
              className="cursor-pointer mx-1 px-xs py-xs rounded hover:bg-gray-100"
            >
              <p className="mb-0 font-semibold whitespace-nowrap">
                {operation.name}
              </p>
              <p className="mb-0">{operation.query?.slice(0, 40)}...</p>
            </div>
          )),
        ]}
      >
        <Button
          iconPosition="end"
          size="sm"
          icon={
            <FaChevronDown className="transition-transform group-radix-state-open:rotate-180 w-3 h-3" />
          }
        >
          <span className="font-bold">Quick Add</span>
        </Button>
      </DropdownMenu>
    </div>
  );
};

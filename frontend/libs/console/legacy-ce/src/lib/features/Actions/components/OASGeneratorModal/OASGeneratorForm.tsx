import React, { useEffect } from 'react';
import z from 'zod';
import {
  CodeEditorField,
  FieldWrapper,
  InputField,
  Radio,
} from '@/new-components/Form';
import { FaSearch } from 'react-icons/fa';
import { useFormContext } from 'react-hook-form';
import { useDebouncedEffect } from '@/hooks/useDebounceEffect';
import { Oas3 } from 'openapi-to-graphql';
import YAML from 'js-yaml';

import { Badge, BadgeColor } from '@/new-components/Badge';
import { trackCustomEvent } from '@/features/Analytics';
import { useIsUnmounted } from '@/components/Services/Data';
import { generateAction, getOperations } from './utils';
import { GeneratedAction, Operation } from './types';

export const formSchema = z.object({
  oas: z.string(),
  operation: z.string(),
  url: z.string(),
  search: z.string(),
});

const editorOptions = {
  minLines: 16,
  maxLines: 16,
  showLineNumbers: true,
  useSoftTabs: true,
  showPrintMargin: false,
};

const badgeColors: Record<string, BadgeColor> = {
  GET: 'green',
  POST: 'blue',
  PUT: 'yellow',
  DELETE: 'red',
  PATCH: 'purple',
};
export const OasGeneratorForm = (props: {
  setValues: (values?: GeneratedAction) => void;
}) => {
  const { setValues } = props;

  const [operations, setOperations] = React.useState<Operation[]>([]);
  const [parsedOas, setParsedOas] = React.useState<Oas3 | null>(null);
  const [isOasTooBig, setIsOasTooBig] = React.useState(false);
  const [selectedMethods, setSelectedMethods] = React.useState<string[]>([]);

  const isUnMounted = useIsUnmounted();

  const { watch, setValue, setError, clearErrors } = useFormContext();
  const oas = watch('oas');
  const operation = watch('operation');
  const search = watch('search');
  const url = watch('url');

  const filteredOperations = React.useMemo(() => {
    return operations.filter(op => {
      const searchMatch =
        !search ||
        op.operationId.toLowerCase().includes(search.toLowerCase()) ||
        op.path.toLowerCase().includes(search.toLowerCase()) ||
        op.method.toLowerCase().includes(search.toLowerCase());
      const methodMatch =
        selectedMethods.length === 0 ||
        selectedMethods.includes(op.method.toUpperCase());
      return searchMatch && methodMatch;
    });
  }, [operations, search, selectedMethods]);

  useEffect(() => {
    (async () => {
      if (parsedOas && operation) {
        try {
          const generatedAction = await generateAction(parsedOas, operation);
          if (!isUnMounted()) {
            setValues({ ...generatedAction, baseUrl: url });
          }
        } catch (e) {
          setError('operation', {
            message: `Failed to generate action: ${(e as Error).message}`,
          });
          trackCustomEvent(
            {
              location: 'Import OAS Modal',
              action: 'generate',
              object: 'errors',
            },
            {
              data: {
                errors: (e as Error).message,
              },
            }
          );
          console.error(e);
        }
      }
    })();
  }, [setValues, operation, operations, url, parsedOas, setError, isUnMounted]);

  useDebouncedEffect(
    async () => {
      let localParsedOas: Oas3 | undefined;
      clearErrors();
      setOperations([]);
      if (oas && oas?.trim() !== '') {
        try {
          localParsedOas = JSON.parse(oas) as Oas3;
        } catch (e) {
          try {
            localParsedOas = YAML.load(oas) as Oas3;
          } catch (e2) {
            setError('oas', {
              message: 'Invalid spec',
            });
          }
        }
      }
      try {
        if (localParsedOas) {
          if (!url && localParsedOas.servers?.[0].url) {
            setValue('url', localParsedOas.servers?.[0].url);
          }
          const ops = await getOperations(localParsedOas);
          setOperations(ops);

          // send number of operations and file length

          const size = oas?.length || 0;
          const numberOfOperations = ops?.length || 0;

          trackCustomEvent(
            {
              location: 'Import OAS Modal',
              action: 'change',
              object: 'specification',
            },
            {
              data: {
                size,
                numberOfOperations: numberOfOperations.toString(),
              },
            }
          );
        }
      } catch (e) {
        setError('oas', {
          message: `Invalid spec: ${(e as Error).message}`,
        });
      }

      setParsedOas(localParsedOas ?? null);
    },
    400,
    [oas, clearErrors, setError, setValue, url]
  );

  const handleFileUpload = (e: React.ChangeEvent<HTMLInputElement>) => {
    const files = e.target.files;
    if (files) {
      const reader = new FileReader();
      reader.onload = loadEvent => {
        if (loadEvent.target) {
          // set isOasTooBig to true if the oas is larger than 512kb
          setIsOasTooBig(
            (loadEvent?.target?.result as string)?.length > 512 * 1024
          );
          setValue('oas', loadEvent.target.result);
        }
      };
      // set error
      reader.onerror = () => {
        setError('oas', {
          message: 'Invalid spec',
        });
      };

      reader.readAsText(files[0]);
    }
  };

  return (
    <div>
      <div>
        <div>
          <FieldWrapper
            label="Upload a YAML or JSON file"
            id="file"
            tooltip="Upload a YAML or JSON file containing your OpenAPI specification to fill up the form below."
          >
            <div className="mb-4">
              <input
                data-testid="file"
                type="file"
                id="file"
                aria-invalid="false"
                aria-label="upload open api specification"
                onChange={handleFileUpload}
                accept=".json,.yaml,.yml"
              />
            </div>
          </FieldWrapper>

          {isOasTooBig ? (
            <div className="mb-8">
              <Badge color="yellow">File is too big to show</Badge>
            </div>
          ) : (
            <div className="h-96 mb-4" data-testid="oas-editor">
              <CodeEditorField
                noErrorPlaceholder
                name="oas"
                label="Or paste an Open API Specification"
                tooltip="Enter a sample request in JSON or YAML format to generate the input type"
                editorOptions={editorOptions}
                editorProps={{
                  className: 'rounded`-r-none',
                }}
              />
            </div>
          )}
        </div>
        {operations?.length > 0 ? (
          <div className="flex flex-col">
            <FieldWrapper
              label="Configure server URL"
              id="url"
              className="-mt-4 -mb-6"
            >
              <InputField
                size="medium"
                name="url"
                type="text"
                placeholder="http://example.com"
              />
            </FieldWrapper>
            <FieldWrapper label="Search" id="search" className="-mb-6">
              <InputField
                size="medium"
                name="search"
                type="text"
                placeholder="Search endpoints..."
                icon={<FaSearch />}
              />
              {/* add badges to filter based on http method */}
              <div className="flex flex-row space-x-2 -mt-2 mb-4">
                {Object.keys(badgeColors).map(method => (
                  <Badge
                    onClick={() => {
                      if (selectedMethods.includes(method)) {
                        setSelectedMethods(
                          selectedMethods.filter(m => m !== method)
                        );
                      } else {
                        setSelectedMethods([...selectedMethods, method]);
                      }
                    }}
                    data-testid={`badge-${method}`}
                    key={method}
                    color={
                      selectedMethods.includes(method)
                        ? badgeColors[method]
                        : 'gray'
                    }
                    className="cursor-pointer"
                  >
                    {method}
                  </Badge>
                ))}
              </div>
            </FieldWrapper>

            <Radio
              name="operation"
              label="Choose an endpoint:"
              orientation="vertical"
              tooltip="Choose an endpoint to generate the input type"
              options={[
                ...filteredOperations.slice(0, 50).map(op => ({
                  label: (
                    <div className="text-bold my-1">
                      <Badge
                        color={badgeColors[op.method.toUpperCase()]}
                        className="text-xs inline-flex w-16 justify-center mr-2"
                      >
                        {op.method.toUpperCase()}
                      </Badge>
                      {op.path}
                    </div>
                  ),
                  value: op.operationId,
                })),
              ]}
            />
            {filteredOperations.length > 50 && (
              <div className="text-sm text-gray-500 mb-2">
                {filteredOperations.length - 50} more endpoints... Use search to
                filter them
              </div>
            )}
            {filteredOperations.length === 0 && (
              <div className="text-sm text-gray-500 mb-2">
                No endpoints found. Try changing the search or the selected
                method
              </div>
            )}
          </div>
        ) : null}
      </div>
    </div>
  );
};

import React, { useEffect } from 'react';
import YAML from 'js-yaml';
import { Button } from '@/new-components/Button';
import { CardedTable } from '@/new-components/CardedTable';
import { DropdownButton } from '@/new-components/DropdownButton';
import { CodeEditorField, InputField } from '@/new-components/Form';
import { FaFilter, FaSearch } from 'react-icons/fa';
import { trackCustomEvent } from '@/features/Analytics';
import { useDebouncedEffect } from '@/hooks/useDebounceEffect';
import { Badge, BadgeColor } from '@/new-components/Badge';
import { Oas3 } from 'openapi-to-graphql';
import { useIsUnmounted } from '@/components/Services/Data/DataSources/CreateDataSource/Neon/useIsUnmounted';
import { useFormContext } from 'react-hook-form';
import { OasGeneratorMoreInfo } from './OASGeneratorMoreInfo';
import { GeneratedAction, Operation } from '../OASGeneratorModal/types';

import { generateAction, getOperations } from '../OASGeneratorModal/utils';
import { UploadFile } from './UploadFile';

export interface OasGeneratorFormProps {
  setValues: (values?: GeneratedAction) => void;
}

const fillToTenRows = (data: any[][]) => {
  const rowsToFill = 10 - data.length;
  for (let i = 0; i < rowsToFill; i++) {
    data.push([<div className="h-5" />, '', '']);
  }
  return data;
};
const badgeColors: Record<string, BadgeColor> = {
  GET: 'green',
  POST: 'blue',
  PUT: 'yellow',
  DELETE: 'red',
  PATCH: 'purple',
};

const editorOptions = {
  minLines: 25,
  maxLines: 25,
  showLineNumbers: true,
  useSoftTabs: true,
  showPrintMargin: false,
  showGutter: true,
  wrap: true,
};

export const OasGeneratorForm = (props: OasGeneratorFormProps) => {
  const { setValues } = props;

  const [operations, setOperations] = React.useState<Operation[]>([]);
  const [parsedOas, setParsedOas] = React.useState<Oas3 | null>(null);
  const [selectedMethods, setSelectedMethods] = React.useState<string[]>([]);

  const isUnMounted = useIsUnmounted();

  const {
    watch,
    setValue,
    setError,
    clearErrors,
    register,
    trigger,
    formState,
  } = useFormContext();
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

  const columns = operations?.length
    ? [null, 'Method', 'Endpoint']
    : [
        <span className="normal-case font-normal tracking-normal">
          2. All available endpoints will be listed here after the import
          completes
        </span>,
      ];

  useEffect(() => {
    (async () => {
      if (parsedOas && operation) {
        try {
          const generatedAction = await generateAction(parsedOas, operation);
          if (!isUnMounted()) {
            await trigger('url');
            if (formState.isValid) {
              setValues({ ...generatedAction, baseUrl: url });
            } else {
              setValues(undefined);
            }
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
            setOperations([]);
          }
        }
      }
      try {
        if (localParsedOas) {
          if (!url && localParsedOas.servers?.[0].url) {
            setValue('url', localParsedOas.servers?.[0].url);
          } else {
            trigger('url');
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
    <div className="flex flex-col gap-4">
      <div className="flex justify-between">
        <div>
          <UploadFile onChange={handleFileUpload} />
        </div>
        <div className="flex w-1/2 pl-[10px]">
          <InputField
            icon={<FaSearch />}
            placeholder="Available endpoints..."
            name="search"
            inputClassName="rounded-r-none"
            className="mb-0"
            noErrorPlaceholder
          />
          <DropdownButton
            className="rounded-l-none"
            size="md"
            data-testid="dropdown-button"
            items={[
              Object.keys(badgeColors).map(method => (
                <div
                  className="py-1"
                  onClick={() => {
                    if (selectedMethods.includes(method)) {
                      setSelectedMethods(
                        selectedMethods.filter(m => m !== method)
                      );
                    } else {
                      setSelectedMethods([...selectedMethods, method]);
                    }
                  }}
                >
                  <div className="flex items-center">
                    <input
                      type="checkbox"
                      className="mr-2 border border-gray-300 rounded"
                      checked={selectedMethods.includes(method)}
                    />
                    <div>{method.toUpperCase()}</div>
                  </div>
                </div>
              )),
            ]}
          >
            <FaFilter className="mr-1 w-3 h-3" /> Method{' '}
            {selectedMethods.length > 0 && `(${selectedMethods.length})`}
          </DropdownButton>
        </div>
      </div>
      <div className="grid grid-cols-2 gap-6">
        <div>
          <div className="h-[400px] mb-4" data-testid="oas-editor">
            <CodeEditorField
              noErrorPlaceholder
              name="oas"
              placeholder="1. Paste OpenAPI spec in raw text (JSON / YAML) here"
              editorOptions={editorOptions}
              editorProps={{
                className: 'rounded`-r-none',
              }}
            />
          </div>
        </div>
        <div>
          <CardedTable
            rowClassNames={filteredOperations.map(op => {
              if (op.operationId === operation) {
                return 'bg-gray-100';
              }
              return '';
            })}
            className="h-[400px] relative"
            columns={[...columns]}
            data={fillToTenRows(
              filteredOperations.map(op => [
                <input
                  className="pointer-events-auto"
                  {...register('operation')}
                  type="radio"
                  data-testid={`operation-${op.operationId}`}
                  value={op.operationId}
                  id={op.operationId}
                />,
                <Badge
                  color={badgeColors[op.method.toUpperCase()]}
                  className="text-xs inline-flex w-16 justify-center mr-2"
                >
                  {op.method.toUpperCase()}
                </Badge>,
                <div>
                  <OasGeneratorMoreInfo operation={op} />
                </div>,
              ])
            )}
          />
        </div>
      </div>
      <div>
        <div className="mt-xs">
          <h4 className="text-lg font-semibold mb-xs flex items-center mb-0">
            Base URL
          </h4>
          <InputField
            size="medium"
            placeholder="http://swagger.io/v2"
            name="url"
            inputClassName="rounded-r-none"
            className="mb-0 mt-xs"
            noErrorPlaceholder
          />
        </div>
      </div>
      <div className="flex justify-end gap-3">
        <Button size="md" mode="default">
          Cancel
        </Button>
        <Button
          type="submit"
          size="md"
          mode="primary"
          disabled={!formState.isValid}
        >
          Generate Action
        </Button>
      </div>
    </div>
  );
};

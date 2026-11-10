import { z } from 'zod';
import {
  CheckboxField,
  Dialog,
  DialogFooter,
  InputField,
  RadioGroupField,
  useConsoleForm,
} from '@hasura/shared/ui';
import { LS_KEYS } from '@hasura/shared/types';
import {
  CSVExportOptions,
  DEFAULT_CSV_EXPORT_OPTIONS,
  getLSItem,
  setLSItem,
} from '@hasura/shared/utils';

const DELIMITERS = {
  comma: ',',
  semicolon: ';',
  tab: '\t',
  pipe: '|',
} as const;

type PresetDelimiter = keyof typeof DELIMITERS;

const schema = z
  .object({
    delimiter: z.enum(['comma', 'semicolon', 'tab', 'pipe', 'custom']),
    customDelimiter: z.string(),
    includeHeader: z.boolean(),
    includeBOM: z.boolean(),
    lineEnding: z.enum(['LF', 'CRLF']),
    quoteAllCells: z.boolean(),
    escapeFormulas: z.boolean(),
    nullValue: z.string(),
  })
  .superRefine((values, ctx) => {
    if (values.delimiter !== 'custom') return;
    if (!values.customDelimiter) {
      ctx.addIssue({
        code: 'custom',
        path: ['customDelimiter'],
        message: 'Enter a delimiter',
      });
    } else if (/["\r\n]/.test(values.customDelimiter)) {
      ctx.addIssue({
        code: 'custom',
        path: ['customDelimiter'],
        message: 'A delimiter cannot contain double quotes or line breaks',
      });
    }
  });

export type ExportCSVFormValues = z.infer<typeof schema>;

const presetFor = (delimiter: string): PresetDelimiter | undefined =>
  (Object.keys(DELIMITERS) as PresetDelimiter[]).find(
    (key) => DELIMITERS[key] === delimiter,
  );

const toFormValues = (options: CSVExportOptions): ExportCSVFormValues => {
  const preset = presetFor(options.delimiter);
  return {
    ...options,
    delimiter: preset ?? 'custom',
    customDelimiter: preset ? '' : options.delimiter,
  };
};

export const toCSVExportOptions = ({
  delimiter,
  customDelimiter,
  ...rest
}: ExportCSVFormValues): CSVExportOptions => ({
  ...rest,
  delimiter: delimiter === 'custom' ? customDelimiter : DELIMITERS[delimiter],
});

/** The last options used, or the defaults. Never throws. */
export const getStoredCSVExportOptions = (): ExportCSVFormValues => {
  const defaults = toFormValues(DEFAULT_CSV_EXPORT_OPTIONS);
  try {
    const stored = getLSItem(LS_KEYS.dataCSVExportOptions);
    if (!stored) return defaults;
    const parsed = schema.safeParse({ ...defaults, ...JSON.parse(stored) });
    return parsed.success ? parsed.data : defaults;
  } catch {
    return defaults;
  }
};

const storeCSVExportOptions = (values: ExportCSVFormValues) => {
  try {
    setLSItem(LS_KEYS.dataCSVExportOptions, JSON.stringify(values));
  } catch {
    // storage unavailable (e.g. private mode): the export still goes ahead
  }
};

type ExportCSVDialogProps = {
  /** Shown in the title, e.g. "selected rows" or "table". */
  scope: string;
  onExport: (options: CSVExportOptions) => void;
  onClose: () => void;
};

export const ExportCSVDialog = ({
  scope,
  onExport,
  onClose,
}: ExportCSVDialogProps) => {
  const {
    Form,
    methods: { handleSubmit, watch },
  } = useConsoleForm({
    schema,
    options: { defaultValues: getStoredCSVExportOptions() },
  });

  const delimiter = watch('delimiter');

  const onSubmit = (values: ExportCSVFormValues) => {
    storeCSVExportOptions(values);
    onExport(toCSVExportOptions(values));
  };

  return (
    <Dialog
      size="sm"
      title={`Export ${scope} as CSV`}
      onClose={onClose}
      onOpenChange={(open) => {
        if (!open) onClose();
      }}
      footer={
        <DialogFooter
          onSubmit={() => {
            handleSubmit(onSubmit)();
          }}
          onClose={onClose}
          callToDeny="Cancel"
          callToAction="Export"
        />
      }
    >
      <Form onSubmit={onSubmit}>
        <RadioGroupField
          name="delimiter"
          label="Delimiter"
          orientation="horizontal"
          options={[
            { value: 'comma', label: 'Comma (,)' },
            { value: 'semicolon', label: 'Semicolon (;)' },
            { value: 'tab', label: 'Tab' },
            { value: 'pipe', label: 'Pipe (|)' },
            { value: 'custom', label: 'Custom' },
          ]}
        />
        {delimiter === 'custom' && (
          <InputField
            name="customDelimiter"
            label="Custom delimiter"
            fieldProps={{ placeholder: 'e.g. ~' }}
          />
        )}
        <RadioGroupField
          name="lineEnding"
          label="Line endings"
          orientation="horizontal"
          options={[
            { value: 'LF', label: 'LF (macOS, Linux)' },
            { value: 'CRLF', label: 'CRLF (Windows)' },
          ]}
        />
        <InputField
          name="nullValue"
          label="Null values as"
          description="Leave empty to write null values as empty cells"
          fieldProps={{ placeholder: 'e.g. NULL' }}
        />
        <CheckboxField name="includeHeader">
          Include a header row with the column names
        </CheckboxField>
        <CheckboxField name="quoteAllCells">
          Quote every cell, not only those that need it
        </CheckboxField>
        <CheckboxField
          name="includeBOM"
          description="Lets Excel detect UTF-8, so non-ASCII characters show correctly"
        >
          Add a UTF-8 byte order mark (BOM)
        </CheckboxField>
        <CheckboxField
          name="escapeFormulas"
          description="Prefixes text starting with =, +, -, @ or a tab with an apostrophe, so spreadsheets do not run it as a formula"
        >
          Escape values that look like formulas
        </CheckboxField>
      </Form>
    </Dialog>
  );
};

import AceEditor from 'react-ace';
import { Controller, useFormContext } from 'react-hook-form';
import { FieldWrapper } from '../../../../../new-components/Form';

export function SqlEditorField() {
  const { control } = useFormContext();
  return (
    <Controller
      name="code"
      control={control}
      render={({ field, fieldState }) => (
        <FieldWrapper
          error={fieldState.error}
          className="w-full"
          label="Native Query Statement"
        >
          <AceEditor
            mode="sql"
            theme={'eclipse'}
            fontSize={14}
            minLines={15}
            maxLines={100}
            width="100%"
            showPrintMargin={false} // prevents unwanted frequent event triggers
            debounceChangePeriod={200}
            setOptions={{
              useWorker: false,
            }}
            {...field}
          />
        </FieldWrapper>
      )}
    />
  );
}

import { DialogFooter } from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import { MdRefresh } from 'react-icons/md';
import { EnvVarsFormState } from '../../../types';

type Props = { formState: EnvVarsFormState };

export function CustomFooter(props: Props) {
  const { formState: state } = props;

  const buttonText = {
    default: 'Set Environment Variables',
    loading: 'Setting Environment Variables...',
    error: 'Retry Setting Environment Variables',
    hidden: '',
  };

  return (
    <Analytics
      name="one-click-deployment-env-var-form-submit"
      passHtmlAttributesToChildren
    >
      <DialogFooter
        callToAction={buttonText[state]}
        callToActionProps={{
          leftIcon: state === 'error' ? MdRefresh : undefined,
          loadingText: buttonText.loading,
        }}
        disabled={state === 'loading'}
        isLoading={state === 'loading'}
        onClose={() => {}}
      />
    </Analytics>
  );
}

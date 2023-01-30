import { CustomFieldNamesForm } from './CustomFieldNamesForm';
import { CustomFieldNamesModal } from './CustomFieldNamesModal';
import { LegacyWrapper } from './LegacyWrapper';

export type { CustomFieldNamesFormProps } from './CustomFieldNamesForm';
export type { CustomFieldNamesModalProps } from './CustomFieldNamesModal';

export const CustomFieldNames = {
  Form: CustomFieldNamesForm,
  Modal: CustomFieldNamesModal,
  LegacyModal: LegacyWrapper,
};

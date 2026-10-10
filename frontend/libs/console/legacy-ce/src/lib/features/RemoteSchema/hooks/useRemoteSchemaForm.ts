import { useState } from 'react';
import type {
  RemoteSchema,
  RemoteSchemaCustomization,
} from '@hasura/shared/types';

export type FormState = {
  manualUrl: string | null;
  envName: string | null;
  timeoutConf: string;
  name: string;
  forwardClientHeaders: boolean;
  // editState: EditState;
  comment?: string;
  customization?: RemoteSchemaCustomization;
};

export type EditState = {
  id: number;
  isModify: boolean;
  originalName: string;
  originalHeaders: any[];
  originalUrl: string;
  originalEnvUrl: string;
  originalTimeoutConf: string;
  originalForwardClientHeaders: boolean;
  originalComment?: string;
  originalCustomization?: RemoteSchemaCustomization;
};

const createFormState = (existingRemoteSchema?: RemoteSchema): FormState => ({
  name: existingRemoteSchema?.name ?? '',
  manualUrl:
    existingRemoteSchema && 'url' in existingRemoteSchema?.definition
      ? existingRemoteSchema.definition.url
      : null,
  envName:
    existingRemoteSchema && 'url_from_env' in existingRemoteSchema?.definition
      ? existingRemoteSchema.definition.url_from_env
      : null,
  timeoutConf: existingRemoteSchema?.definition.timeout_seconds
    ? existingRemoteSchema.definition.timeout_seconds.toString()
    : '60',
  forwardClientHeaders:
    existingRemoteSchema?.definition.forward_client_headers ?? false,
  comment: existingRemoteSchema?.comment || '',
  customization: existingRemoteSchema?.definition?.customization,
  // editState: {
  //   id: -1,
  //   isModify: true,
  //   originalName: '',
  //   originalHeaders: [],
  //   originalUrl: '',
  //   originalEnvUrl: '',
  //   originalTimeoutConf: '',
  //   originalForwardClientHeaders: false,
  //   originalComment: '',
  //   originalCustomization: undefined,
  // },
});

const useRemoteSchemaForm = () => {
  const [formState, setFormState] = useState(createFormState());

  const toggleForwardClientHeaders = () => {
    setFormState((prev) => ({
      ...prev,
      forwardClientHeaders: !prev.forwardClientHeaders,
    }));
  };

  const setCustomization = (customization: RemoteSchemaCustomization) => {
    setFormState((prev) => ({
      ...prev,
      customization,
    }));
  };

  const setName = (name: string) => {
    setFormState((prev) => ({
      ...prev,
      name,
    }));
  };

  const setComment = (comment: string) => {
    setFormState((prev) => ({
      ...prev,
      comment,
    }));
  };

  const setTimeoutConf = (timeoutConf: string) => {
    setFormState((prev) => ({
      ...prev,
      timeoutConf,
    }));
  };

  const setEnvURL = (envName: string) => {
    setFormState((prev) => ({
      ...prev,
      envName,
      manualUrl: '',
    }));
  };

  const setManualURL = (manualUrl: string) => {
    setFormState((prev) => ({
      ...prev,
      manualUrl,
      envName: '',
    }));
  };

  return {
    formState,
    setFormState,
    toggleForwardClientHeaders,
    setCustomization,
    setComment,
    setTimeoutConf,
    setEnvURL,
    setName,
    setManualURL,
  };
};

export default useRemoteSchemaForm;

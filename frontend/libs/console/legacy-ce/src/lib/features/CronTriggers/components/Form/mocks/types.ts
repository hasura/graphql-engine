import { CronTrigger } from '../../../../../metadata/types';

type ResponseBodyOnSuccess = {
  message: 'success';
};

export type ResponseBodyMetadataTypeError = {
  code: string;
  error: string;
  path: string;
};

export type ListCronTriggerAPIResponse = {
  cron_triggers: CronTrigger[];
};

type BulkApiPayload = {
  type: 'bulk';
  args: (CreateCronTriggerApiPayload | DeleteCronTriggerApiPayload)[];
};

type CreateCronTriggerApiPayload = {
  type: 'create_cron_trigger';
  args: CronTrigger;
};

type DeleteCronTriggerApiPayload = {
  type: 'delete_cron_trigger';
  args: {
    name: string;
  };
};

type GetCronTriggersApiPayload = {
  type: 'get_cron_triggers';
  args: Record<string, never>;
};

export type CronRequestBody =
  | BulkApiPayload
  | GetCronTriggersApiPayload
  | CreateCronTriggerApiPayload
  | DeleteCronTriggerApiPayload
  | Record<string, unknown>;

export type CronResponseBody =
  | ResponseBodyOnSuccess
  | ResponseBodyMetadataTypeError
  | ListCronTriggerAPIResponse;

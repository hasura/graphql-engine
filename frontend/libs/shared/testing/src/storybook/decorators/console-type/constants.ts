import { ConsoleTypes } from './types';

export const consoleTypeDropDownArray: {
  value: ConsoleTypes;
  label: string;
}[] = [
  { value: 'oss', label: 'OSS' },
  { value: 'pro-lite', label: 'EE Lite' },
  { value: 'pro', label: 'EE' },
  { value: 'cloud', label: 'Cloud' },
  { value: 'cloud-pro', label: 'Cloud EE' },
];

import { format } from 'date-fns';

export const convertDateTimeToLocale = (dateTime: string | Date | number) => {
  return format(new Date(dateTime), 'EEE, MMM, yyyy, do HH:mm:ss xxx');
};

export function safeParseInt<
  T extends number | null | undefined = number | null | undefined,
>(value: string, fallbackValue: T): T {
  try {
    return Number.parseInt(value) as T;
  } catch {
    return fallbackValue;
  }
}

export function safeParseFloat<
  T extends number | null | undefined = number | null | undefined,
>(value: string, fallbackValue: T): T {
  try {
    return Number.parseFloat(value) as T;
  } catch {
    return fallbackValue;
  }
}

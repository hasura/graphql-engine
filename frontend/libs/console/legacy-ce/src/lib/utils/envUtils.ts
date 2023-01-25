export type ConsoleType = 'oss' | 'cloud' | 'pro' | 'pro-lite';

export function parseConsoleType(envConsoleType: unknown): ConsoleType {
  switch (envConsoleType) {
    case 'oss':
    case 'cloud':
    case 'pro':
    case 'pro-lite':
      return envConsoleType;

    default:
      throw new Error(`Unmanaged console type "${envConsoleType}"`);
  }
}

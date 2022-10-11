export type AllowedConsoleTypes = 'oss' | 'pro' | 'cloud' | 'pro-lite';
export type AllowedConsoleModes = 'cli' | 'server';

export type Privileges = string[];
export interface AuthService {
  isAuthenticated: boolean;
  consoleType: AllowedConsoleTypes;
  consoleMode: AllowedConsoleModes;
  isCollaborator: () => boolean;
  isProjectAdmin: () => boolean;
  hasPrivilege: (privilege: Privileges) => boolean;
}

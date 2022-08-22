import {
  AllowedConsoleModes,
  AllowedConsoleTypes,
  AuthService,
} from './AuthService';

export class OSSAuthService implements AuthService {
  isAuthenticated: boolean;
  consoleType: AllowedConsoleTypes;
  consoleMode: AllowedConsoleModes;
  adminSecret?: string | null;

  constructor({
    consoleType,
    consoleMode,
  }: {
    consoleType: AllowedConsoleTypes;
    consoleMode: AllowedConsoleModes;
  }) {
    this.consoleType = consoleType;
    this.consoleMode = consoleMode;
    this.isAuthenticated = false;
  }

  // Login route already verifies this for us, this method stores the result of post-login
  authenticate = (adminSecret: string | null) => {
    this.adminSecret = adminSecret;
    this.isAuthenticated = true;
  };

  isProjectAdmin = () => true;

  isCollaborator = () => false;

  hasPrivilege = () => false;
}

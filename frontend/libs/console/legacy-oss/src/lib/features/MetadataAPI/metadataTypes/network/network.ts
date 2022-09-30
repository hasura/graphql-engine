export interface DomainList {
  host: string;
  suffix?: string;
  perms?: string[];
}

export type Network = { tls_allowlist?: DomainList[] };

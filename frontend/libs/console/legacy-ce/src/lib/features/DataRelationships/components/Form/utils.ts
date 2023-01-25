export type RelOption = 'local' | 'remoteDatabase' | 'remoteSchema';

export const formTabs: { value: RelOption; title: string; body: string }[] = [
  {
    value: 'local',
    title: 'Local Relationship',
    body: 'Relationships from this table to a local database table.',
  },
  {
    value: 'remoteDatabase',
    title: 'Remote Database Relationship',
    body: 'Relationship from this local table to a remote database table.',
  },
  {
    value: 'remoteSchema',
    title: 'Remote Schema Relationship',
    body: 'Relationship from this local table to a remote schema.',
  },
];

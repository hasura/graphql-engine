export default {
  name: ['Chinook', 'Artist'],
  type: 'table',
  columns: [
    {
      name: 'ArtistId',
      type: 'Int',
      nullable: false,
      description: '',
      insertable: true,
      updatable: true,
    },
    {
      name: 'Name',
      type: 'String',
      nullable: true,
      description: '',
      insertable: true,
      updatable: true,
    },
  ],
  primary_key: ['ArtistId'],
  description: '',
  insertable: true,
  updatable: true,
  deletable: true,
};

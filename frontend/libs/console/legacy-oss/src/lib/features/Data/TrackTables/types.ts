import { MetadataTable, Table } from '@/features/DataSource';

export type TrackableTable = {
  /**
   *	Useful for ID'ing unique table rows for checkboxes and solo-updates for
   *	individual rows.
   */
  id: string;

  /**
   *	Self explanatory. Used for the display name.
   */
  name: string;

  /**
   *	A `table` represent the generic json that is used by metadata to reference
   *	a tracked table. Its best to not mess with this object and just treat it like
   *	a black box, passing it back to the server while tracking/untracking
   */
  table: Table;

  /**
   *	Right now, we don't have a strong use case for the UI, but it's handy to have this info
   *	while tracking tables, Like put it on badge under the table. Maybe could be even
   *	used for filtering in the future versions.
   */
  type: string;
} & (
  | {
      /* Represent whether a table is tracked or not, pretty self explanatory */
      is_tracked: false;
    }
  | {
      is_tracked: true;

      /**
       * Configuration data for a tracked table. Should be applicable only after the
       * table is tracked.
       */
      configuration?: MetadataTable['configuration'];
    }
);

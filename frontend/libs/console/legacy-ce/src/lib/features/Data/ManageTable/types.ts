import { MetadataTable, Table } from '../../hasura-metadata-types';

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

  is_tracked: boolean;

  /**
   * Configuration data for a table.
   * Can be present if adding configurationg and tracking an untracked table or if tracked table has configuraiton data
   * In other words, the UI needs this property for is_tracked: false objects so
   * configuration can be added prior to applying changes
   */
  configuration?: MetadataTable['configuration'];
};

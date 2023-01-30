import {
  getRemoteSchemaNameFromInconsistentObjects,
  getSourceFromInconistentObjects,
} from '../utils';
import {
  inconsistentObject,
  inconsistentRelationshipObject,
  inconsistentRemoteRelation,
  inconsistentRemoteSchema,
  inconsistentSourceObjects,
  inconsistentTableObject,
  multipleInconsistencyRemoteSchema,
} from './fixtures/input';

describe('Metadata_Utils_getSourceFromInconistentObjects()', () => {
  it('should generate an array of soure name for inconsistent DB ', () => {
    const sourceNameArray = getSourceFromInconistentObjects(
      inconsistentSourceObjects
    );
    expect(sourceNameArray).toEqual(['TestDB']);
  });
  it('should generate an array of soure name from inconsistent object due to table deletion ', () => {
    const sourceNameArray = getSourceFromInconistentObjects(
      inconsistentTableObject
    );
    expect(sourceNameArray).toEqual([]);
  });
  it('should generate an array of soure name from inconsistent object due to inconsistent relation ', () => {
    const sourceNameArray = getSourceFromInconistentObjects(
      inconsistentRelationshipObject
    );
    expect(sourceNameArray).toEqual(['default']);
  });
  it('should generate an array of soure name from inconsistent object due to inconsistent relation, DB and table deletion ', () => {
    const sourceNameArray = getSourceFromInconistentObjects(inconsistentObject);
    expect(sourceNameArray).toEqual(['TestDB', 'default']);
  });
});

describe('Metadata_Utils_getRemoteSchemaNameFromInconsistentObjects()', () => {
  it('should generate an array of remote schema name for inconsistent remote_schema, remote_schema_permission, remote_schema_relation ', () => {
    const rsNameArray = getRemoteSchemaNameFromInconsistentObjects(
      inconsistentRemoteSchema
    );
    expect(rsNameArray).toEqual(['Test1', 'Test2']);
  });
  it('should generate an array of remote schema name for inconsistent remote_relation ', () => {
    const rsNameArray = getRemoteSchemaNameFromInconsistentObjects(
      inconsistentRemoteRelation
    );
    expect(rsNameArray).toEqual(['Test']);
  });
  it('should generate an array of a single remote schema name for multiple inconsistency in a remote_schema ', () => {
    const rsNameArray = getRemoteSchemaNameFromInconsistentObjects(
      multipleInconsistencyRemoteSchema
    );
    expect(rsNameArray).toEqual(['Test']);
  });
});

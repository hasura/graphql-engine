import produce from 'immer';
import {
  LogicalModel,
  LogicalModelField,
  NativeQuery,
  NativeQueryRelationship,
  StoredProcedure,
  isArrayLogicalModelType,
  isLogicalModelType,
} from '../../hasura-metadata-types';
import { isFieldImplementingLogicalModel } from './LogicalModel/utils/findReferencedEntities';
import cloneDeep from 'lodash/cloneDeep';

type CommonParams = {
  dataSourceName: string;
  driver: string;
};

type SupportedTypes = NativeQuery | LogicalModel | StoredProcedure;
type MetadataCommandFragments =
  | 'logical_model'
  | 'native_query'
  | 'stored_procedure';
interface MigrationPayloadBuilderParams<T extends SupportedTypes> {
  entity: T;
  commandEntity: MetadataCommandFragments;
  entityKey: keyof T;
}

/**
 * This class lets you build a sequence of metadata commands for bulk_atomic
 *
 * The track() / untrack() methods should be used to add commands to the sequence
 *
 * The other methods mutate the nativeQuery initially passed in that will get used in the track() command
 */
export class MigrationPayloadBuilder<T extends SupportedTypes> {
  protected payloadSequence: Record<string, unknown>[] = [];
  protected source: string;
  protected driver: string;
  protected entity: T;
  protected commandEntity: MetadataCommandFragments;
  protected entityKey: keyof T;

  constructor({
    dataSourceName,
    driver,
    entity,
    commandEntity,
    entityKey,
  }: CommonParams & MigrationPayloadBuilderParams<T>) {
    if (!dataSourceName) {
      throw new Error('Source is required');
    }
    if (!driver) {
      throw new Error('Driver is required');
    }
    if (!entity) {
      throw new Error('Native Query is required');
    }

    if (!commandEntity) {
      throw new Error('Command is required');
    }

    this.source = dataSourceName;
    this.driver = driver;

    // copying object to prevent weird issues with references so mutations to entity within the class don't end up being made to the referenced object
    this.entity = cloneDeep(entity);

    this.commandEntity = commandEntity;
    this.entityKey = entityKey;

    return this;
  }

  // can update with some or all propeties
  updateEntity(newEntity: Partial<T>): this {
    this.entity = { ...this.entity, ...newEntity };
    return this;
  }

  /**
   *
   * @param entityKey Optional: can provide the name or it will default to the name of the entity passed into the constructor
   */
  untrack(entityKey?: string): this {
    this.payloadSequence = [
      ...this.payloadSequence,
      {
        type: `${this.driver}_untrack_${this.commandEntity}`,
        args: {
          source: this.source,
          [this.entityKey]: entityKey ?? this.entity[this.entityKey],
        },
      },
    ];
    return this;
  }

  track(): this {
    this.payloadSequence = [
      ...this.payloadSequence,
      {
        type: `${this.driver}_track_${this.commandEntity}`,
        args: {
          source: this.source,
          ...this.entity,
        },
      },
    ];
    return this;
  }

  // returns the payload sequence
  payload(): Record<string, unknown>[] {
    return this.payloadSequence;
  }
}

export class NativeQueryMigrationBuilder extends MigrationPayloadBuilder<NativeQuery> {
  constructor({
    dataSourceName,
    driver,
    nativeQuery,
  }: CommonParams & {
    nativeQuery: NativeQuery;
  }) {
    super({
      dataSourceName,
      driver,
      commandEntity: 'native_query',
      entity: nativeQuery,
      entityKey: 'root_field_name',
    });

    this.initializeRelationshipArrays();

    return this;
  }

  private initializeRelationshipArrays() {
    if (!this.entity.array_relationships) {
      this.entity.array_relationships = [];
    }
    if (!this.entity.object_relationships) {
      this.entity.object_relationships = [];
    }
  }

  updateLogicalModel(name: string) {
    this.entity.returns = name;
    return this;
  }

  addRelationship(
    type: 'object' | 'array',
    relationshipDetails: NativeQueryRelationship
  ): this {
    const relationshipsKey =
      type === 'object' ? 'object_relationships' : 'array_relationships';
    // call this just in case it was modified between constructor and here
    this.initializeRelationshipArrays();

    this.entity[relationshipsKey]?.push(relationshipDetails);

    return this;
  }

  // removes any relationships that match a string name
  removeRelationship(type: 'object' | 'array', name: string): this {
    const relationshipsKey =
      type === 'object' ? 'object_relationships' : 'array_relationships';

    // call this just in case it was modified between constructor and here
    this.initializeRelationshipArrays();

    this.entity[relationshipsKey] = this.entity[relationshipsKey]?.filter(
      rel => rel.name !== name
    );

    return this;
  }

  override track(): this {
    // this makes sure it's marked as query which for now is hard coded but not part of the form or UI
    this.entity.type = 'query';

    return super.track();
  }
}

export class LogicalModelMigrationBuilder extends MigrationPayloadBuilder<LogicalModel> {
  constructor({
    dataSourceName,
    driver,
    logicalModel,
  }: CommonParams & { logicalModel: LogicalModel }) {
    super({
      dataSourceName,
      driver,
      entity: logicalModel,
      entityKey: 'name',
      commandEntity: 'logical_model',
    });

    return this;
  }

  addField(field: LogicalModelField): this {
    this.entity.fields.push(field);
    return this;
  }

  // this method updates any fields that refer to an external logical model
  // i.e. the referenced logical model's name is changing and we need to update the field to preserve the reference
  updateFieldsLogicalModelReference({
    currentName,
    newName,
  }: {
    currentName: string;
    newName: string;
  }): this {
    this.entity.fields = this.entity.fields.map(field => {
      const isMatch = isFieldImplementingLogicalModel(field, currentName);

      if (!isMatch) return field;

      return produce(field, draft => {
        if (isLogicalModelType(draft.type)) {
          draft.type.logical_model = newName;
        } else if (isArrayLogicalModelType(draft.type)) {
          draft.type.array.logical_model = newName;
        }
      });
    });

    return this;
  }

  removeField(name: string): this {
    this.entity.fields = this.entity.fields.filter(f => f.name === name);
    return this;
  }
}

export class StoredProcedureMigrationBuilder extends MigrationPayloadBuilder<StoredProcedure> {
  constructor({
    dataSourceName,
    driver,
    storedProcedure,
  }: CommonParams & { storedProcedure: StoredProcedure }) {
    super({
      dataSourceName,
      driver,
      entity: storedProcedure,
      entityKey: 'stored_procedure',
      commandEntity: 'stored_procedure',
    });

    return this;
  }

  updateLogicalModel(name: string) {
    this.entity.returns = name;
    return this;
  }
}

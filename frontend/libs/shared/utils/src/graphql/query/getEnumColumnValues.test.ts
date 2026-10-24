import { buildSchema } from 'graphql';
import { getEnumColumnValues } from './getEnumColumnValues';

const schema = buildSchema(`
  enum task_status_enum { active archived }
  enum TaskPriorityEnum { HIGH LOW }

  type tasks {
    id: Int!
    status: task_status_enum
    taskPriority: TaskPriorityEnum
    title: String
  }

  type shop_query { tasks: [tasks!]! }
  type query_root { tasks: [tasks!]! shop: shop_query }
  schema { query: query_root }
`);

describe('getEnumColumnValues', () => {
  it('returns the enum values of the enum columns, by column name', () => {
    expect(
      getEnumColumnValues({
        schema,
        defaultQueryRoot: 'tasks',
        // `task_priority` is camel-cased by the `graphql-default` convention
        columns: ['id', 'status', 'task_priority', 'title'],
        tableCustomization: undefined,
        sourceCustomization: undefined,
      }),
    ).toEqual({
      status: ['active', 'archived'],
      task_priority: ['HIGH', 'LOW'],
    });
  });

  it('looks the table up in the source namespace', () => {
    expect(
      getEnumColumnValues({
        schema,
        defaultQueryRoot: 'tasks',
        columns: ['status'],
        tableCustomization: undefined,
        sourceCustomization: { root_fields: { namespace: 'shop' } },
      }),
    ).toEqual({ status: ['active', 'archived'] });
  });

  it('returns nothing for a table the schema does not expose', () => {
    expect(
      getEnumColumnValues({
        schema,
        defaultQueryRoot: 'untracked',
        columns: ['status'],
        tableCustomization: undefined,
        sourceCustomization: undefined,
      }),
    ).toEqual({});
  });
});

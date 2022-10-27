import { createFormData } from './useFormData';
import { input } from './mock';

const mockResult: ReturnType<typeof createFormData> = {
  columns: ['ArtistId', 'Name'],
  roles: ['user'],
  supportedQueries: ['insert', 'select', 'update', 'delete'],
  tableNames: [['Album'], ['Artist']],
};

test('returns correctly formatted formData', () => {
  const result = createFormData(input);
  expect(result).toEqual(mockResult);
});

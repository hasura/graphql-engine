/**
 * Return an array containing all the "describe"s title.
 *
 * Ex. return ['Describe 1', 'Describe 2'] in this case
 * describe('Describe 1', () => {
 *   describe('Describe 2', () => {
 *     it('Test', () => {})
 *   })
 * })
 */
export function generateDescribesTitle(
  parent?: Mocha.Suite | undefined
): string[] {
  if (!parent) return [];

  let parentNames: string[] = [];
  // parents are recursive
  if (parent.parent) {
    parentNames = parentNames.concat(generateDescribesTitle(parent.parent));
  }

  if (parent.title !== '') {
    parentNames.push(parent.title);
  }

  return parentNames;
}

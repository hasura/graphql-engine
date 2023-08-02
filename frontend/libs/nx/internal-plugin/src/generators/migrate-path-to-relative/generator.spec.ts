import { transformInputText } from './generator';
import * as ts from 'typescript';

type TransformInputTextCases = {
  import: string;
  folder: string;
  expected: string;
};

const baseFolder = 'libs/console/legacy-ce/src/lib/';
describe('transformInputText', () => {
  const testCases: TransformInputTextCases[] = [
    {
      import: '@/Globals',
      folder: 'myFolder',
      expected: '../Globals',
    },
    {
      import: '@/Globals',
      folder: 'a/b',
      expected: '../../Globals',
    },
    {
      import: '@/new-components/Button',
      folder: 'a/b',
      expected: '../../new-components/Button',
    },
    {
      import: '@/new-components/Button',
      folder: 'a/b/c/d/e/f/g',
      expected: '../../../../../../../new-components/Button',
    },
    {
      import: '@/Globals',
      folder: '',
      expected: './Globals',
    },
    {
      import: '@/my/component',
      folder: 'my',
      expected: './component',
    },
    {
      import: '@/my/component',
      folder: 'my/component',
      expected: '.',
    },
  ];

  for (const testItem of testCases) {
    it(`should transform ${testItem.import} into ${testItem.expected} when in ${testItem.folder} folder`, () => {
      const result = transformInputText(
        ts.factory.createStringLiteral(testItem.import, true),
        baseFolder + testItem.folder
      );

      expect(result).toEqual(`'${testItem.expected}'`);
    });
  }
});

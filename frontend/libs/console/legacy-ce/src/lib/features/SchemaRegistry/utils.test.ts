import { hexToRGB, CapitalizeFirstLetter } from './utils';

type hexToRGBTestCase = {
  hex: string;
  alpha: number;
  rgba: string;
};

type capitalizeFirstLetterTestCase = {
  input: string;
  output: string;
};

const hexToRGBATestCases: hexToRGBTestCase[] = [
  {
    hex: '#ff0044',
    alpha: 3,
    rgba: 'rgba(255, 0, 68, 3)',
  },
  {
    hex: '#d80456',
    alpha: 2,
    rgba: 'rgba(216, 4, 86, 2)',
  },
  {
    hex: '#c55632',
    alpha: 1,
    rgba: 'rgba(197, 86, 50, 1)',
  },
];

const capitalizeFirstLetterTestCases: capitalizeFirstLetterTestCase[] = [
  {
    input: 'hector',
    output: 'Hector',
  },
  {
    input: 'youtube',
    output: 'Youtube',
  },
  {
    input: 'lorem ipsum',
    output: 'Lorem ipsum',
  },
];

describe('hexToRGB', () => {
  it('should transform Hex encoded strings to RBGA format', () => {
    hexToRGBATestCases.forEach(tc => {
      expect(hexToRGB(tc.hex, tc.alpha)).toEqual(tc.rgba);
    });
  });
});

describe('CapitalizeFirstLetter', () => {
  it('should capitalize first letter of the string in the sentence', () => {
    capitalizeFirstLetterTestCases.forEach(tc => {
      expect(CapitalizeFirstLetter(tc.input)).toEqual(tc.output);
    });
  });
});

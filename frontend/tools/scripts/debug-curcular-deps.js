const fs = require('fs');

const circularDependanciesRaw = JSON.parse(
  fs.readFileSync(
    './tools/webpack/circularFiles/circular-dependency-console-ce-current.json',
    'utf8'
  )
);

const storeJson = (path, data) =>
  fs.writeFileSync(path, JSON.stringify(data, null, 2));

function arrayEquals(a, b) {
  return (
    Array.isArray(a) &&
    Array.isArray(b) &&
    a.length === b.length &&
    a.every(val => b.includes(val))
  );
}

const compareTwoDepsLoopsFile = secondData => {
  const filtered = circularDependanciesRaw.filter(
    circular => !mainCircular.some(sub => arrayEquals(circular, sub))
  );
  console.log(filtered.length);
  storeJson('cirDiff.json', filtered);
  return filtered;
};

console.log('Dependencies loops total : ' + circularDependanciesRaw.length);

const prettyLogCircular = arr =>
  console.log(`Circular dependency detected!\n * ${arr.join('\n → ')}\n`);

const logAll = () => {
  circularDependanciesRaw.forEach(arr => {
    prettyLogCircular(arr);
  });
};
const findMostCommon = args => {
  const flatMapped = [...args].flat();

  const mostCommonItems = flatMapped.sort(
    (a, b) =>
      flatMapped.filter(v => v === a).length -
      flatMapped.filter(v => v === b).length
  );

  console.log(mostCommonItems.slice(0, 20));
};

const findWithFilter = filter => {
  const foundFiltered = circularDependanciesRaw.filter(it =>
    it.some(row => row.includes(filter))
  );

  foundFiltered.forEach(it => prettyLogCircular(it));

  console.log('Dependencies loops with filter : ' + foundFiltered.length);
  return foundFiltered;
};

const [, , filter] = process.argv;

if (!filter) {
  throw new Error('No filter provided');
}

const filtered = findWithFilter(filter);

// findMostCommon(filtered);

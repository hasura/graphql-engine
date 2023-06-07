const { merge } = require('webpack-merge');
const fs = require('fs');
const path = require('path');
const CircularDependencyPlugin = require('circular-dependency-plugin');

const prettyLogCircular = arr =>
  `Circular dependency detected!\n * ${arr.join('\n → ')}\n`;

const getExclamations = num =>
  Array.from(Array(Math.round(num / 100)).keys())
    .map(it => '!')
    .join('');

module.exports =
  ({ shouldLogEveryCircularDependency = false, circleDependencyFilter }) =>
  (config, { options, context }) => {
    let numCyclesDetected = 0;
    let filteredCircleDeps = 0;
    let fullLoops = [];

    const destPath = path.join(__dirname, 'circularFiles');

    const currentRunFilePath = path.join(
      destPath,
      `circular-dependency-${context.projectName}-current.json`
    );
    const knownCircular = path.join(
      destPath,
      `circular-dependency-${context.projectName}-known.json`
    );

    const currentKnownDepLoops = JSON.parse(
      fs.readFileSync(knownCircular, 'utf8')
    );

    function arrayEquals(a, b) {
      return (
        Array.isArray(a) &&
        Array.isArray(b) &&
        a.length === b.length &&
        a.every(val => b.includes(val))
      );
    }

    const compareTwoDepsLoopsFile = (a, b) => {
      return a.filter(circular => !b.some(sub => arrayEquals(circular, sub)));
    };

    return merge(config, {
      plugins: [
        new CircularDependencyPlugin({
          exclude: /node_modules/,
          failOnError: false,
          onStart({ compilation }) {
            numCyclesDetected = 0;
            filteredCircleDeps = 0;
            fullLoops = [];
          },
          onDetected({
            // `paths` will be an Array of the relative module paths that make up the cycle
            paths: cyclePaths,
            compilation,
          }) {
            numCyclesDetected++;
            fullLoops = [...fullLoops, cyclePaths];
            const err = new Error(prettyLogCircular(cyclePaths));
            if (!shouldLogEveryCircularDependency) {
              return;
            }

            if (!circleDependencyFilter) {
              compilation.warnings.push(err);
              return;
            }

            if (
              cyclePaths.some(path => path.includes(circleDependencyFilter))
            ) {
              filteredCircleDeps++;
              compilation.warnings.push(err);
            }
          },
          onEnd({ compilation }) {
            fs.writeFileSync(
              currentRunFilePath,
              JSON.stringify(fullLoops, null, 2)
            );
            const baseWarning = `Detected ${numCyclesDetected} circular dependency ${getExclamations(
              numCyclesDetected
            )} `;

            const newLoops = compareTwoDepsLoopsFile(
              fullLoops,
              currentKnownDepLoops
            );

            if (newLoops.length === 0) {
              compilation.warnings.push(
                baseWarning +
                  "Found only existing circular dependencies, the current change didn't introduce new ones."
              );
            } else {
              newLoops.forEach(it =>
                compilation.warnings.push(
                  new Error('New ' + prettyLogCircular(it))
                )
              );
              compilation.warnings.push(
                new Error(
                  baseWarning +
                    newLoops.length +
                    ' new dependency loops introduced ! You need to fix them before the build can continue. If you need help, feel free to reach out the platform team.'
                )
              );
            }

            if (circleDependencyFilter) {
              compilation.warnings.push(
                new Error(
                  `Detected ${filteredCircleDeps} circular dependency only for the filter "${circleDependencyFilter}" ${getExclamations(
                    filteredCircleDeps
                  )}`
                )
              );
            }
          },
          cwd: process.cwd(),
        }),
      ],
    });
  };

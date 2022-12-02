/* npm workspaces work by installing workspace packages into the root workspace folder's node_modules
 * folder using a symlink to the actual package folder on disk. Doing this means that any changes
 * to a workspace package automatically shows up in dependant packages since they are transparently
 * looking at its source code directly via the symlink.
 *
 * For example, the `reference` package is dependent on the `dc-api-types` package. Because they are
 * workspaces, npm will create a symlink from `./node_modules/@hasura/dc-api-types` to
 * `./dc-api-types`. Therefore, when the `reference` package looks at files in
 * `./node_modules/@hasura/dc-api-types` it is actually seeing `./dc-api-types`.
 *
 * The lockfile for a workspace is created at the root level (ie `./package-lock.json`) and _not_
 * at the individual package level (ie. _not_ `./reference/package-lock.json`). This is a problem
 * if you want to be able to work with that package outside of the workspace (for example, if you
 * copy it into a Docker container to run it, or export it to a different repo using copybara).
 *
 * The purpose of this script is to derive a lockfile for a workspace project from the lockfile of
 * the root workspace. We do this by following all dependencies from the workspace project and
 * lifting them up from being located in `./<workspace project>/node_modules` to being in
 * `./node_modules`.
 *
 * For example:
 * `.packages["reference/node_modules/fastify"]` moves to `.packages["node_modules/fastify"]`
 * (The `.packages` object in the lockfile has properties that each represent the installed
 * location of a package on disk (the property name), and information about what is installed
 * there (the property value))
 *
 * However, it is not that simple unfortunately. There is a bit of rejigging that needs to be done
 * in some cases.
 *
 * npm's root lockfile has special entries in it when it symlinks a package (for example see
 * `.packages["node_modules/@hasura/dc-api-types"]`). These need to be replaced with the
 * information about the symlinked package directly since the derived lockfiles are to be used
 * independently outside the context of the workspace and therefore a symlink will not exist.
 *
 * npm also floats dependencies that are shared between workspace packages, or are used by the
 * root workspace package up to the root package level. When these dependencies are used by the
 * package we're deriving a lockfile for, we leave them at the root level, since the source
 * location (ie `./node_modules`) is the same as the destination.
 *
 * However, sometimes a dependency may exist at the root level, but a package depended on the
 * workspace package may require a different version (since npm allows different versions of
 * the same package to exist and be used simultaneously). For example, the `camelcase` v6
 * package exists at `node_modules/camelcase` because `openapi-typescript-codegen` requires it,
 * and it has been lifted to the root `node_modules`. However, `camelcase` is also required by
 * the args package which is used by a dependency of the `reference` package but the `args`
 * package requires v5, not v6. So npm has installed v5 `camelcase` at
 * `reference/node_modules/camelcase` so that it won't conflict with v6 in
 * `node_modules/camelcase`. But this means we now have two versions of `camelcase` and if we lift
 * `reference/node_modules/camelcase` into `node_modules/camelcase`, we will replace v6 with v5.
 * The solution is to push v5 down one level of the package path of the of the package that
 * depends on it. So, because `args` (located in `reference/node_modules/args`) depends on
 * `camelcase` v5, we move it from `reference/node_modules/camelcase` to
 * `node_modules/args/node_modules/camelcase` in the derived lockfile. Now v6 and v5 can co-exist.
 *
 * Usage:
 *
 * Derive lockfiles for both the `reference` and `sqlite` workspace packages
 * > npx ts-node ./scripts/derive-lockfile.ts -l package-lock.json -w reference -w sqlite
 */
import fs from "fs/promises";
import os from "os";
import path from "path";
import * as yargs from "yargs";

type Lockfile = {
  name: string,
  version?: string,
  requires: boolean,
  lockfileVersion: number,
  packages: Record<string, Package>,
  dependencies?: Record<string, unknown>,
}

type Package = {
  link?: true,
  resolved?: string
  name?: string,
  version?: string,
  dependencies?: Record<string, string>,
  devDependencies?: Record<string, string>,
  peerDependencies?: Record<string, string>,
  peerDependenciesMeta?: Record<string, { optional?: boolean }>,
}

type LocatedPackage = {
  dependantPackagePath: PackagePath,
  packagePath: PackagePath,
  package: Package,
}

type Dependency = {
  name: string,
  package: LocatedPackage,
}

// Example paths:
//  reference/node_modules/avvio/node_modules/debug -> { workspace: "reference", path: ["avvio", "debug"] }
//  node_modules/ts-node -> { workspace: null, path: ["ts-node"] }
type PackagePath = {
  // Workspace folder name, if any
  workspace: string | null,
  // Path of package names
  path: string[]
}

/**
 * Locates a package dependency searching first in the dependant package's node_modules and then
 * backwards up the package path until we reach the root workspace's node_modules folder.
 *
 * @param lockfile
 * @param dependantPackagePath The package path of the package that depends on the package being located
 * @param packageName The name of the packge being located
 * @returns The located package or undefined if it could not be found
 */
function locatePackageDependency(lockfile: Lockfile, dependantPackagePath: PackagePath, packageName: string): LocatedPackage | undefined {
  const workspacePrefix = dependantPackagePath.workspace !== null ? dependantPackagePath.workspace + "/" : "";

  function locate(packageSearchPath: PackagePath): LocatedPackage | undefined {
    if (packageSearchPath.path.length === 0) {
      const pkgPath = workspacePrefix + "node_modules/" + packageName;
      const pkg = lockfile.packages[pkgPath];
      return pkg !== undefined
        ? { dependantPackagePath,
            packagePath: packageSearchPath,
            package: pkg }
        : packageSearchPath.workspace !== null
          ? locatePackageDependency(lockfile, { workspace: null, path: [] }, packageName) // If there's no package in the workspace, try the above the workspace
          : undefined;
    } else {
      const pkgPath = workspacePrefix + "node_modules/" + packageSearchPath.path.join("/node_modules/") + "/node_modules/" + packageName;
      const pkg = lockfile.packages[pkgPath];
      return pkg !== undefined
        ? { dependantPackagePath,
            packagePath: packageSearchPath,
            package: pkg }
        : locate({...packageSearchPath, path: packageSearchPath.path.slice(0, packageSearchPath.path.length - 1)});
    }
  }

  return locate(dependantPackagePath);
}

/**
 * Given the specified package and its package path, find all transitive dependencies.
 */
function collectDeps(lockfile: Lockfile, packagePath: PackagePath, pkg: Package): Dependency[] {
  const deps =
    [ ...(Object.keys(pkg.dependencies ?? {}).map<[string, boolean]>(d => [d, false])),
      ...(Object.keys(pkg.devDependencies ?? {}).map<[string, boolean]>(d => [d, false])),
      ...(Object.keys(pkg.peerDependencies ?? {})
          .map<[string, boolean]>(peerDep => {
            const optional = pkg.peerDependenciesMeta?.[peerDep]?.optional === true;
            return [peerDep, optional];
          }))
    ];
  return deps.flatMap(([depPkgName, optional]) => {
    const locatedDep = locatePackageDependency(lockfile, packagePath, depPkgName);
    if (locatedDep === undefined) {
      if (optional)
        return [];
      else
        throw new Error(`Can't locate package '${depPkgName}'`);
    } else if (locatedDep.package.link === true) {
      if (locatedDep.package.resolved === undefined)
        throw new Error(`Linked package '${depPkgName}' does not have a resolved property`);

      const linkedPkg = { ...lockfile.packages[locatedDep.package.resolved] };
      if (linkedPkg === undefined)
        throw new Error(`Cannot file package '${locatedDep.package.resolved}' resolved from linked package '${depPkgName}'`);

      delete linkedPkg.name;

      const dependency = {
        name: depPkgName,
        package: {
          ...locatedDep,
          package: linkedPkg
        }
      };

      return [dependency, ...collectDeps(lockfile, {...locatedDep.packagePath, path: [...locatedDep.packagePath.path, depPkgName]}, linkedPkg)];

    } else {
      const dependency = {
        name: depPkgName,
        package: locatedDep
      };
      return [dependency, ...collectDeps(lockfile, {...locatedDep.packagePath, path: [...locatedDep.packagePath.path, depPkgName]}, locatedDep.package)];
    }
  });
}

/**
 * Relocate a workspace's transitive dependencies from their current location to
 * where they ought to live for the derived lockfile
 */
function relocateWorkspacePackagesToRoot(dependencies: Dependency[]) {
  return dependencies.map(dep => {
    if (dep.package.packagePath.workspace !== null) {

      // If the package is found at both the root and directly within the workspace
      // then we're going to have a conflict and the workspace package needs to pushed
      // into the node_modules folder of the dependency one down from the workspace
      // to prevent it from conflicting
      if (dep.package.packagePath.path.length === 0) {
        const conflictingPackageAlreadyAtRoot =
          dependencies.find(d =>
            d.name === dep.name
            && d.package.packagePath.workspace === null
            && d.package.packagePath.path.length === 0
          );
        if (conflictingPackageAlreadyAtRoot !== undefined) {
          return {
            ...dep,
            package: {
              ...dep.package,
              packagePath: {
                workspace: null,
                path: dep.package.dependantPackagePath.path.slice(0, 1),
              }
            }
          }
        }
      }

      // Remove the workspace from the path to lift it to the root
      return {
        ...dep,
        package: {
          ...dep.package,
          packagePath: {
            ...dep.package.packagePath,
            workspace: null,
          }
        }
      };
    } else {
      // Already at root
      return dep;
    }
  });
}

/**
 * Derive a lockfile for a workspace package from the lockfile at the root of the workspace
 *
 * @param lockfile The root lockfile
 * @param workspace The workspace for which to derive a lockfile
 * @returns The derived lockfile
 */
function deriveLockfile(lockfile: Lockfile, workspace: string): Lockfile {
  const workspacePackage = lockfile.packages[workspace];
  if (workspacePackage.name === undefined) {
    throw new Error(`The workspace package ${workspace} is missing a name`);
  }
  const deps = collectDeps(lockfile, { workspace, path: [] }, workspacePackage);
  const relocatedDeps = relocateWorkspacePackagesToRoot(deps);
  const packages = Object.fromEntries(relocatedDeps.map<[string, Package]>(dep => {
    return dep.package.packagePath.path.length === 0
      ? ["node_modules/" + dep.name, dep.package.package]
      : ["node_modules/" + dep.package.packagePath.path.join("/node_modules/") + "/node_modules/" + dep.name, dep.package.package]
  }));

  return {
    name: workspacePackage.name,
    version: workspacePackage.version,
    // We use version 3 because it is the same as version 2 (npm's current default)
    // except without the backwards compatibility of having the "dependencies"
    // property that is only used by old versions of npm that we don't use or
    // care about. This way we don't need to rewrite that property too.
    lockfileVersion: 3,
    requires: true,
    packages: {
      "": workspacePackage,
      ...packages
    }
  };
}

const argParser = yargs
  .option("lockfile", {
    alias: "l",
    describe: "The worktree lockfile",
    type: "string"
  })
  .option("workspace", {
    alias: "w",
    describe: "The workspace you want to derive a lockfile for",
    type: "string"
  })
  .array("workspace")
  .demandOption(["lockfile", "workspace"])
  .help();

(async () => {
  const args = await argParser.argv;
  console.log(`Reading lockfile '${args.lockfile}'...`);
  const lockfile = JSON.parse(await fs.readFile(args.lockfile, "utf-8"));

  for (const workspace of args.workspace) {
    const outputFile = path.join(path.dirname(args.lockfile), `./${workspace}/package-lock.json`);

    console.log(`Deriving lockfile for workspace '${workspace}'...`);
    const workspaceLockfile = deriveLockfile(lockfile, workspace);
    console.log(`Writing derived lockfile to '${outputFile}'...`);
    await fs.writeFile(outputFile, JSON.stringify(workspaceLockfile, null, 2) + os.EOL, "utf-8");
  }
  console.log("Done deriving lockfile" + (args.workspace.length > 1 ? "s" : ""))
})();

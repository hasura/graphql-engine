Problem
-----------
As mentioned in #1558, users don't want be restricted by a specific project directory structure. Removing `config.yaml` will enable us in doing that.

Context
-----------

**Purpose of `config.yaml`:**
1. Provide configuration key values.
2. Get execution directory path (by validating project directory, which in turn, enables us to run commands from anywhere inside the hasura project directory tree.) 

**Current alternatives available:**  
* For 1, all the key/value pairs in `config.yaml` can be provided using CLI flags / ENV vars / .env file (user defined or default `.env`). `config.yaml` is completely replaceable in this regard.

* For 2, we have a global flag `--project` to define the execution directory. In case we remove complete support for `config.yaml`, the flag will need to be provided with every command we run, which might not be the best user experience. 

Proposed Solutions
--------------------------    

**Define a global flag `--config-file` and make `config.yaml` optional.** 

In this case, depending on how config is handled: 

* **User defined config file** ie `--config-file` flag provided: execution directory derived from config file provided (unless `--project` is provided). Commands can be run from anywhere inside the project root but flag needs to be provided. 
```
# initializing hasura project, here the given file is created 
# (syntax: mandatory flag, path: no change) 
hasura init [directory-name] --config-file path/to/config/file

# running other commands, (syntax: mandatory flag added, path: no change)
hasura console --config-file path/to/config/file
```
  * **Default config file**: what our current workflow is. 
  We can make `hasura-config.yaml` the default config, while still having support for `config.yaml`.
 ```
# initializing hasura project with `hasura-config.yaml`
# (syntax: no change, path: no change)
hasura init [directory-name] 

# running other commands, (syntax: no change, path: no change)
hasura console
```  
  * **No config file**: pwd is treated as execution directory and every commands needs to be run from project root unless `--project flag` is provided. 
```
# initializing hasura project without a config file 
# (syntax: no change, path: might change)
hasura init [directory-name] 

# running other commands (syntax: no change, path: might change)
hasura console
```
  In all the above cases, configuration key/values are read from CLI flags / ENV vars / .env file / config file (if present). If none provided, default values used.

IMO the above provides a reasonable trade-off between the two choices without breaking current workflow. 



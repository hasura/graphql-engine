
# How to update the commands.json file

Currently used commands.json file

    commands.json

Original commands file from redis-doc

    commands.json.orig

New commands file from redis-doc

    commands.json.new


## Create a diff with the hedis-specific changes

    diff commands.json commands.json.orig > my-changes.diff

## Patch the new commands file with the hedis-specific changes

    patch -b commands.json.new my-changes.diff

## Check the changes to the new commands file

    diff commands.json.new commands.json.new.orig

## Save new file versions

    mv commands.json.new.orig commands.json.orig
    mv commands.json.new commands.json

## Clean up temp files

#!/bin/bash
echo "📥 Getting `mongosh` dependancies..."
apt-get install -s -y gnupg
wget -qO - https://www.mongodb.org/static/pgp/server-6.0.asc | apt-key add -
echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu focal/mongodb-org/6.0 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-6.0.list
apt-get update && apt-get -s upgrade
apt-get install -y mongodb-mongosh

echo "🕵️‍♀️ Inspecting Collections..."
# Collections to analyze
if [[ -z "${MONGO_SELECT_COLLECTIONS}" ]]; then
    # Get all collections
    collections=$(mongosh --quiet --username ${MONGO_USERNAME} --password ${MONGO_PASSWORD} --authenticationDatabase=admin --eval "db.getCollectionNames()" ${MONGO_DATABASE} | tr -d '[\[\]"\ \n]')
else
    # Only collections in env vars
    collections=${MONGO_SELECT_COLLECTIONS}
fi
IFS=',' read -ra COLLECTIONS <<< "$collections"

echo "📚 Collections for analysis: ${COLLECTIONS[@]}"
echo '---------------------------------'

# Query each collection and run through variety
mkdir -p /schema_exports/analysis
for collection in "${COLLECTIONS[@]}"; do
    echo "🔍 Analyzing ${collection}..."
    # Run variety on the collection
    # Update this --eval query to match your needs (example, filter or limit how many records to analyze)
    mongosh ${MONGO_DATABASE} --quiet --eval "var collection = '${collection//\'/}', outputFormat='json'" --username ${MONGO_USERNAME} --password ${MONGO_PASSWORD} --authenticationDatabase=admin /schema_sampler/variety.js > "/schema_exports/analysis/${collection//\'/}.json"
    echo "✅ ${collection} analysis complete..."
    echo "💿 Converting analysis to validation schema..."

    # Convert the variety output to a validation jsonschema
    node /schema_sampler/validation_exporter.js ${collection//\'/}
    echo "✅ ${collection} validation schema created..."

    # Plug the json schema back into MongoDB if the user has selected that option
    if [[ "${MONGO_UPDATE_COLLECTIONS}" == "true" ]]; then
        echo "🔄 Updating ${collection} with validation schema in MongoDB..."
        schema=$(cat /schema_exports/validation_schema/${collection//\'/}.json)
        mongosh ${MONGO_DATABASE} --quiet --eval "db.runCommand({ collMod: '${collection//\'/}', validator: ${schema}, validationAction: 'warn' })" --username ${MONGO_USERNAME} --password ${MONGO_PASSWORD} --authenticationDatabase=admin
        echo "✅ ${collection} validation schema updated in MongoDB..."
    fi
    echo '---------------------------------'
done
echo "✅ Collection analysis and update complete..."
#!/bin/bash

# MFlix Dummy Data
echo "📡 Importing mflix sample data..."
mongoimport --host localhost:27017 --username root --password password --authenticationDatabase=admin --db sample_mflix --collection comments --file /sample_data/sample_mflix/comments.json
mongoimport --host localhost:27017 --username root --password password --authenticationDatabase=admin --db sample_mflix --collection movies --file /sample_data/sample_mflix/movies.json
mongoimport --host localhost:27017 --username root --password password --authenticationDatabase=admin --db sample_mflix --collection sessions --file /sample_data/sample_mflix/sessions.json
mongoimport --host localhost:27017 --username root --password password --authenticationDatabase=admin --db sample_mflix --collection theaters --file /sample_data/sample_mflix/theaters.json
mongoimport --host localhost:27017 --username root --password password --authenticationDatabase=admin --db sample_mflix --collection users --file /sample_data/sample_mflix/users.json
echo "✅ Mflix sample data imported..."
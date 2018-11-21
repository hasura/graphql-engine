import yaml
import subprocess
import sys
from dotenv import set_key, load_dotenv

def default_env(path, app_name):
    with open(path, "w") as file:
        file.write("")
    set_key(path, 'HASURA_GRAPHQL_DATABASE_NAME', app_name, quote_mode="never")
    set_key(path, 'HASURA_GRAPHQL_DATABASE_USER', 'gql_test', quote_mode="never")
    set_key(path, 'HASURA_GRAPHQL_DATABASE_HOST', 'localhost', quote_mode="never")
    set_key(path, 'HASURA_GRAPHQL_DATABASE_PORT', '5432', quote_mode="never")
    set_key(path, 'HASURA_GRAPHQL_DATABASE_PASS', '', quote_mode="never")
    set_key(path, 'HASURA_GRAPHQL_DATABASE_URL', 'postgres://gql_test@localhost:5432/{}'.format(app_name), quote_mode="never")
    set_key(path, 'HASURA_GRAPHQL_SERVER_URL', 'http://localhost:8080', quote_mode="never")

EXIT_FAILURE=False
POSTGRES_DEFAULT_CMD=['psql', '-h', 'localhost', '-p', '5432', '-U', 'gql_test', '-c']
DEFAULT_DOCKER_CMD=['docker', 'run', '--net', 'host']
APPS_DATA = {}
with open(".circleci/community-apps.yaml", 'r') as stream:
    try:
        APPS_DATA = yaml.safe_load(stream)
    except yaml.YAMLError as exc:
        print(exc)

for app in APPS_DATA['apps']:
    print("Running test for app {}".format(app['name']))
    exit_code = subprocess.call(POSTGRES_DEFAULT_CMD + ['CREATE DATABASE {};'.format(app['name'])])
    if exit_code:
        EXIT_FAILURE=True
        continue
    exit_code = subprocess.call(POSTGRES_DEFAULT_CMD + ['ALTER DATABASE {} OWNER TO gql_test;'.format(app['name'])])
    if exit_code:
        EXIT_FAILURE=True
        continue
    file_path='{}/.env.list'.format(app['path'])
    default_env(file_path, app['name'])
    if 'access_key' in app and app['access_key']:
        set_key(file_path, 'HASURA_GRAPHQL_ACCESS_KEY', app['access_key'], quote_mode="never")
    if 'webhook_url' in app and app['webhook_url']:
        set_key(file_path, 'HASURA_GRAPHQL_AUTH_HOOK', app['webhook_url'], quote_mode="never")
    if 'jwt_secret' in app and app['jwt_secret']:
        set_key(file_path, 'HASURA_GRAPHQL_JWT_SECRET', app['jwt_secret'], quote_mode="auto")
    if 'migrations_dir' in app and app['migrations_dir']:
        docker_migration_cmd = DEFAULT_DOCKER_CMD[:]
        docker_migration_cmd = docker_migration_cmd + ['-e', 'HASURA_GRAPHQL_ONLY_MIGRATION:true']
        docker_migration_cmd = docker_migration_cmd + ['-v', '{}/{}:/hasura-migrations'.format(app['path'], app['migrations_dir'])]
        docker_migration_cmd.append('server:latest')
        exit_code = subprocess.call(docker_migration_cmd)
        if exit_code:
            EXIT_FAILURE=True
            continue
    docker_server_cmd = DEFAULT_DOCKER_CMD[:]
    docker_server_cmd = docker_server_cmd + ['--env-file', '{}/.env.list'.format(app['path'])]
    docker_server_cmd = docker_server_cmd + ['-p', '8080:8080']
    docker_server_cmd = docker_server_cmd + ['--name', 'graphql-engine-server']
    docker_server_cmd.append('-d')
    docker_server_cmd.append('server:latest')
    exit_code = subprocess.call(docker_server_cmd)
    if exit_code:
        EXIT_FAILURE=True
        continue
    exit_code = subprocess.call(['docker', 'build', '-t', app['name'], '-f', '{}/{}'.format(app['path'], app['dockerfile']), app['path']])
    if exit_code:
        EXIT_FAILURE=True
        continue
    docker_community_cmd = DEFAULT_DOCKER_CMD[:]
    docker_community_cmd = docker_community_cmd + ['--env-file', '{}/.env.list'.format(app['path'])]
    for port_mapping in app['port_mappings']:
        docker_community_cmd = docker_community_cmd + ['-p', port_mapping]
    docker_community_cmd.append(app['name'])
    if 'command' in app and app['command']:
        docker_community_cmd.append(app['command'])
    exit_code = subprocess.call(docker_community_cmd)
    if exit_code:
        EXIT_FAILURE=True
        continue
    exit_code = subprocess.call(['docker', 'stop', 'graphql-engine-server'])
    if exit_code:
        EXIT_FAILURE=True
        continue
    exit_code = subprocess.call(['docker', 'rm', 'graphql-engine-server'])
    if exit_code:
        EXIT_FAILURE=True
        continue
    exit_code = subprocess.call(POSTGRES_DEFAULT_CMD + ['DROP DATABASE {};'.format(app['name'])])
    if exit_code:
        EXIT_FAILURE=True

if EXIT_FAILURE:
    sys.exit(1)

import yaml
import subprocess
from dotenv import set_key

def default_env(path, app_name):
    with open(path, "w") as file:
        file.write("")
    set_key(path, 'DATABASE_NAME', app_name)
    set_key(path, 'DATABASE_USER', 'gql_test')
    set_key(path, 'DATABASE_HOST', 'localhost')
    set_key(path, 'DATABASE_PORT', '5432')
    set_key(path, 'DATABASE_PASS', '')
    set_key(path, 'DATABASE_URL', 'postgres://gql_test@localhost:5432/{}'.format(app_name))

POSTGRES_DEFAULT_CMD=['psql', '-h', 'localhost', '-p', '5432', '-U', 'gql_test', '-c']
DEFAULT_DOCKER_CMD=['docker', 'run', '--env-file', '{}/.env.list', '--net', 'host']
APPS_DATA = {}
with open(".circleci/community-apps.yaml", 'r') as stream:
    try:
        APPS_DATA = yaml.safe_load(stream)
    except yaml.YAMLError as exc:
        print(exc)

for app in APPS_DATA['apps']:
    print("Running test for app {}".format(app['name']))
    subprocess.call(POSTGRES_DEFAULT_CMD + ['"CREATE DATABASE {};"'.format(app['name'])])
    subprocess.call(POSTGRES_DEFAULT_CMD + ['"ALTER DATABASE {} OWNER TO gql_test;"'.format(app['name'])])
    subprocess.call(['docker', 'build', '-t', app['name'], '-f', '{}/{}'.format(app['path'], app['dockerfile']), app['path']])
    file_path='{}/.env.list'.format(app['path'])
    default_env(file_path, app['name'])
    docker_cmd = DEFAULT_DOCKER_CMD.copy()
    for port_mapping in app['port_mappings']:
        docker_cmd = docker_cmd + ['-p', port_mapping]
    docker_cmd.append(app['name'])
    if app['command']:
        docker_cmd.append(app['command'])
    print(docker_cmd)
    subprocess.call(docker_cmd)
    subprocess.call(POSTGRES_DEFAULT_CMD + ['"DROP DATABASE {};"'.format(app['name'])])

import pathlib
from typing import Any, NamedTuple
import subprocess


class JWTConfiguration(NamedTuple):
    private_key_file: pathlib.Path
    public_key_file: pathlib.Path
    private_key: str
    public_key: str
    algorithm: str
    server_configuration: dict


def init_rsa(tmp_path: pathlib.Path, configuration: Any) -> JWTConfiguration:
    private_key_file = tmp_path / 'private.key'
    public_key_file = tmp_path / 'public.key'
    subprocess.run(['openssl', 'genrsa', '-out', private_key_file, '2048'], check=True, capture_output=True)
    subprocess.run(['openssl', 'rsa', '-pubout', '-in', private_key_file, '-out', public_key_file], check=True, capture_output=True)
    with open(private_key_file) as f:
        private_key = f.read()
    with open(public_key_file) as f:
        public_key = f.read()
    server_configuration = {
        'type': 'RS512',
        'key': public_key,
        **configuration,
    }
    return JWTConfiguration(
        private_key_file = private_key_file,
        public_key_file = public_key_file,
        private_key = private_key,
        public_key = public_key,
        algorithm = 'RS512',
        server_configuration = server_configuration,
    )


def init_ed25519(tmp_path: pathlib.Path, configuration: Any) -> JWTConfiguration:
    private_key_file = tmp_path / 'private.key'
    public_key_file = tmp_path / 'public.key'
    subprocess.run(['openssl', 'genpkey', '-algorithm', 'ed25519', '-outform', 'PEM', '-out', private_key_file], check=True, capture_output=True)
    subprocess.run(['openssl', 'pkey', '-pubout', '-in', private_key_file, '-out', public_key_file], check=True, capture_output=True)
    with open(private_key_file) as f:
        private_key = f.read()
    with open(public_key_file) as f:
        public_key = f.read()
    server_configuration = {
        'type': 'Ed25519',
        'key': public_key,
        **configuration,
    }
    return JWTConfiguration(
        private_key_file = private_key_file,
        public_key_file = public_key_file,
        private_key = private_key,
        public_key = public_key,
        algorithm = 'EdDSA',
        server_configuration = server_configuration,
    )

def init_es256(tmp_path: pathlib.Path, configuration: Any) -> JWTConfiguration:
    private_key_file = tmp_path / 'private.key'
    public_key_file = tmp_path / 'public.key'
    subprocess.run(['openssl', 'ecparam', '-name', 'prime256v1', '-genkey', '-noout', '-out', private_key_file], check=True, capture_output=True)
    subprocess.run(['openssl', 'ec', '-in', private_key_file, '-pubout', '-out', public_key_file], check=True, capture_output=True)
    with open(private_key_file) as f:
        private_key = f.read()
    with open(public_key_file) as f:
        public_key = f.read()
    server_configuration = {
        'type': 'ES256',
        'key': public_key,
        **configuration,
    }
    return JWTConfiguration(
        private_key_file = private_key_file,
        public_key_file = public_key_file,
        private_key = private_key,
        public_key = public_key,
        algorithm = 'ES256',
        server_configuration = server_configuration,
    )

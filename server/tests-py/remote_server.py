import os
import subprocess
from typing import Optional

import ports

class NodeGraphQL:
    def __init__(self, cmd: list[str], env: dict[str, str] = {}, port: Optional[int] = None):
        self.cmd = cmd
        self.env = env
        self.port = port if port else ports.find_free_port()
        self.proc: Optional[subprocess.Popen[bytes]] = None

    def start(self):
        self.proc = subprocess.Popen(self.cmd, env={**os.environ, **self.env, 'PORT': str(self.port)})
        try:
            ports.wait_for_port(self.port, timeout = 3)
        except:
            self.proc.kill()
            self.proc = None
            raise

    def stop(self):
        if self.proc:
            self.proc.terminate()
            self.proc = None

    @property
    def url(self):
        return f'http://localhost:{self.port}'

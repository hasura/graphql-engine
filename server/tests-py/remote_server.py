import os
import subprocess
from typing import Optional

import ports

class NodeGraphQL:
    def __init__(self, worker_id: str, script: str, env: dict[str, str] = {}, port: Optional[int] = None):
        self.script = script
        self.env = env
        self.port = port if port else ports.find_free_port(worker_id)
        self.proc: Optional[subprocess.Popen[bytes]] = None

    def start(self):
        self.proc = subprocess.Popen(['node', self.script], env={**os.environ, **self.env, 'PORT': str(self.port)})
        try:
            ports.wait_for_port(self.port, timeout = 30)
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

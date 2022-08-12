#!/usr/bin/env python3
import subprocess
import time

class NodeGraphQL():

    def __init__(self, cmd):
        self.cmd = cmd
        self.proc = None

    def start(self):
        proc = subprocess.Popen(self.cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        time.sleep(1)
        proc.poll()
        if proc.returncode is not None:
            raise Exception("It seems our node graphql test server stopped unexpectedly:\n" + proc.stdout.read().decode('utf-8'))
        self.proc = proc

    def stop(self):
        self.proc.terminate()

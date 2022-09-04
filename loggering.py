import logging, sys

# Disable buffering
class Unbuffered(object):
    def __init__(self, stream):
        self.stream = stream
    def write(self, data):
        self.stream.write(data)
        self.stream.flush()
        sys.stderr.write(data)
        sys.stderr.flush()
    def __getattr__(self, attr):
        return getattr(self.stream, attr)

logging.basicConfig(filename='chesster.log', level=logging.DEBUG)
out = Unbuffered(sys.stdout)
def output(line):
  print(line, file=out)
  logging.debug(line)
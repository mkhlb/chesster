import time


def time_bench(function, args, repetition=1):

  start = time.time()
  for i in range(repetition):
    ret = function(*args)
  end = time.time()
  print(end - start)
  print(start)
  print(end)
  return ret
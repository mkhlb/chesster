def reverse_pst(pst):
  out = ''
  for square in pst[::-1]:
    out += '({}, {}), '.format(square[0], square[1])

  print(out)
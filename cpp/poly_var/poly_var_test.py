"""poly_var_test.py

   Unit-tests exercising poly_var API functions.

"""

import poly_var
import unittest

# print poly_var.compiler()
# print poly_var.integers(1, 10)
# print poly_var.sum(range(1, 11)) #[1, ..., 10]
# print poly_var.sum([range(1, 11), range(1, 11)])
# print poly_var.transpose([1, 2, 3]) #Shorthand for [[1, 2, 3]]
# print poly_var.transpose([[1], [2], [3]])

print("Module \"_poly_var\" compiled with %s" % (poly_var.compiler(),))

class poly_var_api_tests(unittest.TestCase):
  def test_integers(self):
    assert poly_var.integers(1, 10) == [[i] for i in range(1, 11)]
  def test_sum(self):
    assert poly_var.sum(range(1, 11))==55.
    assert poly_var.sum([range(1, 11), range(1, 11)]) == 110.
  def test_transpose(self):
    assert poly_var.transpose([1, 2, 3]) == [[1], [2], [3]]
    assert poly_var.transpose([[1], [2], [3]]) == [[1, 2, 3]]

class poly_var_api_test_suite(unittest.TestSuite):
  def __init__(self):
    unittest.TestSuite.__init__(self,
        map(poly_var_api_tests, ['test_integers', 'test_sum', 'test_transpose']))

if __name__ == '__main__':
  unittest.TextTestRunner().run(poly_var_api_test_suite())


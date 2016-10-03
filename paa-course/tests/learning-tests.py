
import unittest

from sympy import *

class TestLearning(unittest.TestCase):

    def test_indexed_same_as_its_symbol(self):

        f, f_symbol = IndexedBase('f'), symbols('f')

        self.assertNotEqual(f, f.args[0])
        self.assertEqual(f_symbol, f.args[0])

    def test_indexed_in_free_symbols(self):

        f, n = IndexedBase('f'), symbols('n')
        term = 3 * f[n+2] + 8

        self.assertTrue(f, term.args)
        self.assertTrue(f.args[0], term.args)
        self.assertTrue(f[4], term.args)

    def test_Add_args_length(self):
        
        i,j,k = symbols('i j k')
        addition = 3*i + j/2 + sqrt(k)
        deep_addition = Add(addition, sqrt(74))
        combination = addition*i + deep_addition*k**2

        self.assertEqual(len(addition.args), 3)
        self.assertEqual(len(deep_addition.args), 4)

        # in order to have a deeply flatten object respect `Add` we have to `expand`
        expanded = combination.expand()
        self.assertEqual(len(flatten(expanded.args, cls=Add)), 7)
        self.assertEqual(flatten(expanded.args, cls=Add), 
                         [k**(5/2), 3*i**2, i*sqrt(k), sqrt(74)*k**2, i*j/2, j*k**2/2, 3*i*k**2])
        

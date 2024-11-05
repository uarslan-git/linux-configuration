vim.snippet.add(
    "testgen",
    [[
import unittest

class ${1:TestClass}(unittest.TestCase):
    def setUp(self):
        """Set up test environment"""
        ${2:pass}

    def tearDown(self):
        """Tear down test environment"""
        ${3:pass}

    def test_${4:sample_test}(self):
        """Test ${4:sample_test}"""
        ${5:self.assertEqual(expected, actual)}

if __name__ == '__main__':
    unittest.main()
    ]],
    { buffer = 0 }
)


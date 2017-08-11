import _pytest
import hy

def pytest_collect_file(parent, path):
    if path.ext == ".hy" and "tests" in path.dirname:
        return _pytest.python.pytest_pycollect_makemodule(path, parent)
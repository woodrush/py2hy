from setuptools import setup, find_packages

#import subprocess
#subprocess.call(['make', 'compilehy'])

setup(
    name="py2hy",
    version="0.9.1",
    description="Python to Hy compiler",
    long_description="""Compiles Python code to Hy.""",
    url="https://github.com/woodrush/py2hy",
    author="Hikaru Ikuta",
    author_email="woodrush924@gmail.com",
    license="LGPL-3",
    keywords="sample setuptools development",
    platforms=['any'],
    python_requires='>=3.6',
    install_requires = ["hy==0.18.0"],
    #packages=find_packages(exclude=["tests","build","tools"]),
    packages=find_packages(exclude=["tests","old"]),
    #packages=['py2hy'],
    package_data={
        "py2hy": ["*.hy"],
    },
    test_suite='nose.collector',
    tests_require=['nose'],
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "Topic :: Software Development :: Build Tools",
        "License :: DFSG approved",
        "License :: OSI Approved :: GNU Lesser General Public License v3 or later (LGPLv3+)",
        "Operating System :: OS Independent",
        "Programming Language :: Lisp",
        "Programming Language :: Python",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.6",
        "Topic :: Software Development :: Code Generators",
        "Topic :: Software Development :: Compilers",
        "Topic :: Software Development :: Libraries"
    ],
    entry_points={
        "console_scripts": [
            "py2hy=py2hy.py2hy:main",
        ],
    },
    scripts = [
        'scripts/py2hy.sh'
    ]
)

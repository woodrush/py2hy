@echo off

if "%1" == "all" goto :all
if "%1" == "" goto :all
goto :body

:all
    call :template
    call :compilehy
goto :EOF


:body

if "%1" == "template" (
:template
    python tools/parse_pygrammarspecs.py > tools/template.hy
goto :EOF
)

if "%1" == "compilehy" (
:template
    for %%f in (src/py2hy/*.hy) do (
        hy2py src/py2hy/%%f > py2hy/%%~nf.py
    )
    python -m compileall .
goto :EOF
)

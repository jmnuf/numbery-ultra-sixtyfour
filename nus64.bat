@ECHO OFF
erl -pa %CD%\build\ -noshell -s nus64 main -- %*
EXIT /B

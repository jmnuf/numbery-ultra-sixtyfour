@ECHO OFF
erl -pa %CD%\build\ -noshell -s nu64 main -- %*
EXIT /B

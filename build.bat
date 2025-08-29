@ECHO OFF
SET FILE_NAME=nu64
erlc -o build/ src/%FILE_NAME%.erl && echo Built %FILE_NAME% succesfully
EXIT /B

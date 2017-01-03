rem Run Eclipse with Java 8, to make sure the right versions are used
rem This assumes that Java 8 and Eclipse have been installed as per the StateCU developer documentation.

rem Since this program is specific to the MinGW environment, run the MinGW setup script first
rem Use "call" so control returns to this script.
call ..\mingw\setup-mingw-env.bat

rem Specify the VM to use and the maximum memory
"C:\Program Files\Eclipse\eclipse-parallel-mars-64\eclipse" -vm "C:\Program Files\Java\jre8\bin\java" -vmargs -Xmx700M

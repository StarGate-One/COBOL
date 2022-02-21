@echo off

call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"

set _root_drive=D:
set _cobol_root=%_root_drive%\Projects\Git\cobol

set COB_CONFIG_DIR=%_cobol_root%\config
set COB_COPY_DIR=%_cobol_root%\cpy
rem set COB_LIBS=%_cobol_root%\libs\libcob.lib
set COB_LIBRARY_PATH=%_cobol_root%\libs

set INCLUDE=%INCLUDE%;D:\Projects\Git\_vcpkg\Release\include;%_cobol_root%\include
set LIB=%LIB%;%COB_LIBRARY_PATH%
set LIBPATH=%LIBPATH%;%COB_LIBRARY_PATH%
set PATH=%PATH%;%_cobol_root%\bin
set PreferredToolArchitecture=%Platform%
set TEMP=%_cobol_root%\temp
set TMP=%_cobol_root%\tmp
set VS140COMNTOOLS=
set VSCMD_SKIP_SENDTELEMETRY=1
set _IsNativeEnvironment=true

%_root_drive%
cd %_root_drive%\
cd %_cobol_root%
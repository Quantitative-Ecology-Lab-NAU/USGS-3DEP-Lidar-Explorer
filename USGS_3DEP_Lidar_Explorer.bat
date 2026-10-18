@echo off
setlocal

rem pushd also supports launching this folder from a UNC network share.
pushd "%~dp0"
if errorlevel 1 (
  echo Could not open the app folder.
  pause
  exit /b 1
)

set "RSCRIPT="
for /f "delims=" %%I in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%I"
if not defined RSCRIPT if defined R_HOME if exist "%R_HOME%\bin\Rscript.exe" set "RSCRIPT=%R_HOME%\bin\Rscript.exe"
if not defined RSCRIPT if defined R_HOME if exist "%R_HOME%\bin\x64\Rscript.exe" set "RSCRIPT=%R_HOME%\bin\x64\Rscript.exe"
if not defined RSCRIPT for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"
if not defined RSCRIPT for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"

if not defined RSCRIPT (
  echo Rscript.exe was not found. Install R, then run this launcher again.
  echo See README.md for the required R packages.
  pause
  popd
  exit /b 1
)

echo Starting USGS 3DEP Lidar Explorer in your browser...
echo Keep this window open while using the app. Press Ctrl+C here to stop it.
"%RSCRIPT%" "%CD%\run_app.R"
set "APP_EXIT=%ERRORLEVEL%"
if not "%APP_EXIT%"=="0" (
  echo.
  echo The app stopped with exit code %APP_EXIT%.
  echo Check that the R packages listed in README.md are installed.
  pause
)
popd
exit /b %APP_EXIT%

Setup:

Install the whole package by running this in Powershell:
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }

Building and running:

By running the task, it will build, compile and execute.
NOTE: Generating a .cabal file could be done with the command 'stack init' in order to build and execute the project
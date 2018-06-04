@echo off 
SETLOCAL 

 
REM We use this to tell FAKE to not use the current latest version to build the netcore version,  
REM but instead use the current NON dotnetcore version 
SET NO_DOTNETCORE_BOOTSTRAP=true 
 
SET FAKE_PATH=..\..\packages\FAKE\tools\Fake.exe 
SET Platform= 

copy "..\..\packages\zulib\System.Reflection.Metadata\lib\portable-net45+win8\System.Reflection.Metadata.dll" "../../packages/FAKE/tools/" /Y
copy Zen.FSharp.Compiler.Service.dll.config "..\..\packages\FAKE\tools/" /Y
copy Zen.FSharp.Compiler.Service.dll.config "..\..\packages\Zen.FSharp.Compiler.Service\lib\net45\" /Y
copy FAKE.exe.config "..\..\packages\FAKE\tools\" /Y

 
IF [%1]==[] ( 
    "%FAKE_PATH%" "build.fsx" "Default"  
) ELSE ( 
    "%FAKE_PATH%" "build.fsx" %*  
)  

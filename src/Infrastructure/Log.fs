module Infrastructure.Log

open System
open Hopac
open Logary
open Logary.Configuration
open Logary.Targets

let private logger = Logging.getLoggerByName ""

let debug fn = logger.debug fn
let verbose fn = logger.verbose fn
let info fn = logger.info fn
let warning fn = logger.warn fn
let error fn = logger.error fn

let create =
  withLogaryManager "Node" (
    withTargets [
      LiterateConsole.create (LiterateConsole.empty) "console"
      File.create (File.Naming ("{datetime}", "log") |> File.FileConf.create System.Environment.CurrentDirectory) "file"
    ] >>
    withRules [

#if DEBUG
      Rule.createForTarget "console"
      Rule.createForTarget "file"
#else
      Rule.createForTarget "console" |> Rule.setLevel LogLevel.Info
      Rule.createForTarget "file" |> Rule.setLevel LogLevel.Info
#endif
    ]
  )
  |> run
module CommonTypes

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ComputingResult =
 type ComputingResult<'TResult, 'TFailure> =
    | Success of 'TResult
    | Failure of 'TFailure

 let internal bind processFunc lastResult =
    match lastResult with
    | Success success -> processFunc success
    | Failure failure -> Failure failure

 let internal switch processFunc input =
    Success(processFunc input)

 let internal (>>=) input inputFunction =
    bind inputFunction input
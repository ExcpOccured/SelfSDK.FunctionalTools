module SelfSDK.FunctionalTools.Algebraic_types.Vector

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =
    /// Represents an algebraic type as a one-dimensional array
    type public Vector< ^T when ^T : (static member (~-): ^T -> ^T)
                and ^T : (static member (+): ^T * ^T -> ^T)
                and ^T : (static member (*): ^T * ^T -> ^T)>(_array: ^T[]) =
                     let internalArray: ^T [] = _array

                     member inline __.Values = internalArray
                     member inline __.Dimension = Array.length internalArray

                     // Constructs a Vector using given initializer
                     static member inline Init (n: int) (initializer: (int -> ^T)) =
                              Vector< ^T>(Array.init n (fun i -> initializer (i + 1)))

                     member inline __.Item with get (i: int) = internalArray.[i - 1]

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VectorFunctions =
    open Vector
    let inline internal ( ~- ) (vector: Vector< ^T>) =
        Vector< ^T>.Init (Array.length vector.Values) (fun i -> -vector.[i])

    let inline internal ( + ) (xVector: Vector< ^T>)  (yVector: Vector< ^T>) =
        Vector< ^T>.Init (Array.length xVector.Values) (fun i -> xVector.[i] + yVector.[i])

    let inline  internal ( - ) (xVector: Vector< ^T>)  (yVector: Vector< ^T>) =
        Vector< ^T>.Init (Array.length xVector.Values) (fun i -> xVector.[i] - yVector.[i])

    let inline internal ( * ) (coefficient: ^T)  (vector: Vector< ^T>) =
        Vector< ^T>.Init (Array.length vector.Values) (fun i -> coefficient * vector.[i])
        
    let inline internal toList (vVector : Vector< ^T>) =
        vVector.Values |> Array.toList
        
    let inline internal toSeq (vVector: Vector< ^T>) =
        vVector.Values |> Seq.cast

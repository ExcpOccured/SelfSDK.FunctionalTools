module SelfSDK.FunctionalTools.Algebraic_types.Matrix

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix =
 open System
 /// Represent the set:'T = {x ∈ Math.Pow(R, (N * M))}
 type public Matrix<'T when 'T: struct and 'T: (new: unit -> 'T)
                                and 'T :> IEquatable<'T> and 'T :> IFormattable
                                and 'T :> ValueType>(N: int, M: int) =
    let internallArray = Array2D.zeroCreate<'T> N M

    member this.Item
           with get (x: int, y: int) = internallArray.[x, y]
           and set (x: int, y) (value: 'T) = internallArray.[x, y] <- value
    member inline __.Values = internallArray       

    /// This method can be used via the x.[start .. finish] syntax
    member this.GetSlice(rowStart: int option, rowFinish: int option, columnStart: int option, columnFinish: int option) =
        let rowStart =
            match rowStart with
            | Some(x) -> x
            | None -> 0
        let rowFinish =
            match rowFinish with
            | Some(x) -> x
            | None -> internallArray.GetLength(0) - 1
        let columnStart =
            match columnStart with
            | Some(x) -> x
            | None -> 0
        let columnFinish =
            match columnFinish with
            | Some(x) -> x
            | None -> internallArray.GetLength(1) - 1
        internallArray.[rowStart..rowFinish, columnStart..columnFinish]

    member this.GetSlice(row: int, columnStart: int option, columnFinish: int option) =
        let columnStart =
            match columnStart with
            | Some(x) -> x
            | None -> 0
        let columnFinish =
            match columnFinish with
            | Some(x) -> x
            | None -> internallArray.GetLength(1) - 1
        internallArray.[row, columnStart..columnFinish]

    member this.GetSlice(rowStart: int option, rowFinish: int option, column: int) =
        let rowStart =
            match rowStart with
            | Some(x) -> x
            | None -> 0
        let rowFinish =
            match rowFinish with
            | Some(x) -> x
            | None -> internallArray.GetLength(1) - 1
        internallArray.[rowStart..rowFinish, column]


/// The module contains basic operations on matrices
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MatrixProcessing =
    open Matrix
    open CommonTypes.ComputingResult
    let displayResult result =
        match result with
        | Success succes -> printfn "%s" succes
        | Failure failure -> printfn "%s" failure
    
    /// Generates a random matrix sizeof [rowCount, columnCount]
    let internal generateRandomMatrix (xCoeff: int) (yCoeff: int) (rowCount: int) (columnCount: int) =
        let matrix = new Matrix<int>(rowCount, columnCount)
        for i in 0..rowCount - 1 do
            for j in 0..columnCount - 1 do
                matrix.[i, j] <- int (i) * xCoeff - int (i) * yCoeff
        matrix

    /// Matrix< ^a> -> Math.Pow(Matrix < 'a>, T)
    let rec transposeMatrix = function
        | (_ :: _) :: _ as matrix -> List.map List.head matrix :: transposeMatrix (List.map List.tail matrix)
        | _ -> []

    let rec innerMultiplication vVector uVector =
        match uVector, vVector with
              | [ x ], [ y ] -> x * y
              | uVector' :: uVector, vVector' :: vVector -> uVector' * vVector' + innerMultiplication uVector vVector
              | _ -> failwithf "vectorLength called with an unsupported array size of %d" (uVector.Length)

    /// Matrix < 'a> * Matrix < 'a>
    let internal matrixMultiplication aMatrix bMatrix =
        [ for row in aMatrix ->
             [ for column in transposeMatrix bMatrix -> innerMultiplication row column ] ]
        
     /// Transform a matrix into a sequence.
    let inline toSeq(matrix: #Matrix<_>) = matrix.Values |> Seq.cast< ^T>
    
    /// Transform a matrix into a 2D array.
    let inline toArray2D (matrix: #Matrix<_>) = matrix.Values
     
    let inline internal (*) (aMatrix: Matrix< ^T>) (bMatrix: Matrix< ^T>) =
        matrixMultiplication (toSeq aMatrix) (toSeq bMatrix |> List.ofSeq)
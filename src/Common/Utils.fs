module Common.Utils


module Seq =        
    let inline tryAverage sq =
        if sq |> Seq.isEmpty then None
        else 
            let wa = sq |> Seq.reduce (+)
            let l = sq |> Seq.length
            let center = LanguagePrimitives.DivideByInt wa l
            Some center
    let optionSwap sq =
        Seq.foldBack (fun a sq -> 
            sq 
            |> Option.bind (fun sq ->
                a |> Option.map (fun a -> seq {yield a; yield! sq}))) 
                sq (Some Seq.empty)

            
open Vector

type Array2D<'T> =
    private {
        length1: int
        length2: int
        array: 'T [] []
    }
with
    member this.GetSlice (start1, finish1, start2, finish2) =
        let start1 = defaultArg start1 0
        let finish1 = defaultArg finish1 this.length1
        let start2 = defaultArg start2 0
        let finish2 = defaultArg finish2 this.length2 
        let arr = this.array.[start2..finish2] |> Array.map (fun a -> a.[start1..finish1])
        {
            length1 = if arr.Length > 0 then arr.[0].Length else 0
            length2 = arr.Length
            array = arr
        }
    member this.GetSlice (index1, start2, finish2) =
        let start2 = defaultArg start2 this.length2
        let finish2 = defaultArg finish2 this.length2
        let arr = this.array.[start2..finish2] |> Array.map (fun a -> a.[index1])
        arr
    member this.GetSlice (start1, finish1, index2) =
        let start1 = defaultArg start1 0
        let finish1 = defaultArg finish1 this.length1
        let arr = this.array.[index2].[start1..finish1]
        arr
    member this.Item 
        with get (x, y) =
            if 0 <= x && x < this.length1 && 0 <= y && y < this.length2 then
                this.array.[y].[x]
            else
                failwith "index out of range"
        and set (x, y) value =
            if 0 <= x && x < this.length1 && 0 <= y && y < this.length2 then
                this.array.[y].[x] <- value
            else
                ()
                // raise (System.IndexOutOfRangeException())

let array2D seqSeq = 
    match Seq.tryHead seqSeq with 
    | None -> {length1 = 0; length2 = 0; array = [||]}
    | Some a ->
        let arr = 
            seqSeq 
            |> Seq.map (Seq.toArray)
            |> Seq.toArray
        {
            length1 = arr.[0].Length
            length2 = arr.Length
            array = arr
        }

module Array2D =
    let length1 array2D = array2D.length1
    let length2 array2D = array2D.length2
    // let toList array2D =
    //     [
    //         for x in [0..Array2D.length1 array2D - 1] do
    //             for y in [0..Array2D.length2 array2D - 1] do
    //                 yield array2D.[x, y]
    //     ]
    // let toSeq array2D =
    //     seq {
    //         for x in [0..Array2D.length1 array2D - 1] do
    //             for y in [0..Array2D.length2 array2D - 1] do
    //                 yield array2D.[x, y]
    //     }
    // let toSeqSeq array2D =
    //     seq {
    //         for x in [0..Array2D.length1 array2D - 1] do 
    //             seq {
    //                 for y in [0..Array2D.length2 array2D - 1] do
    //                     yield array2D.[x, y]
    //             }
    //     }

    let setAt pos data array2D =
        {
            array2D with 
                array =
                array2D.array |> Array.mapi (fun y xs -> 
                    if pos.y = y then 
                        xs |> Array.mapi (fun x v -> if pos.x = x then data else v) 
                    else xs)
        }
    let updateAt pos f array2D =
        {
            array2D with
                array = 
                    array2D.array |> Array.mapi (fun y xs ->
                        if pos.y = y then   
                            xs |> Array.mapi (fun x v -> if pos.x = x then f v else v)
                        else xs
                    )
        }

    let toArray array2D =
        array2D.array |> Array.concat
    
    let toList array2D =
        array2D |> toArray |> Array.toList
    
    let toSeq array2D = array2D |> toArray |> Array.toSeq

    let toIndexedSeq array2D =
        array2D.array |> Seq.mapi (fun y -> Seq.mapi (fun x d -> ({x=x;y=y}, d)))
        |> Seq.concat
    let toIndexedSeqSeq array2D =
        array2D.array |> Seq.mapi (fun y -> Seq.mapi (fun x d -> ({x=x;y=y}, d)))
    
    let toArrayArray array2D = array2D.array


    let map f array2D =
        {
            length1 = array2D.length1
            length2 = array2D.length2
            array = array2D.array |> Array.map (Array.map f)
        }
        
    let copy array2D =
        array2D |> map id
    
    let mapi f array2D =
        {
            length1 = array2D.length1
            length2 = array2D.length2
            array = array2D.array |> Array.mapi (fun y -> Array.mapi (fun x -> f x y))
        }

    let create length1 length2 v =
        {
            length1 = length1
            length2 = length2
            array =
                [|
                    for y in [0..length2 - 1] do
                    [|
                        for x in [0..length1 - 1] do
                            yield v 
                    |]
                |]
        }
    
    let ofSeqSeq seqSeq =
        match Seq.tryHead seqSeq with 
        | None -> {length1 = 0; length2 = 0; array = [||]}
        | Some a ->
            let arr = 
                seqSeq 
                |> Seq.map (Seq.toArray)
                |> Seq.toArray
            {
                length1 = arr.[0].Length
                length2 = arr.Length
                array = arr
            }

module NonEmptyTree =
    
    type NonEmptyTree<'T> =
        | Branch of ('T * NonEmptyTree<'T> list)
    module NonEmptyTree =
        let rec map f tree =
            match tree with
            | Branch (a, t) -> Branch (f a, t |> List.map (map f))
module NonEmptySeqTree =
    type NonEmptySeqTree<'T> =
        | Branch of ('T * NonEmptySeqTree<'T> seq)
    type LazyCtx<'T> =
        | Top 
        | BranchLazyCtx of ('T * NonEmptySeqTree<'T> seq * Lazy<LazyCtx<'T>> * NonEmptySeqTree<'T> seq)
    type NonEmptyTreeLoc<'T> =
        {
            tree: NonEmptySeqTree<'T>
            ctx: LazyCtx<'T>
        }
    module Loc =
        let toTree loc =
            let rec loop loc cnt =
                match loc.ctx with
                | Top -> cnt loc.tree
                | BranchLazyCtx (a, ls, c, rs) ->
                    let ctx = c.Value
                    loop {tree = loc.tree; ctx=ctx} (fun ans -> 
                        let sq =
                            seq {
                                yield! ls |> Seq.rev
                                ans
                                yield! rs
                            } 
                        Branch (a, sq) |> cnt)
            loop loc id

        let modify f loc =
            {
                loc with tree = f loc.tree 
            }
        let top tree = {tree = tree; ctx = Top}
        let down loc = 
            match loc.tree with
            | Branch (l, ls) ->
                let rec loop l (ls: _ list) (rs) =
                    match rs |> Seq.tryHead with
                    | Some h -> 
                        let rs = rs |> Seq.tail 
                        let ctx = BranchLazyCtx (l, ls, lazy Top, rs)
                        let rec loop2 lctx =
                            match lctx with
                            | Top -> ctx
                            | BranchLazyCtx (l, ls, c, rs) ->
                                let c = c.Value
                                BranchLazyCtx (l, ls, lazy (loop2 c), rs)

                        let loc = {
                            tree = h
                            ctx = ctx
                        }
                        seq {
                            loc 
                            yield! loop l (h::ls) rs
                        }
                    | None -> Seq.empty
                loop l [] ls

module ReduUndo =
    
    type RedoData<'T> = 
        | RedoData of ('T * RedoData<'T>) list
    module RedoData =
        let toList (RedoData ls) = ls
    type UndoData<'T> =
        | NilUndoData
        | UndoData of ('T * UndoData<'T> * RedoData<'T> * RedoData<'T>)

    type RedoUndo<'T> =
        {
            current: 'T
            redoData: RedoData<'T>
            undoData: UndoData<'T>
        }

    module RedoUndo =

        let current redoUndo =
            redoUndo.current
        let setCurrent current redoUndo  =
            {
                current = current
                redoData = RedoData []
                undoData = UndoData (redoUndo.current, redoUndo.undoData, RedoData [], redoUndo.redoData)
            }
        let create current =
            {
                current = current
                redoData = RedoData []
                undoData = NilUndoData
            }
                
        let redo i redoUndo  =
            let rec loop i ls rev =
                match ls with
                | [] -> None
                | x::xs ->
                    if i = 0 then
                        Some (rev |> List.rev, x, xs)
                    else 
                        loop (i-1) xs (x::rev)
            
            if i < 0 then None
            else
            let (RedoData ls) = redoUndo.redoData
            loop i ls []
            |> Option.map (fun (l, (c, t), r) ->
                {
                    current = c
                    redoData = t
                    undoData = UndoData (redoUndo.current, redoUndo.undoData, RedoData l, RedoData r)
                })
        let undo redoUndo =
            match redoUndo.undoData with
            | NilUndoData -> None
            | UndoData (c, u, (RedoData l), (RedoData r)) ->
                {
                    current = c
                    undoData = u
                    redoData = [
                        yield! l
                        yield (redoUndo.current, redoUndo.redoData)
                        yield! r
                    ] |> RedoData
                } |> Some
        let origin redoUndo =
            let rec loop redoUndo =
                match undo redoUndo with
                | None -> redoUndo
                | Some redoUndo -> loop redoUndo
            loop redoUndo
        let ofNonEmptyTree tree =
            let rec loop tree =
                match tree with
                | NonEmptyTree.Branch (l, ls) ->
                    let ts = ls |> List.map (loop>> fun redoUndo -> redoUndo.current, redoUndo.redoData)
                    {
                        current = l
                        undoData = NilUndoData
                        redoData = ts |> RedoData
                    }
            loop tree
        let allRedo redoUndo =
            let rec loop ls rev cnt =
                match ls with
                | [] -> cnt []
                | x::xs ->
                    let (l, (c, t), r) = rev |> List.rev, x, xs
                    let ru = {
                        current = c
                        redoData = t
                        undoData = UndoData (redoUndo.current, redoUndo.undoData, RedoData l, RedoData r)
                    } 
                    loop xs (x::rev) (fun ans -> ru::ans |> cnt)
            let (RedoData ls) = redoUndo.redoData
            loop ls [] id
        


module CubicBezier =
    open Vector
    type Pos = Vector2<float>
    let leftTangent p1 p2 =
        p2 - p1 
        |> Vector2.normalize 
    let rightTangent p1 p2 = leftTangent p2 p1

    let centerTangent (p1: Pos) p2 p3 =
        let v1 = p1 - p2
        let v2 = p2 - p3
        let tHatCenter = (v1+v2) |> Vector2.normalize
        tHatCenter

    let chordLengthParameterize (d: Pos []) =
        let u = Array.zeroCreate d.Length
        u.[0] <- 0.
        for i in 1.. d.Length - 1 do
            u.[i] <- u.[i-1] + Vector2.distance d.[i] d.[i-1]
        for i in 1.. d.Length - 1 do
            u.[i] <- u.[i] / u.[d.Length - 1]
        u
    let b1 u =
        let tmp = 1. - u
        3. * u * tmp * tmp
    let b2 u =
        let tmp = 1. - u
        3. * u * u * tmp
    let b3 u =
        u * u * u
    let generateBezier1 d0 d1 (tHat1: Pos) (tHat2: Pos)  =
        let dist = Vector2.distance d0 d1 / 3.
        let bezCurve = Array.zeroCreate 4
        bezCurve.[0] <- d0
        bezCurve.[3] <- d1
        bezCurve.[1] <- d0 + tHat1 * dist
        bezCurve.[2] <- d1 + tHat2 * dist
        bezCurve

    let generateBezier (ds: Pos []) (uPrime: float[]) (tHat1: Pos) (tHat2: Pos) =
        let bezCurve = Array.zeroCreate 4
        let nPts = ds.Length
        let A = Array.zeroCreate nPts
        for i in 0 .. nPts-1 do
            let v1 = tHat1 * b1 uPrime.[i]
            let v2 = tHat2 * b2 uPrime.[i]
            A.[i] <- [|v1; v2|]
        let mutable C = ((0., 0.), (0., 0.))
        let mutable X = (0., 0.)
        for i in 0.. nPts - 1 do
            let ((a, b), (c, d)) = C
            let Ai = A.[i]
            let a = a + Vector2.dot Ai.[0] Ai.[0]
            let b = b + Vector2.dot Ai.[0] Ai.[1]
            let c = b
            let d = d + Vector2.dot Ai.[1] Ai.[1]
            C <- ((a, b), (c, d))
            let tmp =   
                ds.[i] - (ds.[0] * uPrime.[i] + ds.[nPts-1] * b2 uPrime.[i] + ds.[nPts - 1] * b3 uPrime.[i])
            let (x, y) = X
            X <- x + Vector2.dot Ai.[0] tmp, y + Vector2.dot Ai.[1] tmp
        let (C0, C1) = C
        let det (a, b) (c, d) = a * d - b * c
        let detC0C1 = det C0 C1
        let detC0X = det C0 X
        let detXC1 = det X C1
        
        let alphaL = if detC0C1 = 0. then 0. else detXC1 / detC0C1
        let alphaR = if detC0C1 = 0 then 0. else detC0X / detC0C1
        let segLength = Vector2.distance ds.[0] ds.[nPts-1]
        let epsilon = 1.0e-6 * segLength
        if alphaL < epsilon || alphaR < epsilon then
            generateBezier1 ds.[0] ds.[nPts-1] tHat1 tHat2 
        else 
        let bezCurve = Array.zeroCreate 4
        bezCurve.[0] <- ds.[0]
        bezCurve.[3] <- ds.[nPts-1]
        bezCurve.[1] <- ds.[0] + tHat1 * alphaL
        bezCurve.[2] <- ds.[nPts-1] + tHat2 * alphaR
        bezCurve
    
    let bezier2 degree (V: Pos []) t =
        let Vtemp = degree + 1 |> Array.zeroCreate
        for i in 0 .. degree do
            Vtemp.[i] <- V.[i]
        for i in 1.. degree do
            for j in 0.. degree - i do
                Vtemp.[j] <- Vtemp.[j] * (1. - t) + (Vtemp.[j+1] * t)

        let Q = Vtemp.[0]
        Q

    let computeMaxError (d: Pos []) (bezCurve: _ []) (u: _ []) =
        let mutable splitPoint' = d.Length / 2
        let mutable maxDist = 0.
        for i in 1.. d.Length - 2 do
            let P = bezier2 3 bezCurve u.[i]
            let v = P - d.[i]
            let dist = Vector2.length2 v
            if dist >= maxDist then
                maxDist <- dist
                splitPoint' <- i
        maxDist, splitPoint'
    
    let newtonRaphsonRootFind Q P u =
        let Q_u = bezier2 3 Q u
        let Q1 = Array.zeroCreate 3
        for i in 0 .. 2 do
            Q1.[i] <- Q[i+1] - Q.[i] * 3.
        let Q2 = Array.zeroCreate 2
        for i in 0 .. 1 do
            Q2.[i] <- Q.[i+1] - Q.[i] * 2.
        let Q1_u = bezier2 2 Q1 u
        let Q2_u = bezier2 1 Q2 u
        let numerator = (Q_u.x - P.x) * (Q1_u.x) + (Q_u.y - P.y) * (Q1_u.y)
        let denominator = 
            (Q1_u.x) * Q1_u.x + Q1_u.y * Q1_u.y + 
                (Q_u.x - P.x) * (Q2_u.x) + (Q_u.y - P.y) * Q2_u.y
        if denominator = 0. then u
        else 

        let uPrime = u - (numerator / denominator)
        uPrime


    let reparameterize (d: _ []) (u: _ []) bezCurve =
        // let nPts = d.Length
        // let uPrime = nPts |> Array.Create
        // for i in 0.. nPts-1 do  
        //     uPrime.[i] <- newtonRaphsonRootFind bezCurve d.[i] u.[i]
        let uPrime = (d, u) ||> Array.map2 (newtonRaphsonRootFind bezCurve)
        uPrime
    let rec fitCubic maxIterations error d tHat1 tHat2 =
        let rec fitCubic (d: Pos []) tHat1 tHat2 =
            let iterationError = error * 4.0
            let nPts = d.Length
            if nPts = 2 then
                [generateBezier1 d.[0] d.[1] tHat1 tHat2]
            else

            let mutable u = chordLengthParameterize d
            let bezCurve = generateBezier d u tHat1 tHat2
            let maxError, splitPoint = computeMaxError d bezCurve u
            if maxError < error then    
                [bezCurve]
            else
            let mutable splitPoint = splitPoint
            let result =
                if maxError < iterationError then
                    let rec loop restIterations =
                        if restIterations = 0 then 
                            let uPrime = reparameterize d u bezCurve
                            let bezCureve = generateBezier d uPrime tHat1 tHat2
                            let maxError, splitPoint' = computeMaxError d bezCureve uPrime 
                            splitPoint <- splitPoint'
                            (Some bezCurve)
                        else 
                        let uPrime = reparameterize d u bezCurve
                        let bezCureve = generateBezier d uPrime tHat1 tHat2
                        let maxError, splitPoint' = computeMaxError d bezCureve uPrime 
                        splitPoint <- splitPoint'
                        if maxError < error then
                            (Some bezCurve)
                        else
                            u <- uPrime
                            loop (restIterations - 1) 
                    loop maxIterations
                else
                    None
            match result with
            | (Some bezCurve) -> [bezCurve]
            | None ->
                let tHatCenter = centerTangent d.[splitPoint - 1] d.[splitPoint] d.[splitPoint + 1]
                [
                    yield! fitCubic d.[0.. splitPoint] tHat1 tHatCenter
                    let tHatCenter = tHatCenter * (-1.)
                    yield! fitCubic d.[splitPoint .. nPts-1] tHatCenter tHat2
                ]
        fitCubic d tHat1 tHat2
    let fitCurve maxIterations (d: Pos []) (error: float) =
        let nPts = d.Length
        let tHat1 = leftTangent d.[0] d.[1]
        let tHat2 = - leftTangent d.[nPts - 2] d.[nPts - 1]
        fitCubic maxIterations error d tHat1 tHat2



    



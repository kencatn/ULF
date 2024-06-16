module Utils
open Common
module JS =
    open Fable.Core.JS

    let debugPrint print = 
        #if DEBUG
            printfn "%O" print
        #else
            ()
        #endif

    let error data =  
        console.error data

    let warn data =
        console.warn data

    let log data =
        console.log data

    let debugLog data =
        #if DEBUG    
            console.log data
        #else
            ()
        #endif

module Svg =
    open Browser.Types
    open Vector
    let screenPosToSvgPos (svg: SVGSVGElement) (pos) = 
        let p = svg.createSVGPoint()
        p.x <- pos.x
        p.y <- pos.y
        let ctm = svg.getScreenCTM()
        let p = p.matrixTransform(ctm.inverse())
        {x=p.x;y=p.y}

    let svgPosToScreenPos (svg: SVGSVGElement) (pos) =
        let p = svg.createSVGPoint()
        p.x <- pos.x
        p.y <- pos.y
        let ctm = svg.getScreenCTM()
        let p = p.matrixTransform(ctm)
        {x=p.x;y=p.y}
    let svgSvgElemnt (svg: obj) =
        match svg with
        | :? SVGSVGElement as svg -> svg
        | :? SVGElement as svg -> svg.ownerSVGElement
        | _ -> 
            failwith "it is not svg element"
    open Feliz
    let arrow (w: float) (l: float) p1 p2  attrs =
        let d = p2 - p1 |> Vector2.normalize
        let L = v(d.y, - d.x)
        let t1 = p2 + L * w - d * l
        let t2 = p2 - L * w - d * l
        
        Svg.path [
            svg.d ([
                'M', [p1]
                'L', [p2]
                'L', [t1]
                'L', [t2]
                'L', [p2]
            ] |> List.map (fun (a, b) -> a, b |> List.map Vector2.toList))
            yield! attrs
        ]

module Drag =
    open Vector
    open Browser.Types
    type Target<'TargetType> = {
        targetType: 'TargetType
        position: Vector2<float>
        event: PointerEvent
    }
    type DragState<'TargetType, 'DragData> =
        {
            source: Target<'TargetType>
            all: Target<'TargetType> list
            target: Target<'TargetType>
            dragData: 'DragData
        }
    type DragMsg<'TargetType, 'DragData> =
        | DragStart of 'DragData * Target<'TargetType>
        | Dragging of Target<'TargetType>
        | DragEnd
        | Click of Target<'TargetType>

    type DragOutMsg<'TargetType, 'DragData> =
        | DragStarted of DragState<'TargetType, 'DragData>
        | Drag of DragState<'TargetType, 'DragData>
        | DragComplete of DragState<'TargetType, 'DragData>
        | Clicked of Target<'TargetType>

    let updateDragMsg msg dragState =
        match msg with
        | DragStart (d, t) -> 
            let ds = {source = t; all = [t]; target = t;dragData = d}
            Some ds, [
                DragStarted ds
            ]
        | Dragging t -> 
            let dragState = dragState |> Option.map (fun a -> {a with all = t::a.all; target = t})
            dragState , 
            [
                match dragState with 
                | None -> () 
                | Some ds -> Drag ds ]
        | DragEnd -> 
            None,
            match dragState with
            | None -> []
            | Some a -> 
                if a.source.targetType = a.target.targetType && (a.source.position - a.target.position |> Vector2.length2 < 5**2) then
                    [Clicked a.target]
                else
                    [DragComplete a]
        | Click t -> dragState, [Clicked t]
    open Feliz
    let events dragState target dispatch =
        [
            svg.classes ["pointer-events-all"]
            svg.onPointerDown (fun e -> 
                let svg = e.target |> Svg.svgSvgElemnt
                let p = Svg.screenPosToSvgPos svg (v(e.clientX, e.clientY))
                DragStart ((), {targetType = target; position = p; event = e}) |> dispatch)
            svg.onPointerUp (fun e ->
                let svg = e.target |> Svg.svgSvgElemnt
                let p = Svg.screenPosToSvgPos svg (v(e.clientX, e.clientY))
                DragEnd |> dispatch)
            match dragState with
            | None -> ()
            | Some _ ->
                svg.onPointerMove (fun e -> 
                    let svg = e.target |> Svg.svgSvgElemnt
                    let p = Svg.screenPosToSvgPos svg (v(e.clientX, e.clientY))
                    Dragging {targetType = target; position = p; event = e} |> dispatch)
        ]
module EventUtils =
    open Browser.Types
    let addEventListener (event: string) (listener: Event -> unit) options (elm: EventTarget): unit =
        Fable.Core.JsInterop.emitJsExpr (elm, event, listener, options)
            "$0.addEventListener($1,$2,$3)"

module Sub =
    open Browser.Types
    open Browser.Dom
    open System
    open EventUtils
    let keyDownSub f =
        let key dispatch =
            let g (e: Event) = 
                let e = (e :?> KeyboardEvent)
                f e dispatch
            document.addEventListener("keydown", g)
            {
                new IDisposable with
                    member _.Dispose() = 
                        document.removeEventListener("keydown", g)
            }
        key
    let preventDefault event = 
        let sub dispatch =
            let f (e: Event) =  e.preventDefault()
            Browser.Dom.window |> addEventListener event f {|passive=false|} 
            {
                new IDisposable with
                    member _.Dispose() =
                        window.removeEventListener (event, f)
            }
        sub
    let preventContextmenue() = 
        let sub dispatch =
            Browser.Dom.document.oncontextmenu <- fun e -> e.preventDefault()
            Browser.Dom.window.oncontextmenu <- fun e -> e.preventDefault()
            {
                new IDisposable with
                    member _.Dispose() =
                        document.oncontextmenu <- ignore
                        window.oncontextmenu <- ignore
            }
        sub
module KeyConfig =
    open System
    open Browser.Types
    open Fable.Core.JsInterop
    [<CustomComparisonAttribute; CustomEqualityAttribute>]
    type KeyBoard =
        {
            key: string
            ctrl: bool
            alt: bool
            shift: bool
        }
    with
        interface IComparable with
            member this.CompareTo key =
                match key with
                | null -> 1
                | :? KeyBoard as other-> 
                    compare (this.key, this.ctrl, this.alt, this.shift) (other.key, other.ctrl, other.alt, other.shift)
                | _ -> 
                    
                        
                    compare (this.key, this.ctrl, this.alt, this.shift) (key?key, key?ctrl, key?alt, key?shift)
        override this.Equals key =
            match key with
            | :? KeyBoard as other ->
                (=) (this.key, this.ctrl, this.alt, this.shift) (other.key, other.ctrl, other.alt, other.shift)
            | _ ->
                (=) (this.key, this.ctrl, this.alt, this.shift) (key?key, key?ctrl, key?alt, key?shift)
        override this.GetHashCode () =
            hash  (this.key, this.ctrl, this.alt, this.shift) 

    module KeyConfig =
        let parseSingleKeyConfig (key: string): KeyBoard =
            key.Split " "
            |> Array.fold (fun s t ->
                match t with
                | "ctrl" -> {s with ctrl = true}
                | "alt" -> {s with alt = true}
                | "shift" -> {s with shift = true}
                | key -> {s with key = key}
            ) {key = ""; ctrl = false; alt = false; shift = false}

        let parseKeyConfig config = config |> List.map (fun (a, b) -> parseSingleKeyConfig a, b)

        let keyMap keyConfig = 
            let keyMap =
                keyConfig
                |> Map.ofSeq
            keyMap
        
        let keyToMsg keyMap (e: KeyboardEvent) = 
            let key = e
            let keyboard = {
                key = key.key
                ctrl = key.ctrlKey
                alt = key.altKey
                shift = key.shiftKey
            }
            let found =
                keyMap
                |> Map.tryFind keyboard
            found |> function
            | None -> []
            | Some a -> [a]
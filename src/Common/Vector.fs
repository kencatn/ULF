module Common.Vector



type Vector2<'T> = {
    x:'T
    y:'T
}
with
    static member inline (+) (v1,v2) = {x=v1.x+v2.x;y=v1.y+v2.y}
    static member inline (-) (v1,v2) = {x=v1.x-v2.x;y=v1.y-v2.y}

    static member inline (~-) (v) = {x= -v.x;y= -v.y}
    static member inline DivideByInt (v, i:int) = {x=LanguagePrimitives.DivideByInt v.x i; y= LanguagePrimitives.DivideByInt v.y i}
    static member inline (/) (v,c) = {x=v.x/c;y=v.y/c}
    static member inline (*) (v,c) = {x=v.x*c;y=v.y*c}
    static member inline (*) (v1, v2) = {x=v1.x*v2.x;y=v1.y*v2.y}
    static member inline Zero() = {x= LanguagePrimitives.GenericZero;y=LanguagePrimitives.GenericZero}
    static member inline get_Zero() = {x= LanguagePrimitives.GenericZero;y=LanguagePrimitives.GenericZero}

let inline v(x,y) ={x=x;y=y}
module Vector2 =
    let x v = v.x
    let y v = v.y
    let inline scale v1 v2 = v(v1.x * v2.x, v1.y * v2.y)
    let map f p = {x = f p.x; y = f p.y}
    let inline float p = {x = float p.x; y = float p.y}
    let map2 f v1 v2 = {x = f v1.x v2.x; y = f v1.y v2.y}
    let toSeq v = seq{v.x; v.y}
    let toList v = [v.x;v.y]
    let toTuple {x=x;y=y} = x, y
    let ofTuple (x, y) = {x=x;y=y}
    let create x y = {x=x; y=y}

    let inline dot v1 v2 = v1.x * v2.x + v1.y * v2.y

    let inline length2 v = dot v v
    let length v =
        v.x * v.x + v.y * v.y |> System.Math.Sqrt
    let distance v1 v2 = (v1 - v2) |> length
    let normalize v = 
        let l = length v
        if l <> 0. then
            v / (length v)
        else v
    let tryNormalize v =
        let l = length v
        if l = 0. then
            None 
        else
            v / l |> Some

    let inline normal v = {x=v.y; y = -v.x}
    let inline inverse v = {x= -v.x;y= -v.y}


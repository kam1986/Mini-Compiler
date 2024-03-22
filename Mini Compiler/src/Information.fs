module Information


// we need to track the position in the source code
// to give meaningful errors back to the user, 
// and to troubleshoot the compiler
type [<Struct>] Position =
     val Line: int
     val Offset: int
     val Absolut: int
     internal new(l,o,a) =
        {
            Line = l
            Offset = o
            Absolut = a
        }

     override p.ToString() = $"({p.Line}, {p.Offset})"


module Pos =
    let Move (pos: Position) steps = Position(pos.Line, pos.Offset + steps, pos.Absolut + steps)
    let NewLine(pos: Position) = Position(pos.Line + 1, 0, pos.Absolut + 1)




// this will might be extended at a later point
type Info =
    {
        StartsAt: Position
        EndsAt: Position
        
    }

type [<Interface>] Information =
    abstract member GetInfo: Info 


let Info startsat endsat = 
    {
        StartsAt = startsat
        EndsAt = endsat
    }

let GetInfo (item: Information) = item.GetInfo
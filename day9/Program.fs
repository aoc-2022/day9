open System.IO

exception Fail
let input = File.ReadAllLines "/tmp/aoc/input" |> Array.toList

type Move(dx: int, dy: int) =
    member this.Dx = dx
    member this.Dy = dy

    member this.Expand =
        let rec exp (move: Move) =
            match move.Dx, move.Dy with
            | (0, 0) -> []
            | (x, 0) when x < 0 -> Move(-1, 0) :: exp (Move(x + 1, 0))
            | (x, 0) when x > 0 -> Move(1, 0) :: exp (Move(x - 1, 0))
            | (0, y) when y < 0 -> Move(0, -1) :: exp (Move(0, y + 1))
            | (0, y) when y > 0 -> Move(0, 1) :: exp (Move(0, y - 1))

        exp this

    override this.ToString() = $"Move({dx},{dy})"

type Point(x: int, y: int) =
    member this.X = x
    member this.Y = y
    member this.Move(move: Move) = Point(x + move.Dx, y + move.Dy)
    member this.NextTo(p: Point) = abs (x - p.X) < 2 && abs (y - p.Y) < 2

    member this.Catchup(p: Point) =
        if p.NextTo this then
            printfn $"{this} is next to {p}"
            this
        else
            let dx = (p.X - x)
            let dx = if dx = 0 then 0 else dx / abs (dx)
            let dy = (p.Y - y)
            let dy = if dy = 0 then 0 else dy / abs (dy)
            let x = x + dx
            let y = y + dy
            printfn $"move from {this} to {(x, y)} to catch {p}"
            Point(x, y)

    override this.ToString() = $"Point({x},{y})"

type Rope (knots:Point list) =
    member this.Knots = knots
    member this.Head = knots.Head
    
    member this.MoveHead move =
        Rope (knots.Head.Move move :: knots.Tail)
    member this.Catchup () : Rope =
        let rec catchupTail (knots: Point list) =
            match knots with
            | [] -> []
            | [x] -> [x]
            | a::b::rest ->
                let b = b.Catchup a 
                a::(catchupTail (b::rest))
        let knots = catchupTail knots
        Rope (knots)
    member this.Tail =
        knots |> List.skip (knots.Length - 1) |> List.head 

type Map(visited: Set<int * int>, rope: Rope) =
    member this.Visited = visited
    member this.Head = rope.Head 
    member this.Tail = rope.Tail 

    member this.Move(move: Move) =
        let rope = rope.MoveHead move
        let rope = rope.Catchup ()
        // let visited = visited.Add((head.X, head.Y))
        let visited = visited.Add((rope.Tail.X, rope.Tail.Y))
        Map(visited, rope)

    static member start = Map(Set.singleton (0, 0), Rope([Point(0,0);Point(0,0)]))
    static member start2 = Map(Set.singleton (0,0), Rope(Point(0,0) |> List.replicate 10))

let rec processInput (lines: string list) =
    match lines with
    | [] -> []
    | line :: tail ->
        let line = line.Split [| ' ' |] |> Array.toList

        let move =
            match line with
            | [ "U"; steps ] -> Move(0, steps |> int)
            | [ "D"; steps ] -> Move(0, -(steps |> int))
            | [ "L"; steps ] -> Move(-(steps |> int), 0)
            | [ "R"; steps ] -> Move(steps |> int, 0)

        move :: processInput tail

let lines = processInput input

let smoves = lines |> List.map (fun move -> move.Expand) |> List.concat


let rec process (map: Map) (moves: Move list) =
    match moves with
    | [] -> map
    | move :: moves ->
        let map = map.Move move
        process map moves


lines |> List.map (printfn "%A")
smoves |> List.map (printfn "%A")

let xx = smoves |> List.take 6

printfn $"XX = "
xx |> List.map (printfn "%A")
let endMap = process Map.start smoves
printfn "MAP = "
endMap.Visited |> List.ofSeq |> List.map (printfn "%A")

printfn $"MAP.size = {endMap.Visited.Count}"

let rope = Rope([Point(0,0);Point(0,0)])

printfn $"rope.tail = {rope.Tail}"

printfn "### TASK 2 ###"
let endMap2 = process Map.start2 smoves

printfn $"MAP.size = {endMap2.Visited.Count}"

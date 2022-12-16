open Canvas

type Color = 
    | Yellow
    | Cyan
    | Blue
    | Orange 
    | Red
    | Green
    | Purple

type board (w: int, h: int) =
    let _board = Array2D.create w h None 
    do _board.[0 ,1] <- Some Green
    member this.width = w
    member this.height = h
    member this.board with get() = _board
type state = board

let draw (w: int) (h: int) (s: state) =
  let squareSize = 30
  let canvas = Canvas.create (w) (h)

  for y in 0..(s.height-1) do
    for x in 0..(s.width-1) do
        match s.board.[x, y] with
        | Some color ->
            let x1 = x * squareSize
            let y1 = y * squareSize
            let x2 = x1 + squareSize
            let y2 = y1 + squareSize

            let brush =
                match color with
                | Yellow -> yellow
                | Cyan -> fromRgb(224,255,255)
                | Blue -> blue
                | Orange -> fromRgb(255,140,0)
                | Red -> red
                | Green -> green
                | Purple -> fromRgb(128,0,128)

            setFillBox canvas brush (x1, y1) (x2, y2)
        | None ->
            let x1 = x * squareSize
            let y1 = y * squareSize
            let x2 = x1 + squareSize
            let y2 = y1 + squareSize
            setFillBox canvas white (x1, y1) (x2, y2)

  canvas
// insert your definition of draw here
let b = board(10, 20)
let C = draw 300 600 b 
show C "testing"


type position = int*int
type tetromino =
    
    // /// The constructor with its initial shape , its final color , and its inital offset
    new (a: bool[,], c: Color, o: position) =
        tetromino(a, c, o)

    /// Make a string representation of this piece
    // override ToString() with 
        

    /// Make a deep copy of this piece
    member this.clone() : tetromino =
        let img = this.image
        let n = img.GetLength(0)
        let m = img.GetLength(1)
        let newImg = Array2D.zeroCreate n m
        for i in 0..(n - 1) do
            for j in 0..(m - 1) do
                newImg.[i, j] <- img.[i, j]
        tetromino(newImg, this.col, this.offset)

    /// Rotates the piece 90 degrees clock-wise such that its left-top offset is maintained
    member this.rotateRight() =
        let img = this.image
        let n = img.GetLength(0)
        let m = img.GetLength(1)
        let newImg = Array2D.zeroCreate m n
        for i in 0..(n - 1) do
            for j in 0..(m - 1) do
                newImg.[j, n-1-i] <- img.[i, j]
        this.image <- newImg
  
    /// The piece ' color
    member this.col with get() = this.col and set(color:Color) = this.col <- color

    /// The present height of the shape 
    member this.height =
        let img = this.image
        img.GetLength(0)
    /// The piece ' present shape 
    member this.image with get() : bool[,] = this.image and set(value) = this.image <- value

    /// The piece ' present offset
    member this.offset with get() = this.offset and set(pos:position) = this.offset <- pos
    /// The present width of the shape 
    member this.width = 
        let img = this.image
        img.GetLength(1) 

type square (offset) =
    inherit tetromino(Array2D.create 2 2 true, Yellow, offset)

type straight (offset) =
    inherit tetromino(Array2D.create 1 4 true, Cyan, offset)


type t =
    inherit tetromino(Array2D.create



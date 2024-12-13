module Advent_of_Code.Coordinates64

[<Struct>]
type Coordinates64 =
    { X: int64
      Y: int64 }

[<RequireQualifiedAccess>]
module Coordinates64 =
    let zero = { X = 0; Y = 0 }

    let move (moveBy: Coordinates64) (index: Coordinates64) =
        { index with
            X = index.X + moveBy.X
            Y = index.Y + moveBy.Y }

    let moveBack (moveBy: Coordinates64) (index: Coordinates64) =
        { index with
            X = index.X - moveBy.X
            Y = index.Y - moveBy.Y }
module FunTypeProvider

open ChessGame

type SimpleGame = Game.Start.``♙D2D4``.``♟C7C5``.``♙E2E3``.``♟C5D4``.``♙E3D4``
let game = new SimpleGame()
printfn "%s" (game.ToString())
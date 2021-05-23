module ShooterGame

open FSRP.Core
open FSRP.Signal
open System.Threading

type GameState = 
    | InGame
    | PreGame

type Input = 
    | Left
    | Right
    | Shoot
    | Start
    | Exit
    | Nothing

type Position = float * float

type Direction = int

type Projectile = Position * Direction

type Player = {
     Position : Position
     Direction: Direction
     Projectiles : Projectile list
     CurrentScore : int
}
let movePlayerAndProjectilesInCurrentDirection player = player

let updatePlayer playerBox input = playerBox    

let getRandomInput () : Input =
    let idx = System.Random().Next 6
    match idx with
    | 0 -> Left
    | 1 -> Right
    | 2 -> Shoot
    | 3 -> Start 
    | 4 -> Exit
    | _ -> Nothing

let getPlayersInput () : (Input * Input) = (getRandomInput (), getRandomInput ())

let drawToScreen player1 player2 = printfn "%A" player1

[<FSRP>]
let updatePlayerSignal (initPlayer : Player) (inputSignal : Signal<Input>) = 
    scan (box (lazy updatePlayer)) (box (lazy initPlayer)) inputSignal

[<FSRP>]
let preGameSF (initPlayer1: Player) (initPlayer2: Player) 
              (inputSignal : Signal<Input * Input>) = 
    constant (box (lazy (initPlayer1, initPlayer2)))

[<FSRP>]
let inGameSF initPlayer1 initPlayer2 (inputSignal : Signal<Input * Input>) = 
    let inputSignal1 = map (box (lazy fst)) inputSignal
    let inputSignal2 = map (box (lazy snd)) inputSignal
    zip (
            (updatePlayerSignal initPlayer1 inputSignal1), 
            (updatePlayerSignal initPlayer2 inputSignal2)
        )
        
[<FSRP>]
let gameStateTrigger initPlayer1 initPlayer2 (gameState, (input1, input2)) = 
    match (gameState, input1, input2) with 
    | (PreGame, Start, _) | (PreGame, _, Start) -> 
        Some(inGameSF initPlayer1 initPlayer2)
    | (InGame, Exit, _) | (InGame, _, Exit) -> 
        Some(preGameSF initPlayer1 initPlayer2)
    | _ -> None

[<FSRP>]
let gameStateEvent initPlayer1 initPlayer2 
                   (gStateAndInputSignal: Signal<GameState * (Input * Input)>) =
    map 
        (box (lazy (gameStateTrigger initPlayer1 initPlayer2))) 
        gStateAndInputSignal

[<FSRP>]
let rec gameStateSF (gameState: GameState) 
                    (((input1, input2) :: inputTl) : Signal<Input * Input>) =
    let nextGameState = 
        match (gameState, input1, input2) with
        | (PreGame, Start, _) | (PreGame, _, Start) -> 
            InGame
        | (InGame, Exit, _) | (InGame, _, Exit) -> 
            PreGame
        | _ -> gameState
    gameState :: delay (lazy (gameStateSF nextGameState (adv inputTl)))

[<FSRP>]
let shooterGameSF initPlayer1 initPlayer2 (inputSignal : Signal<Input * Input>) = 
    let gameStateSignal = gameStateSF PreGame inputSignal
    let gStateAndInputSignal = (zip (gameStateSignal, inputSignal))
    switchFun 
        (preGameSF initPlayer1 initPlayer2) 
        (gameStateEvent initPlayer1 initPlayer2 gStateAndInputSignal) 
        inputSignal

let start () = 
    let player1 = {
        Position = (0.0, 0.0);
        Direction = 0;
        Projectiles = [];
        CurrentScore = 0;
    }

    let player2 = {
        Position = (50.0, 50.0);
        Direction = 180;
        Projectiles = [];
        CurrentScore = 0;  
    }

    let rec run input (Eval(eval)) =
        let ((player1, player2), eval') = eval (input ())
        Thread.Sleep 10
        drawToScreen player1 player2
        run input eval'

    let evaluator = buildEvaluator (shooterGameSF player1 player2)
    printfn "Starting game"
    run getPlayersInput evaluator



    
  



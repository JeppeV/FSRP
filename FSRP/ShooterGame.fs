module ShooterGame

open FSRP.Core
open FSRP.Library

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
     Position : Position;
     Direction: Direction;
     Projectiles : Projectile list
     CurrentScore : int
     Alive : bool
}
let movePlayerAndProjectilesInCurrentDirection player = player

let updatePlayer playerBox input = 
    playerBox        

let drawToScreen player1 player2 = printfn "p1: %A \n p2: %A" player1 player2


let getRandomInput () : Input =
    let idx = System.Random().Next 6
    match idx with
    | 0 -> Left
    | 1 -> Right
    | 2 -> Shoot
    | 3 -> Start 
    | 4 -> Exit
    | _ -> Nothing

let updatePlayerSignal initPlayer (inputSignal : Signal<Input>) = 
    let signal = scan (box (lazy updatePlayer)) (box (lazy initPlayer)) inputSignal
    map (box (lazy unbox)) signal

let preGameStateSignal = constant (box (lazy (PreGame)))

let inGameStateSignal = constant (box (lazy (InGame)))

let preGameSignal (initPlayer1: Player) (initPlayer2: Player) (gameStateAndInputSignal : Signal<GameState * (Input * Input)>) = 
    zip (preGameStateSignal, constant (box (lazy (initPlayer1, initPlayer2))))

let inGameSignal (initPlayer1: Player) (initPlayer2: Player) (gameStateAndInputSignal : Signal<GameState * (Input * Input)>) = 
    let inputSignal = map (box (lazy snd)) gameStateAndInputSignal
    let inputSignal1 = map (box (lazy fst)) inputSignal
    let inputSignal2 = map (box (lazy snd)) inputSignal
    (zip (inGameStateSignal, (zip ((updatePlayerSignal initPlayer1 inputSignal1), (updatePlayerSignal initPlayer2 inputSignal2)))))

let gameStateTrigger initPlayer1 initPlayer2 (gameState, (input1, input2)) = 
    match (gameState, input1, input2) with 
    | (PreGame, Start, _) | (PreGame, _, Start) -> 
        printfn "\nCHANGE GAMESTATE TO inGameSignal\n"; 
        Some(inGameSignal initPlayer1 initPlayer2)
    | (InGame, Exit, _) | (InGame, _, Exit) -> printfn "\nCHANGE GAMESTATE TO preGameSignal\n"; Some(preGameSignal initPlayer1 initPlayer2)
    | _ -> None

let gameStateEvent (initPlayer1: Player) (initPlayer2: Player) (gameStateAndInputSignal : Signal<GameState * (Input * Input)>) =
    map (box (lazy (gameStateTrigger initPlayer1 initPlayer2))) gameStateAndInputSignal

let shooterGame (initPlayer1: Player) (initPlayer2: Player) (inputSignal : Signal<Input * Input>) = 

    let gameStateAndInputSignal : Signal<GameState * (Input * Input)> = zip (preGameStateSignal, inputSignal)
    
    switchFun (preGameSignal initPlayer1 initPlayer2) (gameStateEvent initPlayer1 initPlayer2 gameStateAndInputSignal) gameStateAndInputSignal

let run () = 
    let player1 = {
        Position = (0.0, 0.0);
        Direction = 0;
        Projectiles = [];
        CurrentScore = 0;
        Alive = true;
    }

    let player2 = {
        Position = (50.0, 50.0);
        Direction = 180;
        Projectiles = [];
        CurrentScore = 0;
        Alive = true;    
    }

    let readPlayerInputFun () : GameState * (Input * Input) = (InGame, (getRandomInput (), getRandomInput ())) // calculate some input
    let readPlayerInputFun () : (Input * Input) = (getRandomInput (), getRandomInput ()) // calculate some input

    let output (gameState, (player1, player2) : Player * Player) =
        drawToScreen player1 player2
        if not player1.Alive || not player2.Alive then
            false
        else true
        
    process_signal (shooterGame player1 player2) readPlayerInputFun output 300


    
  



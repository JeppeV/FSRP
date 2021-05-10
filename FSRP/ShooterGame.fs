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

let inGameSignal (initPlayer1: Player) (initPlayer2: Player) (inputSignal : Signal<Input * Input>) = 
    let inputSignal1 = map (box (lazy fst)) inputSignal
    let inputSignal2 = map (box (lazy snd)) inputSignal
    (zip ((updatePlayerSignal initPlayer1 inputSignal1), (updatePlayerSignal initPlayer2 inputSignal2)))

let startGameTrigger initPlayer1 initPlayer2 (input1, input2) = 
    match (input1, input2) with 
    | (Start, _) | (_, Start) -> printfn "Changing to inGameSignal"; Some(inGameSignal initPlayer1 initPlayer2)
    | _ -> None

let startGameEvent (initPlayer1: Player) (initPlayer2: Player) (inputSignal : Signal<Input * Input>) =
    (map (box (lazy (startGameTrigger initPlayer1 initPlayer2))) inputSignal)

let preGameSignal (initPlayer1: Player) (initPlayer2: Player) (inputSignal : Signal<Input * Input>) = 
    let constPlayerSignalFunction (initPlayer1: Player) (initPlayer2: Player) (_ : Signal<Input * Input>) = constant (box (lazy (initPlayer1, initPlayer2)))
    (switchFun (constPlayerSignalFunction initPlayer1 initPlayer2) (startGameEvent initPlayer1 initPlayer2 inputSignal) inputSignal)
    
let endGameTrigger (initPlayer1: Player) (initPlayer2: Player) (input1, input2) = 
    match (input1, input2) with 
    | (Exit, _) | (_, Exit) -> printfn "Changing to preGameSignal"; Some(preGameSignal initPlayer1 initPlayer2)
    | _ -> None

let shooterGame (initPlayer1: Player) (initPlayer2: Player) (inputSignal : Signal<Input * Input>) = 
    let endGameEvent = (map (box (lazy (endGameTrigger initPlayer1 initPlayer2))) inputSignal)

    switchFun (preGameSignal initPlayer1 initPlayer2) endGameEvent inputSignal

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

    let readPlayerInputFun () : Input * Input = (getRandomInput (), getRandomInput ()) // calculate some input

    let output ((player1, player2) : Player * Player) =
        drawToScreen player1 player2
        if not player1.Alive || not player2.Alive then
            false
        else true
        
    process_signal (shooterGame player1 player2) readPlayerInputFun output 1000


    
  



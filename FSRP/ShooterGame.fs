module ShooterGame

open FSRP.Core
open FSRP.Signal

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

[<FSRP>]
let updatePlayer () = box(lazy(fun playerBox input -> playerBox))     


let getRandomInput () : Input =
    let idx = System.Random().Next 6
    match idx with
    | 0 -> Left
    | 1 -> Right
    | 2 -> Shoot
    | 3 -> Start 
    | 4 -> Exit
    | _ -> Nothing

[<FSRP>]
let updatePlayerSignal (initPlayer : Player) (inputSignal : Signal<Input>) = 
    let signal = scan (updatePlayer ()) (box (lazy initPlayer)) inputSignal
    map (box (lazy unbox)) signal

[<FSRP>]
let preGameStateSignal () = constant (box (lazy (PreGame))) 

[<FSRP>]
let inGameStateSignal () = constant (box (lazy (InGame)))

[<FSRP>]
let preGameSF (initPlayer1: Player) (initPlayer2: Player) (gameStateAndInputSignal : Signal<GameState * (Input * Input)>) = 
    constant (box (lazy (initPlayer1, initPlayer2)))

[<FSRP>]
let inGameSF (initPlayer1: Player) (initPlayer2: Player) (gameStateAndInputSignal : Signal<GameState * (Input * Input)>) = 
    let inputSignal = map (box (lazy snd)) gameStateAndInputSignal
    let inputSignal1 = map (box (lazy fst)) inputSignal
    let inputSignal2 = map (box (lazy snd)) inputSignal
    (zip ((updatePlayerSignal initPlayer1 inputSignal1), (updatePlayerSignal initPlayer2 inputSignal2)))

[<FSRP>]
let gameStateTrigger initPlayer1 initPlayer2 (gameState, (input1, input2)) = 
    match (gameState, input1, input2) with 
    | (PreGame, Start, _) | (PreGame, _, Start) -> 
        printfn "\nCHANGE GAMESTATE TO inGameSignal\n"; 
        Some(inGameSF initPlayer1 initPlayer2)
    | (InGame, Exit, _) | (InGame, _, Exit) -> printfn "\nCHANGE GAMESTATE TO preGameSignal\n"; Some(preGameSF initPlayer1 initPlayer2)
    | _ -> None

[<FSRP>]
let gameStateEvent (initPlayer1: Player) (initPlayer2: Player) (gameStateAndInputSignal : Signal<GameState * (Input * Input)>) =
    map (box (lazy (gameStateTrigger initPlayer1 initPlayer2))) gameStateAndInputSignal


[<FSRP>]
let shooterGame (initPlayer1: Player) (initPlayer2: Player) (inputSignal : Signal<Input * Input>) = 
    let gameStateAndInputSignal : Signal<GameState * (Input * Input)> = zip (preGameStateSignal (), inputSignal)

    switchFun (preGameSF initPlayer1 initPlayer2) (gameStateEvent initPlayer1 initPlayer2 (zip (preGameStateSignal (), inputSignal))) (zip (preGameStateSignal (), inputSignal))

    //shooterGameSwitch initPlayer1 initPlayer2 gameStateAndInputSignal (gameStateEvent initPlayer1 initPlayer2 gameStateAndInputSignal) gameStateAndInputSignal

let drawToScreen player1 player2 = printfn "p1: %A \n p2: %A" player1 player2

[<FSRP>]
let stutter () = gen (box (lazy (fun tBox -> 
                        let (n, t) = unbox tBox
                        if t < 3 then
                            ((box (lazy (n, (t + 1)))), (box (lazy n)))
                        else
                            ((box (lazy (n + 1, 1))), (box (lazy (n + 1))))
                        ))) (box (lazy (1, 1)))

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

    let readPlayerInputFun () : (Input * Input) = (getRandomInput (), getRandomInput ()) // calculate some input

    let output ((player1, player2) : Player * Player) =
        //drawToScreen player1 player2
        if not player1.Alive || not player2.Alive then
            false
        else true

    let rec unfold input (Eval(eval)) =
        let ((player1, player2), eval') = eval (input ())
        drawToScreen player1 player2
        unfold input eval'

    let evaluator = buildEvaluator (shooterGame player1 player2)
    unfold readPlayerInputFun evaluator

    //unfold_signal (stutter ()) (fun i -> (printfn "%A" i) ; true) 0
    //process_signal (shooterGame player1 player2) readPlayerInputFun output 0
    //process_signal (map (box (lazy fun i -> i))) readPlayerInputFun (fun i -> true) 0


    
  



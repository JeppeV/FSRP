module TestInput

open FSRP.Core
open FSRP.Library

type Input = 
    | Left
    | Right
    | Shoot
    | None

type Position = float * float

type Direction = int

type Projectile = Position * Direction

type Player = {
     Position : Position;
     Direction: Direction;
     Projectiles : Projectile list
     CurrentScore : int
}
   

let turnRight direction = direction
let turnLeft direction = direction

let isProjectileOutOfBounds (position, float) = false

let movePlayerAndProjectilesInCurrentDirection player = player

let updatePlayer (player: Player) = 
    let updatedPlayer = (movePlayerAndProjectilesInCurrentDirection player)
    let projectiles = List.filter isProjectileOutOfBounds updatedPlayer.Projectiles
    { updatedPlayer with Projectiles = projectiles }
    
let updatePlayerFromInput player input = 
    match input with
    | Left -> { player with Direction = turnLeft player.Direction }
    | Right -> { player with Direction = turnRight player.Direction }
    | Shoot -> { player with Projectiles = List.Cons((player.Position, player.Direction), player.Projectiles) }
    | None -> player


let computeHits (player : Player) projectiles  = 
    List.fold (fun (count, updatedProjectiles) (projectilePosition, direction) ->
        if player.Position = projectilePosition then ((count + 1), updatedProjectiles)
        else (count, List.Cons((projectilePosition, direction), updatedProjectiles))
    ) (0, []) projectiles


//let main player1 player2 =
//    let accFun = box(lazy(fun boxedPlayers (input1, input2) ->
//        let (player1, player2) = unbox boxedPlayers
//        let updatedPlayer1 =  movePlayerAndProjectilesInCurrentDirection (updatePlayerFromInput player1 input1)
//        let updatedPlayer2 = movePlayerAndProjectilesInCurrentDirection (updatePlayerFromInput player2 input2)
//        let (player1HitCount, player2Projectiles) = computeHits updatedPlayer1 updatedPlayer2.Projectiles
//        let (player2HitCount, player1Projectiles) = computeHits updatedPlayer2 updatedPlayer1.Projectiles
//        let updatedPlayer1' = { updatedPlayer1 with CurrentScore = updatedPlayer1.CurrentScore + player1HitCount; Projectiles = player1Projectiles }
//        let updatedPlayer2' = { updatedPlayer2 with CurrentScore = updatedPlayer2.CurrentScore + player2HitCount; Projectiles = player2Projectiles }
//        box(lazy(updatedPlayer1', updatedPlayer2'))
//    ))
//    scan accFun (box(lazy((player1, player2))))
  

[<FSRP>]
let fib () = 

    let rec zero () = 
        0 :: delay(lazy(one ()))
    and one () =
        1 :: delay(lazy(rest 0 1))
    and rest prev1 prev2 = 
        let sum = prev1 + prev2
        sum :: delay(lazy(rest prev2 sum))
    zero ()


        

[<FSRP>]
let rec test4 (s: Signal<int>) (x : int) = match s with | hd :: tl -> hd :: delay(lazy (test4 (adv tl) x))



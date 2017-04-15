open System

let fireDistance = 9.0

type Entity =
    struct
        val id: int
        val entityType: string
        val x: int
        val y: int
        val arg1: int
        val arg2: int
        val arg3: int
        val arg4: int

        new(id, entityType, x, y, arg1, arg2, arg3, arg4) = {
            id = id
            entityType = entityType
            x = x
            y = y
            arg1 = arg1
            arg2 = arg2
            arg3 = arg3
            arg4 = arg4
        }
    end 


let distance (x1, y1) (x2, y2) =
    sqrt (float ((x1 - x2)*(x1 - x2) + (y1 - y2)*(x1 - x2)))

let distanceE (e1: Entity) (e2: Entity) =
    distance (e1.x, e1.y) (e2.x, e2.y)
    
// let closeEntity ship entities =



let rnd = System.Random()

// Don't fire twice!
// Methods for that

let mutable shipsFiredStatus = [] // (id, bool) list

let didShipFired shipId =
    shipsFiredStatus |> List.exists (fun (sId, fired) -> sId = shipId && fired)

let setShipFired shipId fired =
    let exceptShip = shipsFiredStatus |> List.filter (fun (sId, fired) -> sId <> shipId)
    shipsFiredStatus <- (shipId, fired) :: exceptShip



// Finding target for fire
// todo: consider fire mines
let fireTarget (ship: Entity) (enemies: Entity list) =
    match enemies with
    | [] ->
        None
    | enemies -> 
        let closestEnemy = enemies |> List.minBy (fun e -> distance (ship.x, ship.y) (e.x, e.y))
        if distanceE ship closestEnemy < fireDistance then
            Some closestEnemy
        else None


//todo: we don't want to fire bluntly, right?
// let positionToFire ship fireTarget =


(* game loop *)
while true do
    let myShipCount = int(Console.In.ReadLine()) (* the number of remaining ships *)
    let entityCount = int(Console.In.ReadLine()) (* the number of entities (e.g. ships, mines or cannonballs) *)
    
    let entities = [
        for i in 0 .. entityCount - 1 do
            let token = (Console.In.ReadLine()).Split [|' '|]
            yield Entity(
                int(token.[0]),
                token.[1],
                int(token.[2]),
                int(token.[3]),
                int(token.[4]),
                int(token.[5]),
                int(token.[6]),
                int(token.[7])
            ) 
    ]

    let barrels = entities |> List.filter(fun e -> e.entityType = "BARREL")
    let ships = entities |> List.filter(fun e -> e.entityType = "SHIP")
    let myShips, enemyShips = ships |> List.partition(fun ship -> ship.arg4 = 1)


    for i in 0 .. myShipCount - 1 do
        let ship = myShips.[i]

        
        match (didShipFired ship.id, fireTarget ship enemyShips) with
        | (false, Some target) ->
            setShipFired ship.id true
            printfn "FIRE %i %i" target.x target.y
        | _ ->
            setShipFired ship.id false
            match barrels with
            | [] ->
                printfn "MOVE %i %i" (rnd.Next(0, 23)) (rnd.Next(0, 21))
            | _ -> 
                let closestBarrel = barrels |> List.minBy (fun b -> distanceE ship b)
                printfn "MOVE %i %i" closestBarrel.x closestBarrel.y

        (* To debug: Console.Error.WriteLine("Debug message") *)
        ()

    ()
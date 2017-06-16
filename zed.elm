module Zed
    exposing
        ( Class(..)
        , Health
        , HitZone(..)
        , getDamageResistance
        , getHealth
        , getHitZoneDamageScale
        )

import Damage exposing (Class, Scale)
import Game exposing (Difficulty)


type Class
    = Clot
    | Scrake


type HitZone
    = Head
    | Body


type alias Health =
    Int


getHealth : Class -> Game.Difficulty -> Game.NumOfPlayers -> HitZone -> Health
getHealth class difficulty numOfPlayers hitZone =
    case class of
        Clot ->
            case difficulty of
                Game.Normal ->
                    case hitZone of
                        Head ->
                            15

                        Body ->
                            75

                Game.Hard ->
                    case hitZone of
                        Head ->
                            20

                        Body ->
                            100

                Game.Suicidal ->
                    case hitZone of
                        Head ->
                            20

                        Body ->
                            100

                Game.HellOnEarth ->
                    case hitZone of
                        Head ->
                            20

                        Body ->
                            100

        Scrake ->
            case difficulty of
                Game.Normal ->
                    case numOfPlayers of
                        1 ->
                            case hitZone of
                                Head ->
                                    510

                                Body ->
                                    935

                        2 ->
                            case hitZone of
                                Head ->
                                    652

                                Body ->
                                    1299

                        3 ->
                            case hitZone of
                                Head ->
                                    795

                                Body ->
                                    1664

                        4 ->
                            case hitZone of
                                Head ->
                                    938

                                Body ->
                                    2028

                        5 ->
                            case hitZone of
                                Head ->
                                    1081

                                Body ->
                                    2393

                        6 ->
                            case hitZone of
                                Head ->
                                    1224

                                Body ->
                                    2758

                        _ ->
                            0

                Game.Hard ->
                    case numOfPlayers of
                        1 ->
                            case hitZone of
                                Head ->
                                    600

                                Body ->
                                    1100

                        2 ->
                            case hitZone of
                                Head ->
                                    768

                                Body ->
                                    1529

                        3 ->
                            case hitZone of
                                Head ->
                                    936

                                Body ->
                                    1958

                        4 ->
                            case hitZone of
                                Head ->
                                    1104

                                Body ->
                                    2387

                        5 ->
                            case hitZone of
                                Head ->
                                    1272

                                Body ->
                                    2816

                        6 ->
                            case hitZone of
                                Head ->
                                    1440

                                Body ->
                                    3245

                        _ ->
                            0

                Game.Suicidal ->
                    case numOfPlayers of
                        1 ->
                            case hitZone of
                                Head ->
                                    630

                                Body ->
                                    1210

                        2 ->
                            case hitZone of
                                Head ->
                                    806

                                Body ->
                                    1681

                        3 ->
                            case hitZone of
                                Head ->
                                    982

                                Body ->
                                    2153

                        4 ->
                            case hitZone of
                                Head ->
                                    1159

                                Body ->
                                    2625

                        5 ->
                            case hitZone of
                                Head ->
                                    1335

                                Body ->
                                    3097

                        6 ->
                            case hitZone of
                                Head ->
                                    1512

                                Body ->
                                    3569

                        _ ->
                            0

                Game.HellOnEarth ->
                    case numOfPlayers of
                        1 ->
                            case hitZone of
                                Head ->
                                    660

                                Body ->
                                    1210

                        2 ->
                            case hitZone of
                                Head ->
                                    844

                                Body ->
                                    1681

                        3 ->
                            case hitZone of
                                Head ->
                                    1029

                                Body ->
                                    2153

                        4 ->
                            case hitZone of
                                Head ->
                                    1214

                                Body ->
                                    2625

                        5 ->
                            case hitZone of
                                Head ->
                                    1399

                                Body ->
                                    3097

                        6 ->
                            case hitZone of
                                Head ->
                                    1584

                                Body ->
                                    3569

                        _ ->
                            0


getHitZoneDamageScale : Class -> HitZone -> Damage.Scale
getHitZoneDamageScale class hitZone =
    case class of
        Clot ->
            case hitZone of
                Head ->
                    1.1

                Body ->
                    1.0

        Scrake ->
            case hitZone of
                Head ->
                    1.1

                Body ->
                    1.0


getDamageResistance : Class -> Damage.Class -> Damage.Scale
getDamageResistance class damageClass =
    case class of
        Clot ->
            case damageClass of
                Damage.BallisticHandgun ->
                    1.01

                Damage.BallisticSubmachinegun ->
                    1.5

                Damage.BallisticAssaultRifle ->
                    1.5

                Damage.BallisticRifle ->
                    1.0

                Damage.BallisticShotgun ->
                    1.0

                Damage.BallisticShell ->
                    1.0

                Damage.Slashing ->
                    0.85

                Damage.Piercing ->
                    1.0

                Damage.Bludgeon ->
                    0.9

                Damage.Microwave ->
                    0.25

                Damage.Explosive ->
                    1.0

                Damage.Fire ->
                    1.0

                Damage.Toxic ->
                    1.0

                Damage.Freeze ->
                    1.0

        Scrake ->
            case damageClass of
                Damage.BallisticHandgun ->
                    0.8

                Damage.BallisticSubmachinegun ->
                    1.0

                Damage.BallisticAssaultRifle ->
                    1.0

                Damage.BallisticRifle ->
                    1.0

                Damage.BallisticShotgun ->
                    0.9

                Damage.BallisticShell ->
                    1.0

                Damage.Slashing ->
                    1.0

                Damage.Piercing ->
                    0.75

                Damage.Bludgeon ->
                    0.9

                Damage.Microwave ->
                    1.0

                Damage.Explosive ->
                    0.4

                Damage.Fire ->
                    0.3

                Damage.Toxic ->
                    0.25

                Damage.Freeze ->
                    1.0

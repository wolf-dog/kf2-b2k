module Damage exposing (Class(..), Computed, Scale, Value)


type Class
    = BallisticHandgun
    | BallisticSubmachinegun
    | BallisticAssaultRifle
    | BallisticRifle
    | BallisticShotgun
    | BallisticShell
    | Slashing
    | Piercing
    | Bludgeon
    | Microwave
    | Explosive
    | Fire
    | Toxic
    | Freeze


type alias Value =
    Int


type alias Scale =
    Float


type alias Computed =
    Float

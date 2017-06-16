module Weapon exposing (Class(..), getDamage, getDamageClass)

import Damage exposing (Class, Value)


type Class
    = M14
    | Railgun
    | Scar


getDamage : Class -> Damage.Value
getDamage class =
    case class of
        M14 ->
            90

        Railgun ->
            750

        Scar ->
            55


getDamageClass : Class -> Damage.Class
getDamageClass class =
    case class of
        M14 ->
            Damage.BallisticRifle

        Railgun ->
            Damage.BallisticRifle

        Scar ->
            Damage.BallisticAssaultRifle

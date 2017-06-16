module Calculator exposing (calcBulletsToKill, calcDamage)

import Damage exposing (Scale)
import Player exposing (Player, castDamage, getDamageScale)
import Weapon exposing (Class, getDamage, getDamageClass)
import Zed exposing (Class, Health, HitZone, getDamageResistance, getHitZoneDamageScale)


calcDamage : Player -> Weapon.Class -> Zed.Class -> Zed.HitZone -> Damage.Value
calcDamage player weapon zed hitZone =
    Weapon.getDamage weapon
        |> toFloat
        |> (*) (Zed.getHitZoneDamageScale zed hitZone)
        |> floor
        |> toFloat
        |> (*) (Zed.getDamageResistance zed <| Weapon.getDamageClass weapon)
        |> floor
        |> toFloat
        |> (*) (Player.getDamageScale player weapon hitZone)
        |> Player.castDamage player.perk


calcBulletsToKill : Damage.Value -> Zed.Health -> Int
calcBulletsToKill damage health =
    (toFloat health / toFloat damage) |> ceiling

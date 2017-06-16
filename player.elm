module Player
    exposing
        ( Level
        , Perk(..)
        , Player
        , Skill(..)
        , castDamage
        , getDamageScale
        , getSkill1Name
        , levelFromStr
        , perkFromStr
        , perkToStr
        , skillFromStr
        , skillToStr
        )

import Damage exposing (Scale)
import Weapon exposing (Class)
import Zed exposing (HitZone)


type Perk
    = Commando
    | SharpShooter


perkFromStr : String -> Maybe Perk
perkFromStr perk =
    case perk of
        "commando" ->
            Just Commando

        "sharpshooter" ->
            Just SharpShooter

        _ ->
            Nothing


perkToStr : Perk -> String
perkToStr perk =
    case perk of
        Commando ->
            "commando"

        SharpShooter ->
            "sharpshooter"


type alias Level =
    Int


levelFromStr : String -> Maybe Level
levelFromStr level =
    let
        result =
            String.toInt level
    in
    case result of
        Ok int ->
            if int >= 1 && int <= 25 then
                Just int
            else
                Nothing

        Err msg ->
            Nothing


type Skill
    = NotApplied
    | Left
    | Right


type alias Player =
    { perk : Perk
    , level : Level
    , skill1 : Skill
    , skill2 : Skill
    , skill3 : Skill
    , skill4 : Skill
    , skill5 : Skill
    }


castDamage : Perk -> Damage.Computed -> Damage.Value
castDamage perk damage =
    case perk of
        Commando ->
            ceiling damage

        SharpShooter ->
            ceiling damage


getDamageScale : Player -> Weapon.Class -> Zed.HitZone -> Damage.Scale
getDamageScale player weapon hitZone =
    1.0
        + getGeneralDamageScale player weapon hitZone
        + getSkill1DamageScale player weapon hitZone
        + getSkill2DamageScale player weapon hitZone
        + getSkill3DamageScale player weapon hitZone
        + getSkill4DamageScale player weapon hitZone
        + getSkill5DamageScale player weapon hitZone


getGeneralDamageScale : Player -> Weapon.Class -> Zed.HitZone -> Damage.Scale
getGeneralDamageScale player weapon hitZone =
    case player.perk of
        Commando ->
            if isPerkWeapon player.perk weapon then
                0.01 * toFloat player.level
            else
                0.0

        SharpShooter ->
            case hitZone of
                Zed.Head ->
                    0.01 * toFloat player.level

                Zed.Body ->
                    0.0


getSkill1DamageScale : Player -> Weapon.Class -> Zed.HitZone -> Damage.Scale
getSkill1DamageScale player weapon hitzone =
    case player.perk of
        Commando ->
            0.0

        SharpShooter ->
            case player.skill1 of
                Left ->
                    0.25

                _ ->
                    0.0


getSkill2DamageScale : Player -> Weapon.Class -> Zed.HitZone -> Damage.Scale
getSkill2DamageScale player weapon hitzone =
    0.0


getSkill3DamageScale : Player -> Weapon.Class -> Zed.HitZone -> Damage.Scale
getSkill3DamageScale player weapon hitzone =
    0.0


getSkill4DamageScale : Player -> Weapon.Class -> Zed.HitZone -> Damage.Scale
getSkill4DamageScale player weapon hitzone =
    0.0


getSkill5DamageScale : Player -> Weapon.Class -> Zed.HitZone -> Damage.Scale
getSkill5DamageScale player weapon hitzone =
    0.0


skillFromStr : String -> Skill
skillFromStr skill =
    case skill of
        "left" ->
            Left

        "right" ->
            Right

        _ ->
            NotApplied


skillToStr : Skill -> String
skillToStr skill =
    case skill of
        Left ->
            "left"

        Right ->
            "right"

        _ ->
            "not_applied"


getSkill1Name : Perk -> Skill -> String
getSkill1Name perk skill =
    case perk of
        Commando ->
            case skill of
                Left ->
                    "???"

                Right ->
                    "???"

                _ ->
                    "Not Applied"

        SharpShooter ->
            case skill of
                Left ->
                    "Sniper"

                Right ->
                    "Marksman"

                _ ->
                    "Not Applied"


isPerkWeapon : Perk -> Weapon.Class -> Bool
isPerkWeapon perk weapon =
    case perk of
        Commando ->
            List.member weapon [ Weapon.Scar ]

        SharpShooter ->
            List.member weapon [ Weapon.M14, Weapon.Railgun ]

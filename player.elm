module Player
    exposing
        ( Level
        , Perk(..)
        , Player
        , Skill(..)
        , castDamage
        , getDamageScale
        , getSkillNames
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
    = NoSkill
      -- Commando
    | TacticalReloadCommando
    | HighCapacityMags
    | Fallback
    | ImpactRounds
    | Tenacious
    | Prepared
    | HollowPointRounds
    | EatLead
    | Tactician
    | MachineGunner
      -- SharpShooter
    | Sniper
    | Marksman
    | Stability
    | BallisticShock
    | RackEmUpFirst
    | RackEmUpMax
    | TacticalReloadSharpShooter
    | DeadEye
    | AlwaysPrepared
    | Assassin
    | Ranger


skillFromStr : String -> Skill
skillFromStr skill =
    case skill of
        "Tactical Reload (Commando)" ->
            TacticalReloadCommando

        "High Capacity Mags" ->
            HighCapacityMags

        "Fallback" ->
            Fallback

        "Impact Rounds" ->
            ImpactRounds

        "Tenacious" ->
            Tenacious

        "Prepared" ->
            Prepared

        "Hollow Point Rounds" ->
            HollowPointRounds

        "Eat Lead" ->
            EatLead

        "ZED TIME - Tactician" ->
            Tactician

        "ZED TIME - Machine Gunner" ->
            MachineGunner

        "Sniper" ->
            Sniper

        "Marksman" ->
            Marksman

        "Stability" ->
            Stability

        "Ballistic Shock" ->
            BallisticShock

        "Rack'em Up (First shot)" ->
            RackEmUpFirst

        "Rack'em Up (Max)" ->
            RackEmUpMax

        "Tactical Reload (SharpShooter)" ->
            TacticalReloadSharpShooter

        "DeadEye" ->
            DeadEye

        "Always Prepared" ->
            AlwaysPrepared

        "ZED TIME - Assassin" ->
            Assassin

        "ZED TIME - Ranger" ->
            Ranger

        _ ->
            NoSkill


skillToStr : Skill -> String
skillToStr skill =
    case skill of
        TacticalReloadCommando ->
            "Tactical Reload (Commando)"

        HighCapacityMags ->
            "High Capacity Mags"

        Fallback ->
            "Fallback"

        ImpactRounds ->
            "Impact Rounds"

        Tenacious ->
            "Tenacious"

        Prepared ->
            "Prepared"

        HollowPointRounds ->
            "Hollow Point Rounds"

        EatLead ->
            "Eat Lead"

        Tactician ->
            "ZED TIME - Tactician"

        MachineGunner ->
            "ZED TIME - Machine Gunner"

        Sniper ->
            "Sniper"

        Marksman ->
            "Marksman"

        Stability ->
            "Stability"

        BallisticShock ->
            "Ballistic Shock"

        RackEmUpFirst ->
            "Rack'em Up (First shot)"

        RackEmUpMax ->
            "Rack'em Up (Max)"

        TacticalReloadSharpShooter ->
            "Tactical Reload (SharpShooter)"

        DeadEye ->
            "DeadEye"

        AlwaysPrepared ->
            "Always Prepared"

        Assassin ->
            "ZED TIME - Assassin"

        Ranger ->
            "ZED TIME - Ranger"

        NoSkill ->
            "No Skill"


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
        + getSkillDamageScale player.skill1 weapon hitZone
        + getSkillDamageScale player.skill2 weapon hitZone
        + getSkillDamageScale player.skill3 weapon hitZone
        + getSkillDamageScale player.skill4 weapon hitZone
        + getSkillDamageScale player.skill5 weapon hitZone


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


getSkillDamageScale : Skill -> Weapon.Class -> Zed.HitZone -> Damage.Scale
getSkillDamageScale skill weapon hitzone =
    case skill of
        Fallback ->
            0.0

        HollowPointRounds ->
            0.3

        MachineGunner ->
            0.03

        Sniper ->
            0.25

        Stability ->
            0.3

        RackEmUpFirst ->
            0.1

        RackEmUpMax ->
            0.5

        DeadEye ->
            0.1

        Assassin ->
            0.35

        _ ->
            0.0


getSkillNames : Perk -> ( List String, List String, List String, List String, List String )
getSkillNames perk =
    case perk of
        Commando ->
            ( [ "Tactical Reload (Commando)", "High Capacity Mags" ]
            , [ "Fallback", "Impact Rounds" ]
            , [ "Tenacious", "Prepared" ]
            , [ "Hollow Point Rounds", "Eat Lead" ]
            , [ "ZED TIME - Tactician", "ZED TIME - Machine Gunner" ]
            )

        SharpShooter ->
            ( [ "Sniper", "Marksman" ]
            , [ "Stability", "Ballistic Shock" ]
            , [ "Rack'em Up (First shot)", "Rack'em Up (Max)", "Tactical Reload (SharpShooter)" ]
            , [ "DeadEye", "Always Prepared" ]
            , [ "ZED TIME - Assassin", "ZED TIME - Ranger" ]
            )


isPerkWeapon : Perk -> Weapon.Class -> Bool
isPerkWeapon perk weapon =
    case perk of
        Commando ->
            List.member weapon [ Weapon.Scar ]

        SharpShooter ->
            List.member weapon [ Weapon.M14, Weapon.Railgun ]

module Lib

open Microsoft.FSharp.Quotations.Patterns
open FSharpPlus

type D<'k,'v> = System.Collections.Generic.Dictionary<'k,'v>

let upFromBase_Floor (upPer:int) baseV =
    let p = float upPer / 100.0
    int (float baseV * (1.0 + p))

type StatCat = STR | DEX | LUK
let statCatAll = [STR; DEX; LUK]

type Prop
    = Stat of StatCat * int
    | StatPer of StatCat * int
    | FinStat of StatCat * int
    | Att of int
    | AttPer of int

    | IED of float
    | Boss of float
    | Dmg of float
    | CriDmg of float
    | AbsFinDmg of float

    | DynIED of float
    | DynBoss of int
    | MonDmg of int // always battle-time
    | DynDmg of int
    | DynCriDmg of int
    | DynAbsFinDmg of float

let zeroMap = statCatAll |>> (flip tuple2) 0 |> Map

type PropRecord = {
    Stats: Map<StatCat, int>
    StatPers: Map<StatCat, int>
    FinStats: Map<StatCat, int>
    Att: int
    AttPer: int

    IEDPer: float  // %
    Dmg: float
    Boss: float
    CriDmg: float
    AbsFinDmgPer: float  // %, include base 1.0

    DynIEDPer: float  // %
    DynDmg: float
    DynBoss: float
    DynCriDmg: float
    DynAbsFinDmgPer: float  // %, include base 1.0
}
with
    member this.AccumStats =
        Map.mapValues3 (fun b p f -> upFromBase_Floor p b + f) this.Stats this.StatPers this.FinStats
    member this.AccumAtt =
        upFromBase_Floor this.AttPer this.Att

    override this.ToString() =
        let s = 
            sprintf "%+A" this
            
        let s = s.[..(s.Length - 2)]
        sprintf "%s\n  AccumStats = %A\n  AccumAtt = %d\n}" s this.AccumStats this.AccumAtt


    static member Zero = 
        {
            Stats = zeroMap
            StatPers = zeroMap
            FinStats = zeroMap
            Att = 0
            AttPer = 0

            IEDPer = 0
            Dmg = 0
            Boss = 0
            CriDmg = 0
            AbsFinDmgPer = 1

            DynIEDPer = 0
            DynDmg = 0
            DynBoss = 0
            DynCriDmg = 0
            DynAbsFinDmgPer = 1
        }

let combineIEDs (ied1: float) (ied2: float) =
    assert(abs ied1 <= 1.)
    assert(abs ied2 <= 1.)

    let _left (f: float) = 
        if f > 0 then
            1. - f
        else
            1. / (1. - abs f)

    1.0 - (_left ied1) * (_left ied2)


let combinePropRecords (p1: PropRecord) (p2: PropRecord) =
    {
        Stats = Map.unionWith (+) p1.Stats p2.Stats
        StatPers = Map.unionWith (+) p1.StatPers p2.StatPers
        FinStats = Map.unionWith (+) p1.FinStats p2.FinStats
        Att = p1.Att + p2.Att
        AttPer = p1.AttPer + p2.AttPer
        IEDPer = combineIEDs p1.IEDPer p2.IEDPer
        Dmg = p1.Dmg + p2.Dmg
        Boss = p1.Boss + p2.Boss
        CriDmg = p1.CriDmg + p2.CriDmg
        AbsFinDmgPer = p1.AbsFinDmgPer * p2.AbsFinDmgPer

        DynIEDPer = combineIEDs p1.DynIEDPer p2.DynIEDPer
        DynDmg = p1.DynDmg + p2.DynDmg
        DynBoss = p1.DynBoss + p2.DynBoss
        DynCriDmg = p1.DynCriDmg + p2.DynCriDmg
        DynAbsFinDmgPer = p1.DynAbsFinDmgPer * p2.DynAbsFinDmgPer
    }

let accumulateProps (props: Prop list) =
    let Stats = D<StatCat, int>(zeroMap)
    let StatPers = D<StatCat, int>(zeroMap)
    let FinStats = D<StatCat, int>(zeroMap)
    let mutable Att: int = 0
    let mutable AttPer: int = 0

    let mutable IEDPer: float = 0 // %
    let mutable Dmg: float = 0
    let mutable Boss: float = 0
    let mutable CriDmg: float = 0
    let mutable AbsFinDmgPer: float = 1// %, include base 1.0

    let mutable DynIEDPer: float = 0 // %
    let mutable DynDmg: int = 0
    let mutable DynBoss: int = 0
    let mutable DynCriDmg: int = 0
    let mutable DynAbsFinDmgPer: float = 1// %, include base 1.0
    
    for prop in props do
        match prop with
        | Stat(cat, v) -> Stats[cat] <- Stats[cat] + v
        | StatPer(cat, v) -> StatPers[cat] <- StatPers[cat] + v
        | FinStat(cat, v) -> FinStats[cat] <- FinStats[cat] + v

        | Att v -> Att <- Att + v
        | AttPer v -> AttPer <- AttPer + v

        | IED f -> IEDPer <- combineIEDs IEDPer <| f / 100.
        | Boss v  -> Boss <- Boss + v
        | Dmg v  -> Dmg <- Dmg + v
        | CriDmg v  -> CriDmg <- CriDmg + v
        | AbsFinDmg f -> AbsFinDmgPer <- AbsFinDmgPer * (f / 100. + 1.)

        | DynIED f -> DynIEDPer <- combineIEDs DynIEDPer <| f / 100.
        | DynBoss v  -> DynBoss <- DynBoss + v
        | DynDmg v | MonDmg v -> DynDmg <- DynDmg + v
        | DynCriDmg v  -> DynCriDmg <- DynCriDmg + v
        | DynAbsFinDmg f -> DynAbsFinDmgPer <- DynAbsFinDmgPer * (f / 100. + 1.)



    let _d2m d = d |> Seq.map (|KeyValue|) |> Map 
        
    {
        Stats = _d2m Stats
        StatPers = _d2m StatPers
        FinStats = _d2m FinStats
        Att = Att
        AttPer = AttPer

        IEDPer = IEDPer
        Dmg = Dmg
        Boss = Boss
        CriDmg = CriDmg
        AbsFinDmgPer = AbsFinDmgPer

        DynIEDPer = DynIEDPer
        DynDmg = DynDmg
        DynBoss = DynBoss
        DynCriDmg = DynCriDmg
        DynAbsFinDmgPer = DynAbsFinDmgPer
    }

    

let str n = Stat (STR, n)
let dex n = Stat (DEX, n)
let luk n = Stat (LUK, n)
let pstr n = StatPer (STR, n)
let pdex n = StatPer (DEX, n)
let pluk n = StatPer (LUK, n)
let fstr n = FinStat (STR, n)
let fdex n = FinStat (DEX, n)
let fluk n = FinStat (LUK, n)
let all n = [str n; dex n; luk n]
let pall n = [pstr n; pdex n; pluk n]
let fall n = [fstr n; fdex n; fluk n]


let calcMapleBlessing (up: int) (chara:Prop list) =
    let groupByStatCat (props: Prop list )=
        statCatAll
        |> List.map (fun statCat ->
            let nums = 
                List.choose 
                    (fun p ->
                        match p with
                        | Stat(cat, n) when cat = statCat -> Some n
                        | _ -> None
                    )
                    props
            statCat, nums
        )
    groupByStatCat chara
    |> List.map (fun (cat, nums) ->
        let total = List.sum nums
        Stat(cat, upFromBase_Floor up total - total)
    )



type ScrollForce
    = Red
    | X
    | V
    | B_Armor  // 9 ATT, 2 ALL
    | B_Acc  // 9 ATT
    | HighGollux
    | SF_Any of Prop list

type EquipSet
    = PitchSet
    | DawnSet
    | TopGolluxSet
    | ArcshadeSet
    | EternalSet
    | BrilliantSet
    | LuckyGenesis
    | LuckyPurpleWingsEarring

// All set effects start from set number 2
let _equipSetProps = dict[
    PitchSet, [
        all 10 @ [Att 10; Boss 10] // 2
        all 10 @ [Att 10; IED 10] // 3
        all 15 @ [Att 15; CriDmg 5] // 4
        all 15 @ [Att 15; Boss 10] // 5
        all 15 @ [Att 15; IED 10] // 6
        all 15 @ [Att 15; CriDmg 5] // 7
        all 15 @ [Att 15; Boss 10] // 8
        all 15 @ [Att 15; CriDmg 5] // 9
    ]
    DawnSet, [
        all 10 @ [Att 10; Boss 10]
        all 10 @ [Att 10]
        all 10 @ [Att 10; Boss 10]
    ]
    TopGolluxSet, [
        all 20
        [Att 35]
        [Boss 30; IED 30]
    ]
    ArcshadeSet, [
        [Att 30; Boss 10]
        [Att 30; IED 10]
        all 50 @ [Att 35; Boss 10]
        [Att 40; Boss 10]
        [Att 30]
        [Att 30; IED 10]
    ]
    EternalSet, [
        [Att 40; Boss 10]
        [Att 40; Boss 10] @ all 50
        [Att 40; Boss 10]
        [Att 40; IED 20]
        [Att 40; Boss 15]
        all 50 @ [Att 40; Boss 15]
        [Att 40; Boss 15]
    ]
    BrilliantSet, [
        all 20 @ [Att 20; Boss 15]
    ]
]


type Equip
    = Equip of
        Name:string * Lv:int * Star:int 
        * Base:Prop list 
        * Flame: Prop list
        * Scroll: ((*Many:*)int * (*ScrollType:*)ScrollForce) list 
        * White: int  // -1 for non-white-able
        * MainPotential: Prop list
        * BonusPotential: Prop list
        * EquipSet:EquipSet option

let getEquipName (Equip(Name=n)) = n

type EquipSlot =
    SlotRing


let scrollforce (scroll:ScrollForce) : Prop list =
    match scroll with
    | Red -> [Att 5]
    | X -> [Att 7]
    | V -> [Att 8]
    | B_Armor -> [Att 9] @ all 2
    | B_Acc -> [Att 9]
    | HighGollux -> [Att 4] @ all 3
    | SF_Any props -> props



let starforce (Equip(name, lv, stars, _,_,scrolls,_,_,_,_):Equip) : Prop list =
    if stars = 0 then
        []
    elif name.StartsWith("Shield") then
        [dex 75; luk 75; Att 45]
    else

    let stat_lower n =
        if name.StartsWith("Hat") || name.StartsWith("Glove") || name.StartsWith("Shoe") || name.StartsWith("Shoulder") // job-specifc armor
        then
            [dex n; luk n]
        else
            all n
    let stat_higher n = 
        if name.StartsWith("Hat") || name.StartsWith("Glove") || name.StartsWith("Shoe") // job-specifc armor
        then
            let n_str = 
                scrolls |> List.map snd |> List.map scrollforce
                |> List.concat
                |> List.choose (fun p ->
                    match p with
                    | Stat(STR, n) -> Some 1
                    | _ -> None
                ) |> List.length
            if n_str > 0 then
                all n
            else
                [dex n; luk n]
        else
            all n

    assert (stars >= 20)
    if name.StartsWith("Glove")
    then
        match lv with
        | 200 ->
            let sf20 =
                [
                    stat_lower (2*5) @[ Att 1 ] // 1-5
                    stat_lower (10*3) @[  Att 6] // 6-15
                    stat_higher (15*5) @ [ Att (12+13+14+15+16) ] // 16-20
                ] |> List.concat
            let s15 = stat_higher 15
            let above20tbl = [
                s15 @ [Att 17]; // 21
                s15 @ [Att 19]; // 22
                [Att 21]; // 23
                [Att 23];
                [Att 25]
            ]
            let above20 = 
                above20tbl 
                |> List.take (stars - 20)
                |> List.concat
            sf20 @ above20
        | 250 ->
            let sf20 =
                [
                    stat_lower (2*5) @[ Att 1 ] // 1-5
                    stat_lower (10*3) @[  Att 6] // 6-15
                    stat_higher (17*5) @ [ Att (14+15+16+17+18) ] // 16-20
                ] |> List.concat
            let s17 = stat_higher 17
            let above20tbl = [
                s17 @ [Att 19]; // 21
                s17 @ [Att 21]; // 22
                [Att 23]; // 23
                [Att 25];
                [Att 27]
            ]
            let above20 = 
                above20tbl 
                |> List.take (stars - 20)
                |> List.concat
            sf20 @ above20

    else
        let (sf20, above20tbl) =
            match lv with
            | 140 -> 
                stat_lower (5*2) @ stat_lower (10*3)
                @ stat_higher (5*9) @ [Att (8+9+10+11+12)],
                [
                    stat_higher 9 @ [Att 13]
                    stat_higher 9 @ [Att 15]
                    [Att 17]
                    [Att 19]
                    [Att 21]
                ]
            | 150 ->
                stat_lower (5*2)@ stat_lower (10*3)
                @ stat_higher (5*11)@ [Att (9+10+11+12+13)],
                [
                    stat_higher 11 @ [Att 14]
                    stat_higher 11 @ [Att 16]
                    [Att 18]
                    [Att 20]
                    [Att 22]
                    // unknown 25-26 stars
                    [Att 23]
                    [Att 24]
                ]
            | 160 | 170 ->
                stat_lower (5*2)@ stat_lower (10*3)
                @ stat_higher (5*13)@ [Att (10+11+12+13+14)],
                [
                    stat_higher 13 @ [Att 15]
                    stat_higher 13 @ [Att 17]
                    [Att 19]
                    [Att 21]
                    [Att 23]
                    // unknown 25-26 stars
                    [Att 24]
                    [Att 25]
                ]
            | 200 ->
                stat_lower (5*2)@ stat_lower(10*3)
                @ stat_higher (5*15)@ [Att (12+13+14+15+16)],
                [
                    stat_higher 15 @ [Att 17]
                    stat_higher 15 @ [Att 19]
                    [Att 21]
                    [Att 23]
                    [Att 25]
                    // unknown 25-26 stars
                    [Att 26]
                    [Att 27]
                ]
            | 250 ->
                stat_lower (5*2) @ stat_lower (10*3)
                @ stat_higher (5*17) @ [Att (14+15+16+17+18)],
                [
                    stat_higher 17 @ [Att 19]
                    stat_higher 17 @ [Att 21]
                    [Att 23]
                    [Att 25]
                    [Att 27]
                    // unknown 25-26 stars
                    [Att 28]
                    [Att 29]
                ]

        let above20 = 
            above20tbl 
            |> List.take (stars - 20)
            |> List.concat
        sf20 @ above20


let calcEquips (equips: Equip list) : Prop list =
    let calcEquip (Equip(name, lv, stars, baseProps, flame, scrolls, white, main, bonus, equipSet) as equip: Equip) : Prop list =
        let starprops = starforce equip
        let scrollprops = 
            scrolls
            |> List.map (fun (n, scroll) -> List.replicate n (scrollforce scroll) |> List.concat)
            |> List.concat
        baseProps @ flame @ scrollprops @ starprops @ main @ bonus
    let selfProps = equips |> List.map calcEquip |> List.concat

    let maybeLuckySets (Equip(EquipSet = s)) =
        match s with
        | Some(LuckyGenesis) -> None // Some [ArcshadeSet]
        | Some(LuckyPurpleWingsEarring) -> Some [TopGolluxSet; PitchSet]
        | _ -> None
    let maybeSet (Equip(EquipSet = s)) = 
        match s with
        | Some(LuckyGenesis) -> Some EternalSet
        | Some(LuckyPurpleWingsEarring) -> None
        | Some _ -> s
        | _ -> None
    let optionalLucky =
        let triggers = equips |> List.choose maybeLuckySets
        assert (triggers.Length <= 1)
        let luckys = triggers |> List.concat
        luckys

    let setProps =
        equips |> List.choose maybeSet |> List.countBy id
        |> List.map (fun (k, n) -> 
            _equipSetProps.[k] 
               |> List.take (
                if List.contains k optionalLucky && n >= 3 then
                    n
                else
                   n -  1
               )
            |> List.concat
        )
        |> List.concat

    selfProps @ setProps


let aut (lv:int) : Prop =
    fluk <| 300 + lv * 200

let equipEzRing =
    Equip(
        "__EzRing__", 110, 0,
        all 4 @ [Att 4],
        [],
        [], -1,
        [], [],
        None
    )

type SkillId
    = Assassinate
    | AssassinateVI
    | PulverizeVI
    | OriginSkill
    | MoneyBomb
    | MoneyBombVI
    | Trickblade
    | Sonicblow
    | V4
    | ShadowVeil  // no effect, only to trigger 12s darksight
    | Darksight of Duration:float
    | VDarksight
    | Smokescreen
    | VMapleBlessing
    | LegendaryAdventure
    | ReadyToDie
    | AngelBlaster
    | Wugong

    | ROR of Lv:int
    | RingL of Lv:int * WeaponAtt:int
    | RingCont of Lv:int

    // Not really skills, but actions:
    | Idle of Second:float
    | TimelineMark of Timing:float
    | ChangeEquip of EquipSlot * NewEquip:Equip

let getShortSkillName (s:SkillId) : string =
    match s with
    | Assassinate 
    | AssassinateVI -> "暗杀"
    | PulverizeVI -> "粉碎"
    | OriginSkill -> "起源"
    | MoneyBomb -> "钱炸"
    | MoneyBombVI -> "钱炸VI"
    | Trickblade -> "切开"
    | Sonicblow -> "音速"
    | V4 -> "灭鬼"
    | ShadowVeil -> "雾杀"
    | Darksight dur -> $"隐身{dur}s"
    | VDarksight -> "终隐"
    | Smokescreen -> "烟雾弹"
    | VMapleBlessing -> "大枫祝"
    | LegendaryAdventure -> "传说冒险"
    | ReadyToDie -> "必死"
    | AngelBlaster -> "天破"
    | Wugong -> "武公"
    | ROR lv -> $"规范{lv}"
    | RingL(Lv, WeaponAtt) -> $"泡泡{Lv}"
    | RingCont(lv) -> $"Cont{lv}"
    | Idle snd -> $"暂停{snd}s"
    | TimelineMark timing -> $"时刻{timing}s"
    | ChangeEquip (slot, newEquip) -> $"切换"
    
let _attackSkillIds = [
    //Assassinate
    AssassinateVI
    PulverizeVI
    OriginSkill
    MoneyBomb
    MoneyBombVI
    Trickblade
    Sonicblow
    V4
]

let castLatency (s:SkillId) : float =
    match s with
    | Assassinate
    | AssassinateVI
    | PulverizeVI
        -> 0.56 + 0.28
    | OriginSkill -> 5.

    | MoneyBomb -> 0
    | MoneyBombVI -> 0
    | Trickblade -> 1.0
    | Sonicblow -> 3.0
    | V4 -> 0
    | ShadowVeil -> 1.0
    | Darksight _ -> 0
    | VDarksight -> 0
    | Smokescreen -> 0.5
    | VMapleBlessing -> 0.5
    | LegendaryAdventure -> 0.5 
    | ReadyToDie -> 0.5
    | AngelBlaster -> 0.5
    | Wugong -> 1
    | ROR _ -> 0
    | RingL _ -> 0
    | RingCont _ -> 0

    | Idle snd -> snd
    | ChangeEquip _ -> 5.0
    | TimelineMark _ -> 0.



type EvalConfig = {
    HexaLevels: Map<SkillId, int>
}
with
    static member NonVI = {
        HexaLevels = Map[
            AssassinateVI, 0
            MoneyBombVI, 0
            OriginSkill, 0
            Trickblade, 0
            Sonicblow, 0
            V4, 0
        ]
    }

    member x.AssassinateVI_Level = x.HexaLevels[AssassinateVI]
    member x.MoneyBombVI_Level = x.HexaLevels[MoneyBombVI]
    member x.OriginSkill_Level = x.HexaLevels[OriginSkill]
    member x.VSkillEnhance_V2 = x.HexaLevels[Trickblade]
    member x.VSkillEnhance_V3 = x.HexaLevels[Sonicblow]
    member x.VSkillEnhance_V4 = x.HexaLevels[V4]

let attackBaseDmg (ec:EvalConfig) (s:SkillId) : (float * int) list =
    match s with
    | Assassinate ->
        let lines =
            [(273., 6)]
            @
            [(495. * 1.5, 6)]
            |>> arrFirst ((+) 100.)
        (lines |>> arrFirst ((*) 0.7)) @ lines

    | AssassinateVI ->
        let lines = 
            [
                float (275 + ec.AssassinateVI_Level * 4), 6
                1.5 * float (510 + ec.AssassinateVI_Level * 6), 6
            ] |>> arrFirst ((+) 100.)
        (lines |>> arrFirst ((*) 0.7)) @ lines

    | PulverizeVI ->
        let lines = [
            float (300 + ec.AssassinateVI_Level * 6), 6
            1.5 * float (595+ ec.AssassinateVI_Level * 8), 6
        ]
        (lines |>> arrFirst ((*) 0.7)) @ lines

    | OriginSkill ->
        let lines = [
            float (3150 + ec.OriginSkill_Level * 105), 5 * 6
            float (3600 + ec.OriginSkill_Level * 120), 5 * 6
            float (4200 + ec.OriginSkill_Level * 140), 8 * 7
        ]
        (lines |>> arrFirst ((*) 0.7)) @ lines

    | MoneyBomb ->
        // one invocation for a turn (2 times) of assassinate
        // with average multiplier of 11 (the number of coins)
        [100., 2 * 11]

    | MoneyBombVI ->
        [150. + 5. * float ec.MoneyBombVI_Level, 2 * 11]

    | Trickblade ->
        let d = 1485.
        [d, 7*5; d * 0.7, 7*5]
    | Sonicblow ->
        // if not interrupted, attack 15 times in 2.5s, each time with 7 damages
        let d = 1100.
        [d, 7*15; d * 0.7, 7*15]
    | V4 ->
        [935., 12*8; 1375., 4*15]

    | _ ->
        []


(*
Level 30: Shadow Assault’s final damage is increased by 60%. The number of times you can use this skill is increased to 6. The cooldown is decreased to 50 seconds.
Level 30: Eviscerate’s final damage is increased by 60%.
Level 30: Sonic Blow’s final damage is increased by 60%.
Level 30: Demon Slashing Shadow Formation’s final damage is increased by 60%.
每级增加1%。但是，1级11%，10级25%，20级40%，30级60%。

All Skill Cores (Origin Skills) get bonus stats when they reach level 10/20/30.

Level 10: 20% defense ignore
Level 20: 20% boss damage
Level 30: 30% defense ignore and boss damage
*)

// as attack bonus, it's a one-time bonus associated with invocation of certain skills
let attackBonus (ec:EvalConfig) (s:SkillId) : Prop list =
    let hexa_VbonusFinDmg lv = 
        if lv = 0 then 0 else
            [10; 25; 40; 60].[lv / 10] + lv % 10
        |> float |> DynAbsFinDmg

    let moneyBombBase = 
        [
            // job 3, greedy
            DynDmg 20

            // job 4
            DynBoss 30

            // super skill
            DynDmg 20

            // V core
            DynIED 20
            DynAbsFinDmg 180.
        ]

    match s with
    | Assassinate | AssassinateVI | PulverizeVI -> 
        [
            // super skill
            DynDmg 20
            DynBoss 20
            //NonTimelineIED 10

            // V core
            DynIED 20
            DynAbsFinDmg 120.
        ]
    | OriginSkill ->
        [
            if ec.OriginSkill_Level >= 10 then
                DynIED 20
            if ec.OriginSkill_Level >= 20 then
                DynBoss 20
            if ec.OriginSkill_Level >= 30 then
                DynIED 30
                DynBoss 30
        ]
    | MoneyBomb | MoneyBombVI ->
        moneyBombBase
        @ [
            DynBoss (ec.MoneyBombVI_Level / 3)
        ]

    | Trickblade -> 
        [
            DynIED 100
            hexa_VbonusFinDmg ec.VSkillEnhance_V2
        ]
    | Sonicblow -> 
        [
            DynIED 100
            hexa_VbonusFinDmg ec.VSkillEnhance_V3
        ]
    | V4 -> 
        [
            DynIED 100
            hexa_VbonusFinDmg ec.VSkillEnhance_V4
        ]
    | _ -> []

let _rng = System.Random(2345)
let randoms = [ for _ in 0..1000 do _rng.Next(12) ]
let mutable _mutCont = 0
let _mutSampleContEffect () =
    let c = _mutCont
    _mutCont <- _mutCont + 1
    if c < 8 then
        [DynBoss 140; AttPer 10]
    else
        []



// as buff, it's lasting and universal prop bonus
// for stat STR/DEX/LUK it's always %-able Props and need to rebase.
let buffEffect (mapleBlessing:Prop list) (s:SkillId) : float * Prop list =
    match s with
    | Assassinate 
    | AssassinateVI
    | PulverizeVI
    | OriginSkill
        -> 0, []
    | MoneyBomb -> 0, []
    | MoneyBombVI -> 0, []
    | Trickblade -> 0, []
    | Sonicblow -> 0, []
    | V4 -> 0, []
    | ShadowVeil -> 0, []

    | Darksight dur -> float dur, [AbsFinDmg 15.]
    | VDarksight -> 30.0, [AbsFinDmg(15.+14.)]
    | Smokescreen -> 30.0, [DynCriDmg 22]
    | VMapleBlessing -> 
        60.0,
            (calcMapleBlessing 400 mapleBlessing)
            @ [Dmg 20]
    | LegendaryAdventure -> 60.0, [Dmg 10]
    | ReadyToDie -> 15.0, [AbsFinDmg 36.]
    | AngelBlaster -> 17.0, [Dmg 45]
    | Wugong -> 60.0, [AttPer 100]
    | ROR lv -> float (7 + lv * 2), [AttPer (25*lv)]
    | RingL(lv, weapon) -> float (7 + lv * 2), [luk (weapon*lv)]
    | RingCont(lv) -> float (4 + lv), [Boss (35. * float lv); AttPer (2 + lv * 2)]

    | Idle _
    | ChangeEquip _
    | TimelineMark _
        -> 0, []

    | Cont_Absolute -> 30.0, _mutSampleContEffect()
    | Cont_Percentage -> 999.0, _mutSampleContEffect()

type HexaStatCat
    = HexaCriDmg
    | HexaBoss
    | HexaIED
    | HexaDmg
    | HexaAtt
    | HexaStat


let _hexa4Kinds (perStage: Prop list list) : Prop list list =
    let (s1::s2::s3::s4::[]) = perStage
    List.replicate 4 s1 @ List.replicate 3 s2 @ List.replicate 2 s3 @ [s4]

let _hexaStatMains = Map[
    HexaCriDmg, _hexa4Kinds (List.map (CriDmg >> List.singleton) [0.35; 0.7; 1.05; 1.4])
    HexaBoss, _hexa4Kinds (List.map (Boss  >> List.singleton) [1.; 2.; 3.; 4.])

    // summed as one line, even for IED
    HexaIED, _hexa4Kinds (List.map (IED  >> List.singleton) [1.; 2.; 3.; 4.])

    HexaDmg, _hexa4Kinds (List.map (Dmg  >> List.singleton) [0.75; 1.5; 2.25; 3.])
    HexaAtt, _hexa4Kinds (List.map (Att  >> List.singleton) [5; 10; 15; 20])
    HexaStat, _hexa4Kinds [[fluk 100] @ fall 48; [fluk 200] @ fall 96; [fluk 300] @ fall 144; [fluk 400] @ fall 192]
]

let _hexaStatSides = Map[
    HexaCriDmg, List.replicate 10 [CriDmg 0.35]
    HexaBoss, List.replicate 10 [Boss 1.]
    HexaIED, List.replicate 10 [IED 1.]
    HexaDmg, List.replicate 10 [Dmg 0.75]
    HexaAtt, List.replicate 10 [Att 5]
    HexaStat, List.replicate 10 ([fluk 100] @ fall 48)
]

let hexaStat (main: HexaStatCat * int) (side1: HexaStatCat * int) (side2: HexaStatCat * int) : (Prop list * string) =
    let sumAll (m: Map<HexaStatCat, Prop list list>) ((cat, lv): HexaStatCat * int) =
        let props = m.[cat] |> List.take lv
        match props with
        | [(IED _)]::_ ->
            props |> List.concat
            |>> (fun (IED ied) -> ied)
            |> sum |> IED |> List.singleton
        | _ -> List.concat props

    let title (cat, lv) = sprintf "%A-%d " cat lv

    sumAll _hexaStatMains main @ sumAll _hexaStatSides side1 @ sumAll _hexaStatSides side2,
        title main + title side1 + title side2


let calcAttackDmg (defRatio: int) (R: PropRecord) (skillLineDmg: float) =
    let weaponRatio = 1.30
    let mastery = 0.91

    let accumStats = R.AccumStats
    let statContrib = (accumStats[LUK]) * 4 + (accumStats[DEX]) + (accumStats[STR])

    let statAtt = weaponRatio * float statContrib * float R.AccumAtt / 100. * ((1. + mastery) / 2.)

    let ied = combineIEDs R.IEDPer R.DynIEDPer
    let throughDef = max 0. (1. - float defRatio / 100. * (1. - ied))
    let criratio = float (R.CriDmg + R.DynCriDmg + (*ave*)35. + 100.) / 100.
    let dmgRatio = float (R.Dmg + R.DynDmg + R.Boss + R.DynBoss) / 100. + 1.
    let finRatio = R.AbsFinDmgPer * R.DynAbsFinDmgPer

    statAtt * (skillLineDmg / 100.) * dmgRatio * finRatio * throughDef * criratio
        / 2. // boss's stat defense, propotional



let evalDmgSeq (defRatio: int)
        (ec: EvalConfig)
        (mapleBlessing: Prop list) 
        (equips: Equip list) (staticProps: Prop list) 
        (skills: SkillId list) =

    let maybeRingContIdxAct =
        List.choosei (fun i s ->
            match s with
            | RingCont lv -> Some(i, s)
            | _ -> None
        ) skills
        |> List.tryHead

    let useRingCont, ringContStartIdx, ringContDur, ringContBuffs, skills =
        match maybeRingContIdxAct with
        | Some(i, s) ->
            let contDur, contBuffs = buffEffect mapleBlessing s
            true, i, contDur, contBuffs, List.deleteAt i skills
        | _ ->
            false, skills.Length, 0, [], skills
    let ringContBuffRecord = accumulateProps ringContBuffs

    let mutable _ringContStartTick = 9999999.


    let staticR = accumulateProps staticProps
    let mutable _equipR = calcEquips equips |> accumulateProps
    let mutable _buffR = PropRecord.Zero

    let initAccDmg = _attackSkillIds |>> (flip tuple2) 0. |> Map
    let initBuff = Map<SkillId, float>[]
    let (endAccDmg, endBuff, endTick) =
        List.scan 
            (fun (accDmg, (buffLeft:Map<SkillId, float>), tick) (actIdx, skillId) ->
                let castTime = castLatency skillId
                let attBaseDmg = attackBaseDmg ec skillId

                match skillId with
                | ChangeEquip(SlotRing, newEquip) ->
                    let newEquips =
                        equips
                        |>> (fun (Equip(Name=n) as p) -> 
                            if n = getEquipName equipEzRing then
                                newEquip
                            else
                                p
                        )
                    _equipR <- calcEquips newEquips |> accumulateProps
                | _ -> ()

                let buffDur, _ = buffEffect mapleBlessing skillId

                let inheritedBuffN = buffLeft.Count
                let timeDecreBuffs = 
                    Map.toSeq buffLeft
                    |> Seq.map (fun (k, left) -> k, (max 0. (left - castTime)))
                    |> Seq.where (snd >> (fun left -> left > 0.))
                    |> Map
                let buffDisappear = inheritedBuffN <> timeDecreBuffs.Count

                let buffLeft2, buffAdd =
                    if buffDur > 0. then
                        // really contains a buff, not a purely attack skill
                        timeDecreBuffs.Add(skillId, buffDur)
                        , true

                    else
                        timeDecreBuffs
                        , false

                if buffDisappear || buffAdd then

                    let _priorityBuff (lefts: SkillId seq) =
                        let m = Set lefts
                        let hasVdarkside = Set.contains VDarksight m

                        Seq.where (fun s ->
                            match s with
                            | Darksight _ -> not hasVdarkside
                            | _ -> true
                        ) lefts

                    let buffProps: Prop list =
                        buffLeft2
                        |> Seq.map (fun p -> p.Key)
                        |> _priorityBuff
                        |> Seq.map (buffEffect mapleBlessing >> snd)
                        |> Seq.concat
                        |> Seq.toList

                    _buffR <- accumulateProps buffProps

                let btimeR = _buffR |> combinePropRecords staticR |> combinePropRecords _equipR


                if actIdx = ringContStartIdx then
                    _ringContStartTick <- tick

                let ringContActivateTimes = int(tick - _ringContStartTick) / 12
                let ringContActivateTickStart = (float ringContActivateTimes) * 12. + _ringContStartTick
                let ringContActivateTickEnd = ringContActivateTickStart + ringContDur
                let castEndTick = tick + castTime

                let btimeR =
                    if useRingCont && tick >= ringContActivateTickStart && castEndTick < ringContActivateTickEnd then
                        btimeR |> combinePropRecords ringContBuffRecord
                    else
                        btimeR


                let accDmg2 =
                    if attBaseDmg.Length > 0 then

                        // included stats must be separatedly rebased
                        let attBonusProps = attackBonus ec skillId

                        let attR = accumulateProps attBonusProps
                        let totalR = combinePropRecords btimeR attR

                        let dmgLines =
                            attBaseDmg
                            |>> (fun (lineDmg, mult) ->
                                (calcAttackDmg defRatio totalR lineDmg) / 100000000. // in 1E
                                    , float mult
                            )

                        //printfn ">>> %A, %f %A\n%O" skillId tick dmgLines totalR

                        let totalDmg = 
                            (dmgLines |>> uncurry (*) |> sum)
                            / 10000. // in Zhao

                        let accDmg2 = Map.change skillId <| Option.map ((+) totalDmg) <| accDmg

                        accDmg2

                    else
                        //printfn ">>> %A, %f\n%O" skillId tick btimeR
                        accDmg



                accDmg2, buffLeft2, tick + castTime
            ) 
            (initAccDmg, initBuff, 0.)
            (List.indexed skills)
        |> List.last

    endAccDmg

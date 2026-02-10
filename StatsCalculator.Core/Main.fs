module Main

open Lib
open BattleScenarios
open HexaSolver
open FSharpPlus

let equipsWithRingPlaceholder: Equip list = [
    Equip(
        "TotemLoop", 0, 0,
        [str 41; dex 35; luk 37; Att 15], [], [], -1, [], [], None
    )
    Equip(
        "TotemStone", 200, 0,
        all 20 @ [Att 5],
        [str 12; luk 6; Att 1] @ pall 5,
        [], -1, [], [], None
    )
    Equip(
        "TotemWumu", 101, 0,
        all 20 @ [Att 10],
        [], [], -1, [], [], None
    )
    Equip(
        "RingGollux", 150, 22,
        all 10 @ [Att 8],
        [],
        [9, HighGollux], 2,
        [pluk (9+12+12)],
        [pluk 4; Att 10],
        Some TopGolluxSet
    )
    Equip(
        "RingHeaven", 150, 22,
        all 25 @ [Att 25],
        [],
        [4, V], 2,
        [pluk (12+12)] @ pall 6,
        [pluk (4+2)],
        None
    )

    Equip("RingET", 200, 22, 
        all 5 @ [Att 4], [],
        [2+1+2, V], 2,
        pall 7 @ [pluk 23],
        [pluk (5+3); Att 11],
        Some PitchSet
    )

    equipEzRing

    Equip(
        "PacketBook", 160, 0,
        all 10 @ [luk 10; Att 10],
        [dex 15; luk 30] @ pall 3,
        [], -1,
        [],
        [],
        Some PitchSet
    )
    Equip(
        "NecklaceGollux", 150, 22,
        all 28 @ [Att 5],
        [luk 32],
        [9, HighGollux], 2,
        [pluk (9+12+12)],
        [pluk (4+2)],
        Some TopGolluxSet
    )
    Equip(
        "NecklaceRootOfPain", 160, 22,
        all 10 @ [Att 3],
        [luk 27; Att 2] @ pall 3,
        [8, V], 2,
        pall 10 @ [pluk (10+13)],
        [Att 12] @ all 4, // TODO
        Some PitchSet
    )
    Equip(
        "WeaponGenesis", 200, 0,
        [str 33; dex 313; luk 363; Att 752] @ [Boss 30; IED 20],
        [], [], -1,
        [Boss 40; AttPer 13; Boss 40], // @ [pdex 7; att 95]
        [AttPer (13+10); pdex 10],
        Some LuckyGenesis
    )
    Equip(
        "BeltGollux", 150, 22,
        all 60 @ [Att 35],
        [dex 8; luk 32],
        [6, V], 2,
        [pluk (9+12+12)],
        [pluk (4+2)],
        Some TopGolluxSet
    )
    //Equip(
    //    "HatArcshade", 200, 22,
    //    [dex 65; luk 65; Att 7; IED 15],
    //    [dex 23; luk 45] @ pall 2,
    //    [1, B_Armor; 13, V], 2,
    //    [],
    //    [str 18],
    //    Some ArcshadeSet
    //)
    Equip("HatEternal", 250, 22,
        [dex 65; luk 65; Att 7; IED 15.],
        pall 2 @ [luk 78; str 21; dex 21],
        [13, V; 1, B_Armor], 3,
        [], [pluk (7)],
        Some EternalSet
    )

    Equip(
        "FaceAcc", 160, 22,
        all 10 @ [Att 10],
        [dex 20; luk 42] @ pall 2,
        [8, V], 2,
        pall 10 @ [pluk (13+13)],
        [pluk (5+3); luk 11],
        Some PitchSet
    )
    Equip(
        "EyeAcc", 160, 22,
        all 15 @ [Att 3],
        [luk 37; Att 2] @ pall 3,
        [6, V], 2,
        [pluk(13+10+13)],
        [Att 12; pluk (3+3)],
        Some PitchSet
    )
    //Equip(
    //    "OverallDragonSlayer", 170, 22,
    //    all 100 @ [Att 200; Boss 30; IED 30],
    //    [dex 15; luk 42],
    //    [12, V], -1,
    //    [pluk (10+13+13)],
    //    [pluk (5+3)],
    //    None
    //)
    //Equip(
    //    "_SlotBottom", 0, 0, [], [], [], -1, [], [], None)

    Equip("EternalTop", 250, 22, 
            [Att 6; dex 50; luk 50; IED 5],
            pall 3 @ [luk 64; str 21; dex 7],
            [9, V; 1, B_Armor], 2,
            [pluk 33],
            [luk (287/9*2); pluk 7; pdex 7],
            Some EternalSet)
    Equip("EternalBottom", 250, 22, 
            [Att 6; dex 50; luk 50; IED 5],
            pall 3 @ [luk 57; str 14; dex 35],
            [9, V; 1, B_Armor], 2,
            [pluk 26] @ pall 10,
            [pluk (9+7); luk (288/9)],
            Some EternalSet)


    //Equip(
    //    "ShoeArcshade", 200, 22,
    //    [dex 40; luk 40; Att 9],
    //    [dex 34; luk 57],
    //    [8, X; 1, V], 1,
    //    [pluk (10+13+13)],
    //    [pluk 6; pluk 5],
    //    Some ArcshadeSet
    //)
    Equip("ShoeEternal", 250, 22,
        [dex 55; luk 55; Att 12],
        pall 3 @ [luk 57],
        [10, V; 1, B_Armor], 3,
        [pluk 33],
        [pluk <| 6 + 5],
        Some EternalSet
    )

    Equip(
        "EarringGollux", 150, 22,
        all 15 @ [Att 10],
        [luk 24; Att 2] @ pall 2,
        [10, HighGollux], 2,
        [pluk (9+12+12)],
        [pluk 4; pdex 2; Att 10],
        Some TopGolluxSet
    )
    Equip("ShoulderEternal", 250, 22,
        all 51 @ [Att 28],
        [],
        [3, V], 2,
        [pluk 23] @ pall 10,
        [pluk (7)] @ pall 6,
        Some EternalSet
    )
    //Equip(
    //    "GloveArcshade", 200, 22,
    //    [dex 40; luk 40; Att 9],
    //    [str 12; luk 45] @ pall 3,
    //    [9, V; 1, B_Armor], 2,
    //    [CriDmg 8; pluk 13; CriDmg 8],
    //    [luk (285 / 9 * 2); str 18] @ pall 6,
    //    Some ArcshadeSet
    //)
    Equip("GloveEternal", 250, 22,
        [dex 55; luk 55; Att 12],
        pall 3 @ [luk 57],
        [10, V; 1, B_Armor], 3,
        [pluk 10; CriDmg 16],
        [pluk <| 6 + 5],
        Some EternalSet
    )

    Equip("EmblemMitra", 200, 0,
        [dex 40; luk 40; Att 5],
        [],
        [], 0,
        [AttPer(13+13+10);],
        [AttPer(13+13); str 32],
        Some PitchSet
    )

    Equip(
        "Badge", 10, 0,
        [str 9; dex 11; luk 10; Att 10],
        [], [],  -1,
        [], [], 
        None
    )
    Equip(
        "MedalReboot", 0, 0,
        all 21 @ [Att 21],
        [], [], -1,
        [], [],
        None
    )
    Equip(
        "ShieldDarkness", 130, 20,
        [luk 10],
        [],
        [10, V], 2,
        [Boss (35.+40.); AttPer 9],
        [AttPer (12+9); pstr 9],
        None
    )
    Equip(
        "CapeWing", 150, 22,
        all 25 @ [Att 8; Att 20],
        [luk 8] @ pall 3,
        [8, Red; 2, SF_Any [luk 10]], 2,
        [pluk (9+12)] @ pall 9,
        pall 2 @ [Att 10; pluk 2],
        None
    )

    //Equip(
    //    "Heart", 150, 22,
    //    all 5,
    //    [],
    //    [1, SF_Any [str 139; dex 141; luk 139; Att 169]], -1,
    //    [pluk (9+12)] @ pall 9,
    //    pall 2 @ [pluk (2+2)],
    //    None
    //)
    Equip(
        "ExtremeSuuHeart", 200, 22,
        all 25 @ [Att 15],
        [],
        [1, SF_Any [str 175; dex 183; luk 181; Att 180]], -1,
        [pluk (10+13)] @ pall 10,
        pall 3 @ [pluk (3+3)],
        Some PitchSet
    )

]


let battlefield_occupy_training = [
    str 5
    dex 25
    luk 75
    Att 10
    MonDmg 40 // normal monster
    CriDmg 20
]

let battlefield_occupy_bossing = [
    str 5
    dex 10
    luk 75
    Att 15
    IED 40
    Boss 40
    CriDmg 20
]

let battlefield_member_training = [
    fstr <| 80 * 8
    fdex <| 80 * 5
    fluk <| 100
    fluk <| 80 * 5
    fstr 40; fdex 40; fluk 40
    DynDmg(int (0.2 * 16.))
    Att 35
    IED 5
    IED 5
    Boss <| 5. + 5.
    CriDmg <| 5. + 5. + 5.
]

let battlefield_member_bossing = [
    fstr <| 80 * 8
    fdex <| 80 * 5
    fluk <| 100
    fluk <| 80 * 5
    fstr 40; fdex 40; fluk 40
    DynDmg(int (0.2 * 16.))
    Att 35
    IED 5
    IED 5
    Boss <| 5. + 5.
    CriDmg <| 5. + 5. + 5.
]

let extreme_abilities_training = [
    luk 150
    CriDmg 11
    Dmg 39
    Att 9
    MonDmg 51 // normal monster
]

let extreme_abilities_bossing = [
    luk 180   
    CriDmg 12
    IED 42
    Dmg 36
    Boss 47
    Att 15
]

let link_skills_training = 
    [
        DynDmg (18/2) // thief x3

        Dmg 5; MonDmg 11  // lala
        Dmg 5 // kali
    ]
    @   pall 10 
    @ [
        IED 10; MonDmg 14 // hoyoung
        CriDmg 4
        Dmg 10
        DynDmg 11 // ark
        Dmg 10
        DynDmg <| 6+6
        DynDmg 12
    ]

let link_skills_bossing = 
    [
        //CriDmg 4
        IED 15
        Dmg 10
        Boss 15
        Boss 4; DynBoss 12 // moxuan
        DynDmg 11 // ark
        Dmg 10
        DynDmg <| 6+6
        DynDmg 9; DynIED 9
        Boss 7
        DynDmg 12
    ]

let pet_equips_bossing = [ 
    Att (80*3)
    Boss 20
]

let pet_equips_training = pet_equips_bossing

let chara = [
    str 4; dex 4
    luk 1448
]

let mapleBlessing = calcMapleBlessing 16 chara


// contains bastStat, stat%, Att, Att%, BOSS/IED/DMG...
let _staticProps = 
    chara
    @ mapleBlessing
    @ [AbsFinDmg 99.5]

    @ [AbsFinDmg 20.] // lv +5


    @ let puzzle = [
        luk 2; luk 3; luk 2; Att  1
        dex 1; luk 2; luk 2; luk 3
        luk 2; Att 1; dex 1; luk 2; luk 2
        luk 3; luk 2; Att 1; luk 7; Att 1;
    
        // set
        luk 15; luk 25; Att 10
    ] in puzzle

    @ let skills = 
        [
            // job 0
            Att 30 // empress blessing
            str 5; dex 5; luk 5; Att 5 // alliance will
            // final 10<per> //genesis
        
            // event
            Boss 30
            IED 30
        ]
        @   all 20
        @[
            Att 10
            IED 12
            CriDmg 10

        
        
            // job 1
            luk 20
        
            // job 2
            CriDmg 5
            // Att 30 // steal posion
            luk 20 // fast dagger
            Att 30 // karma
            luk 30; dex 30 // training

            // ####
            // Only with sheild
            Att 15 // shield master
            // ####
        
            // job 3
            // shadow partner 70% FinDmg
            Att 25 // greedy
        
            // job 4
            // cold stub 25% FinDmg
            luk 10; CriDmg 20 // bloody packet
            // maple blessing
            Att 52; IED 21 // instrict

            // ####
            // Only with weapn
            Att 41; CriDmg 15 // advanced dagger
            // ####

            // SuperSkill
            Dmg 25 // coin
        ]
        
            // job V 
            @ all 27 // connect lv27  AUTO
            @ all 3 // decent fire eye lv13  AUTO
        @ [
            CriDmg 8 // decent fire eye
            Att 20 // decent blessing
            Att 30 // flush lv30  AUTO
            Att 30 // ready to die lv30 AUTO

            // assassinate
            // ied 20
        ] in skills

    @ let title = all 20 @ [
        //all 10; att 15; boss 5<per>     // snowfield undefeatable
        Att 10; Boss 10    // yeti & pkb
    ] in title
    @ let moe_monster_set = all 10 in moe_monster_set
    @ let inner_abilities = [
        Boss 10
    ] in inner_abilities
    @ let guild_skill = [
        str 40; dex 40; luk 40
        Att 15
        MonDmg 12
    ] in guild_skill
    
    @ let leadership = [ IED 10 ] in leadership
    

    // finals
    @ let arc = [fluk <| 2200 * 6] in arc
    @ let aut = [
        aut 11
        aut 11
        aut 11
        aut 10
        aut 10
        aut 10
    ] in aut

    @ [
        // battlefield red card
        Att 30

        // guild buffs
        Att 30
        Dmg 30
        Boss 30
        CriDmg 30
    ]

    @ [
        // legion artifact
        Dmg 15
        Boss 15
        CriDmg 4
        IED 20
        Att 30
        
    ] @ all 150

    // NOTE comment this block before solving best HEXA stat config
    @ let hexaStat = [
        CriDmg 1.4
        //Boss 0
        fluk 300
        //Dmg 0
    ] in hexaStat


    //@ [
    //    // hero
    //    Att 30
    //    CriDmg 10

    //    // bishop
    //    AbsFinDmg 10.
    //    IED 44.

    //    // hayato
    //    Dmg 20

    //    // LZL
    //    Dmg 40
    //]
    @ [
        //Boss 140
        //AttPer 10
    ]

    // Lv FD: +5=20 +4=18 +3=16 +2=14 +1=12 +0=10 -1=5 -2=0 -3=-5 -4=-10 -5=-12.5
    @ [ DynAbsFinDmg 20. ]   // kalosP2 280lv, I'm +5lv == 20%
    // AUT FD
    @ [ DynAbsFinDmg 25. ]
    

let isBossing = true

let staticProps = 
    _staticProps
    @ if isBossing then pet_equips_bossing else pet_equips_training
    @ if isBossing then link_skills_bossing else link_skills_training
    @ if isBossing then battlefield_occupy_bossing else battlefield_occupy_training
    @ if isBossing 
        then battlefield_member_bossing @ extreme_abilities_bossing
        else battlefield_member_training @ extreme_abilities_training
    

let _staticPanelR = 
    let equips = equipsWithRingPlaceholder
    combinePropRecords
    <| accumulateProps staticProps
    <| accumulateProps (calcEquips equips)
//in printfn "Static panel:\n%O\n\n" _staticPanelR

let _configSets (sets: ((Prop list) list) list) =
    let sets = 
        if sets.Length > 0 then
            sets
        else
            [] :: sets

    // Prop list := One parallelized alternative of a set
    // Prop list list := lifted elements (initially [x])
    // Prop list list list := a elem-lifted set
    //      allPair ELset1 ELset2 -> (Prop list list * Prop list list) list
    // Prop list list list list := elem-lifted sets
    let setElemLift (x: Prop list) = [x]
    let elemLifted = List.map (List.cons [] >> List.map setElemLift) sets

    // : Prop list list             list
    //  (change)   (cross-set comb) (sets cartesian)
    List.reduceBack (fun elemLiftedSet1 elemLiftedSet2 -> 
        List.allPairs elemLiftedSet1 elemLiftedSet2 
        |>> (uncurry List.append)
    ) elemLifted

let _upgradeEquips (equips:Equip list) (sets: (string * Equip) list list) : (Equip list) list =
    let sets = 
        if sets.Length = 0 then
            []
        elif sets.[0] = [] then
            sets
        else
            [] :: sets

    sets
    |>> (fun updates ->
        List.fold (fun eqs (name, newE) -> 
            let i = List.findIndex (getEquipName >> ((=) name)) eqs
            List.setAt i newE eqs
        ) equips updates
    )

type DiffRecord = {
    Title: string
    DEF: int
    Up: float
    DumpLines: string list
}

let defRatio = 380
let ecVI: EvalConfig = {
    HexaLevels = Map[
        AssassinateVI, 30
        MoneyBombVI, 30
        OriginSkill, 30
        Trickblade, 30
        Sonicblow, 30
        V4, 30
    ]
}

open Subphases

module ZH =
    let ror = "规"
    let ringL = "泡" 
    let shadowVeil = "雾杀"
    let vDarkside = "终隐"
    let originSkill = "起源"
    let cont = "永"

let NamedPhases = 
    let rorOrL = [
        //for rorLv in [3; 4] do
        for ring1, ring2, ringTitle in [ 
            //[ROR(rorLv)], [ringL], $"{ZH.ror}{rorLv}_"
            //[ringL], [ror], ZH.ringL

            [ROR(4)], [], "ror4"
            [ringL4], [], "ringl4"
        ] do
            for dark1, dark1M, dark2, dark2M, darksideTitle in [ 
                [ VDarksight; Smokescreen ], [], [ ShadowVeil ], [], ""//$"2{ZH.shadowVeil}"
                //[ Smokescreen ], [ShadowVeil], [VDarksight], [ShadowVeil], $"2{ZH.vDarkside}"
            ] do
                yield
                    let mainSubphases = [ 
                        p1Blast dark1 ring1 [OriginSkill]
                        phase1MidBlast dark1M; phase2Blast dark2 ring2; phase2MidBlast dark2M
                        phase180sEnd ring1 
                    ] in
                    let mainSubphases2 = [
                        p1Blast dark1 ring1 []
                        phase1MidBlast dark1M; phase2Blast dark2 ring2; phase2MidBlast dark2M
                        phase180sEnd ring1 
                    ]
                    in $"{ringTitle}{darksideTitle}360s{ZH.originSkill}", mainSubphases, buildPhase mainSubphases @ buildPhase mainSubphases2

            //yield 
            //    let mainSubphases = [ p1Blast [VDarksight;Smokescreen] ring1 [OriginSkill]; [TimelineMark 30.] ]
            //    in $"{ringTitle}30s{ZH.originSkill}", mainSubphases, buildPhase mainSubphases
    ]
    
    let cont = [
        for lv in [4] do
        for ring1, origin, firstContNote in [
            [RingCont(lv)], [OriginSkill], "@V3";
            [], [RingCont(lv); OriginSkill], "@OS"
        ] do
        let dark1 = [VDarksight; Smokescreen] in
        let dark1M = [] in
        let dark2 = [ShadowVeil] in 
        let dark2M = []
        let mainSubphases = [ 
            p1Blast dark1 ring1 origin
            phase1MidBlast dark1M; phase2Blast dark2 []; phase2MidBlast dark2M
            phase180sEnd ring1 
        ] in
        let mainSubphases2 = [
            p1Blast dark1 ring1 []
            phase1MidBlast dark1M; phase2Blast dark2 []; phase2MidBlast dark2M
            phase180sEnd ring1 
        ]
        let totalPhaseWoCont = buildPhase mainSubphases @ buildPhase mainSubphases2

        $"{ZH.cont}{lv}{firstContNote}_360s{ZH.originSkill}", mainSubphases, totalPhaseWoCont
    ]

    rorOrL @ cont

let calculatePropUps () =
    NamedPhases
    |>> (fun (_phaseName, _mainSubphases, _phase) ->
        printfn "\n\n%s\n" _phaseName

        let initDmg = 
            evalDmgSeq defRatio ecVI mapleBlessing equipsWithRingPlaceholder staticProps _phase
        let initTotalDmg = initDmg |> Map.values |> sum

        let propKinds: (string * Prop list) list = [
             let mag = 1 in $"luk{mag}", [luk mag] 
             let mag = 1 in $"pluk{mag}%%", [pluk mag] 
             let mag = 1 in $"pall{mag}%%", pall mag 
             let mag = 1 in $"Att{mag}", [Att mag] 
             let mag = 1 in $"Att{mag}%%", [AttPer mag] 
             let mag = 10. in $"IED{mag}%%", [IED mag] 
             let mag = 20. in $"IED{mag}%%", [IED mag] 
             let mag = 30. in $"IED{mag}%%", [IED mag] 
             let mag = 1. in $"Dmg{mag}%%", [Dmg mag] 
             let mag = 1. in $"CriDmg{mag}%%", [CriDmg mag] 
             let mag = 1. in $"FinDmg{mag}%%", [AbsFinDmg mag] 

             let mag = 1 in $"fluk{mag}", [fluk mag] 
             let mag = 1 in $"dex{mag}", [dex mag]
        ]

        let x =
            propKinds
            |>> (fun (title, propKind) ->
                let props = propKind @ staticProps
                let dmg = 
                    evalDmgSeq defRatio ecVI mapleBlessing equipsWithRingPlaceholder props _phase

                let totalDmg = dmg|> Map.values |> sum
                let totalPer = totalDmg / initTotalDmg * 100. - 100.

                (title, totalPer)
            )
        in let pers = x

        //pers
        //    |>> (fun (title, totalPer) ->
        //        let desc = sprintf "%A \t[%0.2f%% up]" title totalPer
        //        desc
        //    )
        //    |> String.concat "\n"
        //    |> printfn "%s"
        //printfn "\n"

        _phaseName, pers
    )


            


let solveHexaPlan () =
    for (name, a, phases) in NamedPhases do
        printfn "\n\n%s\n" name
        HexaSolver.solveHexaPlan
            //{ ecVI  with HexaLevels = ecVI.HexaLevels |> Map.map (fun k v -> 30) }
            ecVI
            250
            defRatio mapleBlessing equipsWithRingPlaceholder staticProps phases
    


let deltaProps = _configSets [
        //// emblem
        //[
        //    [IED -30.; AttPer 9]
        //    [IED -30.; AttPer 12]
        //    //[IED -30.; IED 35.]
        //    //[IED -30.; IED 40.]
        //] 

        //// shield
        //[
        //    [AttPer 12; Boss 75]
        //    //[AttPer 12; Boss 80]
        //] |>> ((@) [Boss -75; AttPer -9])

        //[
        //    //[pluk -13; CriDmg 8]
        //    [luk -(285/9); ] @ pall -6 @ [CriDmg 3 ; pluk 6]
        //]

        [
            [Att (54*4)]
        ]


        //// TEMPLATE
        //[
        //]
    ]


// The basic equips is a parameter, and let UI to decide the init ring (e.g. ez ring)
let getNewEquipLists equips = 

    //let hatArcshareToEternal =
    //    "HatArcshade", Equip("HatEternal", 250, 22,
    //        [dex 65; luk 65; Att 7; IED 15.],
    //        pall 2 @ [luk 78; str 21; dex 21],
    //        [13, V; 1, B_Armor], 3,
    //        [], [pluk (7)],
    //        Some EternalSet
    //    )

    //let shoeArcsharetoExternal =
    //    "ShoeArcshade", Equip("ShoeEternal", 250, 22,
    //        [dex 55; luk 55; Att 12],
    //        pall 3 @ [luk 57],
    //        [10, V; 1, B_Armor], 3,
    //        [pluk 33],
    //        [pluk <| 6 + 5],
    //        Some EternalSet
    //    )

    //let gloveArcsharetoExternal =
    //    "GloveArcshade", Equip("GloveEternal", 250, 22,
    //        [dex 55; luk 55; Att 12],
    //        pall 3 @ [luk 57],
    //        [10, V; 1, B_Armor], 3,
    //        [pluk 10; CriDmg 16],
    //        [pluk <| 6 + 5],
    //        Some EternalSet
    //    )

    let noEq = Equip(
        "NONE", 0, 0,
        [],
        [],
        [], 0,
        [],
        [],
        None
    )

    let earringPurpleWings = Equip(
        "EarringPurpleWings", 140, 20,
        all 4 @ [Att 1],
        [luk 8] @ pall 3,
        [9, V], 2,
        [pluk (9+12+12)],
        [pluk 4; pdex 2; Att 10],
        Some LuckyPurpleWingsEarring
    )



    //let _baseLimboRing = 
    //    Equip("LimboRing", 250, 0,
    //        all 10 @ [Att 5],
    // scroll 3
    //        Some BrilliantSet

    //let _baseBaldrixPendant = 
    //    Equip("BaldrixPendant", 250, 0,
    //        all 15 @ [Att 5],
    // scroll 6
    //        Some BrilliantSet

    _upgradeEquips equips [
        [
            //"Badge", Equip("Badge7Days", 100, 0,
            //    all 7 @ [Att 7; IED 27],
            //    [], [], [], [], None
            //)
            //"MedalReboot", Equip("Medal7Days", 100, 0,
            //    all 7 @ [Att 7],
            //    [], [], [], [], None
            //)
        ]

        //[
        //    "RingGollux", Equip(
        //        "LimboRing", 250, 22,
        //        all 10 @ [Att 5],
        //        [],
        //        [7, B_Acc], 3,
        //        [pluk (9+12+12)],
        //        [pluk (9+7)],
        //        Some BrilliantSet
        //    )
        //    "NecklaceGollux", Equip(
        //        "BaldrixPendant", 250, 22,
        //        all 15 @ [Att 5],
        //        pall 3 @ [luk 57],
        //        [, B_Acc], 3,
        //        [pluk (9+12+12)],
        //        [pluk (9+7)],
        //        Some BrilliantSet
        //    )
        //    "EarringGollux", Equip(
        //        "EarringPurpleWings", 140, 20,
        //        all 4 @ [Att 1],
        //        [luk 8] @ pall 3,
        //        [9, V], 2,
        //        [pluk (9+12+12)],
        //        [pluk 4; pdex 2; Att 10],
        //        Some LuckyPurpleWingsEarring
        //    )
        //]


        // Gollux Belt/Ear to 26, vs, new LucidBelt/PitchEar 26
        let gollux_case_star = 22
        [
            "BeltGollux", Equip(
                $"BeltGollux{gollux_case_star}", 150, gollux_case_star,
                all 60 @ [Att 35],
                [dex 8; luk 32],
                [6, V], 2,
                [pluk (9+12+12)],
                [pluk (6+5)],
                Some TopGolluxSet
            )
            "EarringGollux", Equip(
                $"EarringGollux{gollux_case_star}", 150, gollux_case_star,
                all 15 @ [Att 10],
                [luk 24; Att 2] @ pall 2,
                [10, V], 2,
                [pluk (9+12+12)],
                [pluk (6+5)],
                Some TopGolluxSet
            )
        ]
        [
            //"RingGollux", noEq
            //"NecklaceGollux", noEq
            "BeltGollux", Equip(
                $"LucidBelt{gollux_case_star}", 200, gollux_case_star,
                all 50 @ [Att 6],
                [luk 51] @ pall 3,
                [7, V], 3,
                [pluk (9+12+12)],
                [pluk (8+6)],
                Some PitchSet
            )

            "EarringGollux", Equip(
                $"EarringPitch{gollux_case_star}", 200, gollux_case_star,
                all 7 @ [Att 5],
                [luk 51] @ pall 3,
                [10, V], 3,
                [pluk (9+12+12)],
                [pluk (8+6)],
                Some PitchSet
            )
        ]
        // if ear is purple-wings
        [
            "BeltGollux", Equip(
                $"BeltGollux{gollux_case_star}", 150, gollux_case_star,
                all 60 @ [Att 35],
                [dex 8; luk 32],
                [6, V], 2,
                [pluk (9+12+12)],
                [pluk (4+2)],
                Some TopGolluxSet
            )
            "EarringGollux", earringPurpleWings
        ]



        //[
        //    "Emblem", Equip("Mitra", 200, 0,
        //        [dex 40; luk 40; Att 5],
        //        [],
        //        [], 0,
        //        [AttPer(13+13+10)],
        //        [AttPer(13+10)],
        //        Some PitchSet
        //    )
        //    //"Badge", Equip(
        //    //    "BadgeGenesis", 200, 0,
        //    //    all 15 @ [Att 10],
        //    //    [], [],  0,
        //    //    [], [], 
        //    //    Some PitchSet
        //    //)
        //    "Heart", Equip(
        //        "HeartExtreme45", 200, 22,
        //        all 25 @ [Att 15; IED 30],
        //        [],
        //        [12, V], 2,
        //        [pluk 36],
        //        [pluk 8],
        //        Some PitchSet
        //    )

        //]


        //[
        //    "CapeWing", Equip(
        //        "CapeArcshade", 200, 22,
        //        all 35 @ [Att 6] @ [AttPer (-5)],
        //        [luk 45] @ pall 3,
        //        [10, V], 2,
        //        [pluk 36],
        //        [pluk (5+2)],
        //        Some ArcshadeSet
        //    )
        //]
    ]

let getEquipListDelta prev next =
    let deltaEqs = 
        List.map2 (fun f t ->
            f <> t, (f, t)
        ) prev next
        |> List.where (fst >> (=) true)
        |>> snd
    deltaEqs


type EquipPreset = {
    Title: string

    Dmg: Map<SkillId, float>
}

type BattleAnalysisRecord = {
    Phase: string

    EquipPresets: EquipPreset list  // .[0] is for init preset
}


let compareEquipUpgradeCandidates () : BattleAnalysisRecord list =
    // including the original Equips list
    let newEquipsLists = getNewEquipLists equipsWithRingPlaceholder
    NamedPhases
    |>> (fun (name, a, phases) ->

        let x =
            newEquipsLists
            |>> (fun equips ->
                let dmg = 
                    evalDmgSeq defRatio ecVI mapleBlessing equips staticProps phases
                let equipsDelta = getEquipListDelta equipsWithRingPlaceholder equips
                let deltaTitle = 
                    equipsDelta
                    |>> (fun (Equip(Name=n1) as e1, Equip(Name=n2) as e2) -> $"{n1} => {n2}")
                    |> String.concat ",  "
                    |> sprintf "[%s]"
                (deltaTitle, (equipsDelta, dmg))
            )

        in let configs = x

        {
            Phase = name
            EquipPresets = 
                configs
                |>> (fun (title, (delta, dmg)) -> 
                    {
                        Title = title
                        Dmg = dmg
                    } : EquipPreset
                )
        } :BattleAnalysisRecord

    )


            


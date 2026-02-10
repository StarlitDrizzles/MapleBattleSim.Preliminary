module BattleScenarios

open Lib
open FSharpPlus


let v3cd = 34.3
let ringCd = 161.2
let ringContCd = 8

let trickbladeCd = 10.8
let tb_castTime = castLatency Trickblade

let amam = [
    Assassinate
    Assassinate
    MoneyBomb
]
let amam_castTime = List.map castLatency amam |> List.sum


// Pulverize doesn't start charging when there left charge counts.
type VIzeState = {
    Timing: float
    LastSmokeScreenTiming: float
    LastPulverizeTiming: float
}
let VIzeAssassinate (acts: SkillId list) =
    let random = new System.Random()
    let pulverize = [Darksight (castLatency PulverizeVI); PulverizeVI]
    List.mapFold (fun state act -> 
        let mutable lastSS = state.LastSmokeScreenTiming
        let mutable lastPulverize = state.LastPulverizeTiming

        let newActs =
            match act with
            | Assassinate ->
                if state.Timing - state.LastSmokeScreenTiming < 30. ||
                   state.Timing - state.LastPulverizeTiming >= 5.6
                then
                    lastPulverize <- state.Timing
                    pulverize
                else
                    if random.NextDouble() <= 0.1 then
                        lastPulverize <- -999999.
                    [AssassinateVI]
                
            | Smokescreen ->
                lastSS <- state.Timing
                lastPulverize <- -999999.
                [Smokescreen]

            | ShadowVeil -> 
                lastPulverize <- -999999.
                [ShadowVeil]
            | _ -> [act]
        
        (newActs, {
            Timing = state.Timing + castLatency act
            LastSmokeScreenTiming = lastSS
            LastPulverizeTiming = lastPulverize
        })
    ) {
        Timing = 0.
        LastSmokeScreenTiming = 99999999999.
        LastPulverizeTiming = 0.
    } acts
    |> fst
    |> List.concat

    |> (fun r ->
        let c = 
            r |> List.countBy (fun act ->
                match act with
                | AssassinateVI -> "ass"
                | PulverizeVI -> "pul"
                | _ -> "no"
            )
        printf "%A" c

        r
    )



type BuildState = {
    PreEnd: float

    // As long as Tickblade is ready, cast it.
    LastTickbladeTiming: float
}


let buildPhase (mainSubphases: SkillId list list) =
    mainSubphases
    |>> (fun subphase ->
        let (Some markedTiming), preMarkDur, postMarkDur =
            subphase
            |> List.fold (fun (markTiming:float option, preDur:float, postDur:float) skillId ->
                match skillId with
                | TimelineMark timing ->
                    Some timing, preDur, postDur
                | skillId ->

                let castDur = castLatency skillId
                if markTiming.IsNone then
                    markTiming, preDur + castDur, postDur
                else
                    markTiming, preDur, postDur + castDur

            ) (None, 0., 0.)
        
        (markedTiming - preMarkDur), (markedTiming + postMarkDur), subphase
    )
    // Fill-in all empty time blocks interleavely, but don't assume the final time -- just reply on the last block to specify
    |> List.fold (fun (builtPhase, state) thisSubphase ->
        let (thisS, thisE, thisActs) = thisSubphase
        // Build interleaving blocks between preEnd and thisS

        let mutable _curTime = state.PreEnd
        let mutable _lastTB = state.LastTickbladeTiming
        let mutable _leftTime = thisS - state.PreEnd
        let _blockActs = System.Collections.Generic.List<SkillId>()

        while _leftTime > 0 do
            if _curTime - _lastTB > trickbladeCd && _leftTime > tb_castTime then
                _blockActs.AddRange([Trickblade; MoneyBomb])
                _lastTB <- _curTime
                _curTime <- _curTime + tb_castTime
                _leftTime <- _leftTime - tb_castTime

            elif _leftTime > amam_castTime then
                _blockActs.AddRange(amam)
                _curTime <- _curTime + amam_castTime
                _leftTime <- _leftTime - amam_castTime

            else
                _blockActs.Add(Idle _leftTime)
                _leftTime <- 0.


        let blockActs = List.ofSeq _blockActs in
        let latestPhase = builtPhase @ blockActs @ thisActs in
        latestPhase, { LastTickbladeTiming=_lastTB; PreEnd=thisE }
        
    ) ([],  { 
        LastTickbladeTiming = -42.; PreEnd = 0.
    })
    |> fst
    
    |> List.collect (fun s ->
        if s = ShadowVeil then
            [ShadowVeil; Darksight(12.)]
        else
            [s]
    )
    |> VIzeAssassinate
    |> List.replace [MoneyBomb] [MoneyBombVI]


module Subphases =
    let ringL4 = RingL(4, 752)

    let p1Blast (darksideBuffs: SkillId seq) (p1Rings: SkillId seq) (originSkills: SkillId seq) = [
        TimelineMark 0
        LegendaryAdventure
        VMapleBlessing
        Wugong

        yield! darksideBuffs

        ReadyToDie
        AngelBlaster

        yield! p1Rings

        V4
        Sonicblow
        MoneyBomb

        Trickblade
        MoneyBomb

        yield! originSkills
    ]

    let phase2Blast  (darksideBuffs: SkillId seq)  (p2Rings: SkillId seq) = [
        if Seq.length p2Rings > 0 then
            ChangeEquip(SlotRing, equipEzRing)
        TimelineMark (ringCd / 2.)

        yield! darksideBuffs

        ReadyToDie
        AngelBlaster

        yield! p2Rings

        V4
        Sonicblow
        MoneyBomb
    ]

    
    let phase1MidBlast  (darksideBuffs: SkillId seq)  = [ 
        TimelineMark v3cd 

        yield! darksideBuffs

        Sonicblow
        MoneyBomb
    ]

    let phase2MidBlast  (darksideBuffs: SkillId seq)  = [ 
        TimelineMark (ringCd / 2. + v3cd)

        yield! darksideBuffs

        Sonicblow
        MoneyBomb
    ]

    let phase180sEnd (p1Rings: SkillId seq) = [
        if Seq.length p1Rings > 0 then
            ChangeEquip(SlotRing, equipEzRing)
        TimelineMark (ringCd)
    ]


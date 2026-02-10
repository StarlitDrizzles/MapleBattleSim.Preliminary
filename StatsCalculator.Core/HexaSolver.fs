module HexaSolver

open Lib
open FSharpPlus

let vcosts = [75;23;27;30;34;38;42;45;49;150;60;68;75;83;90;98;105;113;120;263;128;135;143;150;158;165;173;180;188;375] in
let _hexaCosts = Map[
    AssassinateVI, [50;15;18;20;23;25;28;30;33;100;40;45;50;55;60;65;70;75;80;175;85;90;95;100;105;110;115;120;125;250]
    MoneyBombVI, [50;15;18;20;23;25;28;30;33;100;40;45;50;55;60;65;70;75;80;175;85;90;95;100;105;110;115;120;125;250]
    OriginSkill, [-111111;30;35;40;45;50;55;60;65;200;80;90;100;110;120;130;140;150;160;350;170;180;190;200;210;220;230;240;250;500]
    Trickblade, vcosts
    Sonicblow, vcosts
    V4, vcosts
]

let hexaSkills = _hexaCosts.Keys |> Seq.toList

let propagateToMaximums (ec: EvalConfig) (quota: int) =
    let mutable accumCostsDict = Map[]  // s => [0:C(lv+1), 1:C(lv+1)+C(lv+2), ...]
    let mutable maxLvs = Map[]  // right-inclusive

    for s in hexaSkills do
        let curLv = ec.HexaLevels[s]
        if curLv = 30 then
            maxLvs <- maxLvs.Add(s, 30)
        else
        
        let accumCosts = 
            Seq.scan (+) 0 <| Seq.skip curLv _hexaCosts[s]
            |> Seq.skip 1  // scan yields initState (0) first
            |> Seq.toList
        accumCostsDict <- accumCostsDict.Add(s, accumCosts)

        let maxLv =
            Seq.takeWhile (fun ac -> ac <= quota) accumCosts
            |> Seq.length

        maxLvs <- maxLvs.Add(s, maxLv+curLv)
        
    //let mutable ubLvsList: int list list = []

    // { (lvs, accumCost) ... }
    let rec prop (ss: SkillId list) (leftSideAccumCost: int) : (int list * int) seq =
        match ss with
        | s::tail ->
            let curLv = ec.HexaLevels[s]
            if curLv = 30 then
                match tail with
                | [] -> seq { [30],0 }
                | _ ->
                    prop tail leftSideAccumCost
                    |>> (fun (lvs, rhsCost) -> (30::lvs, rhsCost))

            else
            let thisAccumCosts = accumCostsDict[s]
            let inContextMaxLvDelta = 
                Seq.takeWhile (fun ac -> ac + leftSideAccumCost <= quota) thisAccumCosts
                |> Seq.length
            let inContextMaxLv = 
                curLv + inContextMaxLvDelta

            match tail with
            | [] ->
                seq { [inContextMaxLv], if inContextMaxLvDelta = 0 then 0 else thisAccumCosts.[inContextMaxLvDelta-1] }
                
            | _ ->

                seq {

                    let mutable lastTailLvs = []
                    for lv = inContextMaxLv downto curLv do
                        let thisAccumCost =
                            if lv = curLv then
                                0
                            else
                                thisAccumCosts.[lv-curLv-1]
                        
                        for tailMaxLvs, rightSideCost in prop tail (leftSideAccumCost + thisAccumCost) do

                            if lastTailLvs <> tailMaxLvs then
                                yield (lv :: tailMaxLvs, thisAccumCost+rightSideCost)

                            lastTailLvs <- tailMaxLvs
                }

        | _ -> invalidOp "unreachable"


    let ubLvsList = 
        prop hexaSkills 0
        |> Seq.toList
    ubLvsList


let solveHexaPlan (ecInit:EvalConfig) (weeklyQuota: int)
        defRatio mapleBlessing equipsWithRingPlaceholder staticProps phases
    = 

    let lowerAss (dmg:Map<SkillId,float>) =
        let _lower = Option.map ((*) 0.8)
        dmg |> Map.change AssassinateVI _lower |> Map.change PulverizeVI _lower

    let initDmg = 
        evalDmgSeq defRatio ecInit mapleBlessing equipsWithRingPlaceholder staticProps phases
        |> lowerAss
    let initDmg =
        initDmg
        |> Map.toList |>> snd |> sum

    let mutable dmgCache = Map<EvalConfig, float>[]
    dmgCache <- dmgCache.Add(ecInit, initDmg)

    // (ec, weeklyAccumScore, leftQuota, plan)
    let mutable topK_Ecs: (EvalConfig * double * int * (EvalConfig list)) list = [ecInit, 0., 0, []]

    while not (
        let ec, _, _, _ = topK_Ecs.[0]
        ec.HexaLevels.Values |> Seq.forall ((=) 30)
    ) do
        let _x = 
            topK_Ecs
            |> List.collect (fun ( ec, score, leftQuota, plan ) ->
                let oldDmg = dmgCache[ec]

                let quota = weeklyQuota + leftQuota
                let ubLvsList = propagateToMaximums ec quota
                ubLvsList
                |>> (fun (lvs, actualCost) ->
                    let ecCandi = { HexaLevels = List.zip hexaSkills lvs |> Map }

                    let dmgCandi =
                        match dmgCache.TryFind ecCandi with
                        | Some dmg -> dmg
                        | _ ->
                            let dmg =
                                evalDmgSeq defRatio ecCandi mapleBlessing equipsWithRingPlaceholder staticProps phases
                                |> lowerAss
                                |> Map.values |> sum
                            dmgCache <- dmgCache.Add(ecCandi, dmg)
                            dmg

                    ecCandi, score + (oldDmg + dmgCandi) / 2., quota - actualCost, plan @ [ecCandi]
                )
            )
            |> Seq.sortByDescending (fun (ec,score,leftQuota,plan) -> score)
            |> Seq.take 3
            |> Seq.toList
        
        topK_Ecs <- _x

    let (ec30s, score, leftQuota, plan) = topK_Ecs.[0]

    for i, { HexaLevels = lvs } in List.indexed plan do
        printf "(%02d) " i
        for s in hexaSkills do
            printf "%s-%d " <| getShortSkillName s <| lvs.[s]
        printfn ""
        


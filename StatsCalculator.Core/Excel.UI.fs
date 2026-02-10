module ExcelUI

open Lib
open BattleScenarios
open Policy
open Main

open ClosedXML.Excel
open System
open FSharpPlus

let fillinPropUps (workbook:XLWorkbook) =
    let sheet = workbook.Worksheets.Add()
    sheet.Name <- "PropUps"

    let ups = Main.calculatePropUps()

    // Cell row/col are 1-based
    let mutable phaseSecRowBase = 1
    let next n = phaseSecRowBase <- phaseSecRowBase + n

    for (phaseName, perUps) in ups do
        sheet.Cell(phaseSecRowBase, 1).Value <- phaseName
        sheet.Cell(phaseSecRowBase, 3).Value <- "equiv to"
        next 1
        let count = perUps.Length

        (*          col-1   col-2   col-3   col-4
                |   Title           EquivTo
        RowBase |                   Prop1   Prop2
                |   Prop1   Up1%    %       %  ...
                |   Prop2   Up2%    %       %
                |   ...
        *)

        for i = 0 to (count - 1) do
            let (title, up) = perUps.[i]
            sheet.Cell(phaseSecRowBase + 1 + i, 1).Value <- title
            sheet.Cell(phaseSecRowBase + 1 + i, 2).Value <- string (sprintf"%0.2f%% up" up)

            sheet.Cell(phaseSecRowBase, 3 + i).Value <- title

        for i = 0 to (count - 1) do
            for j = 0 to (count - 1) do
                let (_, upBase) = perUps.[i]
                let (_, upOther) = perUps.[j]
                let times = upBase / upOther
                sheet.Cell(phaseSecRowBase + 1 + i, 3 + j).Value <- times
                sheet.Cell(phaseSecRowBase + 1 + i, 3 + j).Style.NumberFormat.Format <- "#,##0.0"

        next (count + 2)

let fillinBattleAnalysis (workbook:XLWorkbook) =
    let byPhases = Main.compareEquipUpgradeCandidates()

    for { Phase=phaseName; EquipPresets=configs } in byPhases do
        let sheet = workbook.Worksheets.Add()
        sheet.Name <- "BA_" + phaseName
        sheet.ColumnWidth <- 10

        // Cell row/col are 1-based
        let nedge = int <| Math.Sqrt (float configs.Length)
        let nedge =
            if nedge * nedge < configs.Length then
                nedge + 1
            else
                nedge
        (* 
        Title0          [ ] Title1
        Skill   Dmg Up  [ ] Skill
        Skill   Dmg Up
        ...
        Total   Dmg Up
        [ ]
        Titlle_nedge
        ...
        *)
        let blk_w = 4 // including white separator
        let blk_h = _attackSkillIds.Length + 3 // including white separator

        let initDmg = configs.[0].Dmg
        let initTotalDmg = initDmg.Values |> sum

        for i_config, { Title=title; Dmg=dmg } in List.indexed configs do
            let blk_row = 1 + i_config / nedge * blk_h
            let blk_col = 1 + i_config % nedge * blk_w

            sheet.Cell(blk_row, blk_col).Value <- title

            for i, k in List.indexed _attackSkillIds do
                let initD = initDmg.[k]
                let d = dmg.[k]
                let upPer = d / initD * 100. - 100.

                sheet.Cell(blk_row + 1 + i, blk_col).Value <- k.ToString()
                sheet.Cell(blk_row + 1 + i, blk_col + 1).Value <- d.ToString()
                sheet.Cell(blk_row + 1 + i, blk_col + 2).Value <- string (sprintf"%0.2f%% up" upPer)


            let totalDmg = dmg|> Map.values |> sum
            let totalUpPer = totalDmg / initTotalDmg * 100. - 100.

            sheet.Cell(blk_row + 1 + _attackSkillIds.Length, blk_col).Value <- "Total"
            sheet.Cell(blk_row + 1 + _attackSkillIds.Length, blk_col + 1).Value <- totalDmg
            sheet.Cell(blk_row + 1 + _attackSkillIds.Length, blk_col + 2).Value <- string (sprintf"%0.2f%% up" totalUpPer)

        


[<EntryPoint>]
let main args = 
    //Main.solveHexaPlan()
    //0
    

    use workbook = new XLWorkbook()

    fillinPropUps workbook
    fillinBattleAnalysis workbook




    let fn = System.IO.Path.Combine(System.IO.Path.GetTempPath(), System.IO.Path.GetRandomFileName() + ".xlsx")
    workbook.SaveAs(fn)
    
    let pi = System.Diagnostics.ProcessStartInfo()
    pi.UseShellExecute <- true
    pi.FileName <- fn
    System.Diagnostics.Process.Start(pi) |> ignore
    1

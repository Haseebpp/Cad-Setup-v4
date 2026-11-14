#:property TargetFramework=net10.0-windows
#:property OutputType=Library
#:property PublishAot=false
#:property UseWindowsForms=true
// ==========================================================================
// DotNet_SaveCopy.cs - SketchUp-style "Save a Copy" without modifying current DWG
// Layer   : DotNet (Level 5)
// Build   : Automatic via Autorun.lsp -> "dotnet build DotNet_SaveCopy.cs"
//           (.NET 10 single-file build, no .csproj. AutoCAD references come
//            from Directory.Build.props; the #:property lines above must stay
//            at the very top of every DotNet_*.cs file)
// Commands: SAVEASCOPY (aliased to SV in Commands/99_Aliases.lsp)
// ==========================================================================

using System;
using System.IO;
using System.Text.RegularExpressions;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.Runtime;
using AcadSaveFileDialog = Autodesk.AutoCAD.Windows.SaveFileDialog;
using CoreApp = Autodesk.AutoCAD.ApplicationServices.Core.Application;

[assembly: CommandClass(typeof(CadSetup.DotNet.SaveCopy))]

namespace CadSetup.DotNet;

/// <summary>
/// Saves an exact incremental copy of the active in-memory drawing database
/// without saving over, renaming, or clearing the dirty state of the open file.
/// </summary>
public class SaveCopy
{
    private static readonly Regex BracketNumberSuffix = new(@"^(.*\()(\d+)(\))$", RegexOptions.Compiled);
    private static readonly Regex TrailingNumberSuffix = new(@"^(.*?)(\d+)$", RegexOptions.Compiled);

    /// <summary>
    /// Saves an incremented copy of the active drawing. For saved drawings, writes
    /// directly to the next free filename in the same folder; for untitled drawings,
    /// prompts for a destination via SaveFileDialog.
    /// </summary>
    [CommandMethod("CADSETUP", "SAVEASCOPY", CommandFlags.Modal)]
    public void SaveAsCopy()
    {
        var doc = CoreApp.DocumentManager.MdiActiveDocument;
        if (doc is null) return;

        var ed = doc.Editor;
        var db = doc.Database;

        bool isTitled = Convert.ToInt32(CoreApp.GetSystemVariable("DWGTITLED")) != 0;
        string? targetPath;

        if (isTitled && !string.IsNullOrWhiteSpace(doc.Name) && Path.IsPathRooted(doc.Name))
        {
            string folder = Path.GetDirectoryName(doc.Name)!;
            string stem = Path.GetFileNameWithoutExtension(doc.Name);
            string ext = Path.GetExtension(doc.Name);
            if (string.IsNullOrEmpty(ext)) ext = ".dwg";

            targetPath = GetNextAvailablePath(folder, stem, ext);
        }
        else
        {
            string baseDir = CoreApp.GetSystemVariable("MYDOCUMENTSPREFIX") as string
                             ?? Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
            if (string.IsNullOrWhiteSpace(baseDir) || !Directory.Exists(baseDir))
                baseDir = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);

            string rawName = string.IsNullOrWhiteSpace(doc.Name)
                ? "Drawing1"
                : Path.GetFileNameWithoutExtension(doc.Name);

            string suggestedPath = GetNextAvailablePath(baseDir, rawName, ".dwg");

            var dlg = new AcadSaveFileDialog(
                "Save a Copy As",
                suggestedPath,
                "dwg",
                "CadSetup.SaveAsCopy",
                AcadSaveFileDialog.SaveFileDialogFlags.DefaultIsFolder);

            if (dlg.ShowDialog() != System.Windows.Forms.DialogResult.OK)
            {
                ed.WriteMessage("\n[SAVEASCOPY] Cancelled.");
                return;
            }

            targetPath = dlg.Filename;
        }

        if (string.IsNullOrWhiteSpace(targetPath))
            return;

        if (isTitled && string.Equals(
                Path.GetFullPath(targetPath),
                Path.GetFullPath(doc.Name),
                StringComparison.OrdinalIgnoreCase))
        {
            ed.WriteMessage("\n[SAVEASCOPY] Aborted: target path matches the active drawing.");
            return;
        }

        try
        {
            // bBakAndRename = false writes the current in-memory database to targetPath
            // without renaming the active document, saving the source file, or resetting DBMOD.
            db.SaveAs(targetPath, false, DwgVersion.Current, db.SecurityParameters);
            ed.WriteMessage($"\n[SAVEASCOPY] Saved copy: {targetPath}");
        }
        catch (System.Exception ex)
        {
            ed.WriteMessage($"\n[SAVEASCOPY] Failed to save copy: {ex.Message}");
        }
    }

    /// <summary>
    /// Increments an existing trailing/bracketed number or appends " (1)" until
    /// an unused file path is found in <paramref name="folder"/>.
    /// </summary>
    internal static string GetNextAvailablePath(string folder, string stem, string ext)
    {
        string candidateStem = IncrementStem(stem);
        string candidatePath = Path.Combine(folder, candidateStem + ext);

        while (File.Exists(candidatePath))
        {
            candidateStem = IncrementStem(candidateStem);
            candidatePath = Path.Combine(folder, candidateStem + ext);
        }

        return candidatePath;
    }

    private static string IncrementStem(string stem)
    {
        var bracketMatch = BracketNumberSuffix.Match(stem);
        if (bracketMatch.Success &&
            long.TryParse(bracketMatch.Groups[2].Value, out long bracketNum))
        {
            string digits = bracketMatch.Groups[2].Value;
            string nextDigits = FormatIncremented(bracketNum, digits.Length);
            return bracketMatch.Groups[1].Value + nextDigits + bracketMatch.Groups[3].Value;
        }

        var trailingMatch = TrailingNumberSuffix.Match(stem);
        if (trailingMatch.Success &&
            long.TryParse(trailingMatch.Groups[2].Value, out long trailingNum))
        {
            string digits = trailingMatch.Groups[2].Value;
            string nextDigits = FormatIncremented(trailingNum, digits.Length);
            return trailingMatch.Groups[1].Value + nextDigits;
        }

        return stem + " (1)";
    }

    private static string FormatIncremented(long current, int width)
    {
        long next = current + 1;
        return width > 1 ? next.ToString().PadLeft(width, '0') : next.ToString();
    }
}

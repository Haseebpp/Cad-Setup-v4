#:property TargetFramework=net10.0-windows
#:property OutputType=Library
#:property PublishAot=false
// ==========================================================================
// DotNet_HelloCad.cs - Sample CadSetup .NET command module (template)
// Layer   : DotNet (Level 5)
// Build   : Automatic via Autorun.lsp -> "dotnet build DotNet_HelloCad.cs"
//           (.NET 10 single-file build, no .csproj. AutoCAD references come
//            from Directory.Build.props; the #:property lines above must stay
//            at the very top of every DotNet_*.cs file)
// Commands: HELLO-NET
// ==========================================================================

using Autodesk.AutoCAD.ApplicationServices.Core;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.Runtime;

[assembly: CommandClass(typeof(CadSetup.DotNet.HelloCad))]

namespace CadSetup.DotNet;

public class HelloCad
{
    [CommandMethod("CADSETUP", "HELLO-NET", CommandFlags.Modal)]
    public void HelloNet()
    {
        var doc = Application.DocumentManager.MdiActiveDocument;
        if (doc is null) return;

        var db = doc.Database;
        int count = 0;

        using (var tr = db.TransactionManager.StartOpenCloseTransaction())
        {
            var ms = (BlockTableRecord)tr.GetObject(
                SymbolUtilityServices.GetBlockModelSpaceId(db), OpenMode.ForRead);
            foreach (ObjectId _ in ms) count++;
            tr.Commit();
        }

        var asm = typeof(HelloCad).Assembly.GetName().Name;
        doc.Editor.WriteMessage(
            $"\n[CadSetup .NET] Hello from {asm} - Model space contains {count} object(s).");
    }
}

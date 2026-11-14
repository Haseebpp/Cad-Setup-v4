#:property TargetFramework=net10.0-windows
#:property OutputType=Library
#:property PublishAot=false
#:property UseWPF=true
// ==========================================================================
// DotNet_MButtonOrbit.cs - Runtime control for Shift + Middle-Click 3D Orbit
// Layer   : DotNet (Level 5)
// Build   : Automatic via Autorun.lsp -> "dotnet build DotNet_MButtonOrbit.cs"
// SysVar  : MBUTTONORBIT (0 = Ignore Shift so Shift+Middle-Click pans like
//           normal Middle-Click, 1 = AutoCAD default Shift+Middle-Click Orbit)
//           Requires one-time HKLM registration via scripts\Register-MButtonOrbit.ps1
// Commands: MBUTTONORBIT-STATUS
// ==========================================================================

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Windows.Threading;
using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.Runtime;
using CoreApp = Autodesk.AutoCAD.ApplicationServices.Core.Application;

[assembly: ExtensionApplication(typeof(CadSetup.DotNet.MButtonOrbitApp))]
[assembly: CommandClass(typeof(CadSetup.DotNet.MButtonOrbitApp))]

namespace CadSetup.DotNet;

/// <summary>
/// When MBUTTONORBIT is 0, temporarily clears Shift state via SendInput and
/// replays the middle-mouse press without Shift so AutoCAD executes its native
/// middle-button behavior (Pan / Zoom Extents) instead of 3D Orbit or the OSNAP menu.
/// </summary>
public class MButtonOrbitApp : IExtensionApplication
{
    private const string SysVarName = "MBUTTONORBIT";
    private const string TeardownSlot = "CadSetup.MButtonOrbit.Teardown";

    private const uint InputMouse = 0;
    private const uint InputKeyboard = 1;
    private const uint MouseEventMiddleDown = 0x0020;
    private const uint MouseEventMiddleUp = 0x0040;
    private const uint KeyEventKeyUp = 0x0002;
    private const uint LlkhfInjected = 0x0010;
    private const int WhKeyboardLl = 13;

    private const int WM_KEYDOWN = 0x0100;
    private const int WM_KEYUP = 0x0101;
    private const int WM_SYSKEYDOWN = 0x0104;
    private const int WM_SYSKEYUP = 0x0105;
    private const int WM_MOUSEMOVE = 0x0200;
    private const int WM_MBUTTONDOWN = 0x0207;
    private const int WM_MBUTTONUP = 0x0208;
    private const int WM_MBUTTONDBLCLK = 0x0209;

    private const long MK_SHIFT = 0x0004;
    private const long MK_MBUTTON = 0x0010;

    private const int VK_MBUTTON = 0x04;
    private const int VK_SHIFT = 0x10;
    private const int VK_LSHIFT = 0xA0;
    private const int VK_RSHIFT = 0xA1;

    private static readonly UIntPtr InjectedTag = new(0x4D424F52u); // "MBOR"
    private static readonly IntPtr InjectedTagPtr = unchecked((IntPtr)(long)InjectedTag.ToUInt64());

    private static bool _hookAttached;
    private static bool _isRegistered;
    private static bool _orbitEnabled;
    private static bool _shiftSuppressed;
    private static bool _awaitingSyntheticMiddleDown;
    private static bool _replayingAsDblClk;
    private static bool _swallowNextInjectedMiddleUp;
    private static long _replayTickMs;
    private static bool _physicalLeftDown;
    private static bool _physicalRightDown;
    private static bool _physicalGenericDown;
    private static IntPtr _kbHook = IntPtr.Zero;
    private static LowLevelKeyboardProc? _kbProc;
    private static Action? _ownTeardown;

    private delegate IntPtr LowLevelKeyboardProc(int nCode, IntPtr wParam, IntPtr lParam);

    [StructLayout(LayoutKind.Sequential)]
    private struct INPUT
    {
        public uint type;
        public InputUnion U;
    }

    [StructLayout(LayoutKind.Explicit)]
    private struct InputUnion
    {
        [FieldOffset(0)] public MOUSEINPUT mi;
        [FieldOffset(0)] public KEYBDINPUT ki;
        [FieldOffset(0)] public HARDWAREINPUT hi;
    }

    [StructLayout(LayoutKind.Sequential)]
    private struct MOUSEINPUT
    {
        public int dx;
        public int dy;
        public uint mouseData;
        public uint dwFlags;
        public uint time;
        public UIntPtr dwExtraInfo;
    }

    [StructLayout(LayoutKind.Sequential)]
    private struct KEYBDINPUT
    {
        public ushort wVk;
        public ushort wScan;
        public uint dwFlags;
        public uint time;
        public UIntPtr dwExtraInfo;
    }

    [StructLayout(LayoutKind.Sequential)]
    private struct HARDWAREINPUT
    {
        public uint uMsg;
        public ushort wParamL;
        public ushort wParamH;
    }

    [StructLayout(LayoutKind.Sequential)]
    private struct KBDLLHOOKSTRUCT
    {
        public uint vkCode;
        public uint scanCode;
        public uint flags;
        public uint time;
        public UIntPtr dwExtraInfo;
    }

    [DllImport("user32.dll")]
    private static extern bool IsChild(IntPtr hWndParent, IntPtr hWnd);

    [DllImport("user32.dll")]
    private static extern short GetKeyState(int nVirtKey);

    [DllImport("user32.dll")]
    private static extern short GetAsyncKeyState(int vKey);

    [DllImport("user32.dll")]
    private static extern bool GetKeyboardState(byte[] lpKeyState);

    [DllImport("user32.dll")]
    private static extern bool SetKeyboardState(byte[] lpKeyState);

    [DllImport("user32.dll")]
    private static extern IntPtr GetMessageExtraInfo();

    [DllImport("user32.dll", SetLastError = true)]
    private static extern uint SendInput(uint nInputs, INPUT[] pInputs, int cbSize);

    [DllImport("user32.dll")]
    private static extern uint MapVirtualKey(uint uCode, uint uMapType);

    [DllImport("user32.dll", SetLastError = true)]
    private static extern IntPtr SetWindowsHookEx(int idHook, LowLevelKeyboardProc lpfn, IntPtr hMod, uint dwThreadId);

    [DllImport("user32.dll", SetLastError = true)]
    private static extern bool UnhookWindowsHookEx(IntPtr hhk);

    [DllImport("user32.dll")]
    private static extern IntPtr CallNextHookEx(IntPtr hhk, int nCode, IntPtr wParam, IntPtr lParam);

    [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
    private static extern IntPtr GetModuleHandle(string? lpModuleName);

    public void Initialize()
    {
        // Unhook any older hot-reloaded assembly in this session before subscribing.
        if (AppDomain.CurrentDomain.GetData(TeardownSlot) is Action previousTeardown)
        {
            previousTeardown();
        }

        CoreApp.PreTranslateMessage += OnPreTranslateMessage;
        CoreApp.SystemVariableChanged += OnSystemVariableChanged;
        CoreApp.Idle += OnApplicationIdle;
        _hookAttached = true;

        _ownTeardown = DetachHandlers;
        AppDomain.CurrentDomain.SetData(TeardownSlot, _ownTeardown);

        SyncState();

        if (!_isRegistered)
        {
            var ed = CoreApp.DocumentManager?.MdiActiveDocument?.Editor;
            ed?.WriteMessage(
                "\n[CadSetup .NET] Notice: MBUTTONORBIT sysvar is not registered in HKLM yet. " +
                "Ignoring Shift on Middle-Click by default; run scripts\\Register-MButtonOrbit.ps1 as Admin and restart AutoCAD.");
        }
    }

    public void Terminate()
    {
        DetachHandlers();
        if (ReferenceEquals(AppDomain.CurrentDomain.GetData(TeardownSlot), _ownTeardown))
        {
            AppDomain.CurrentDomain.SetData(TeardownSlot, null);
        }
    }

    [CommandMethod("CADSETUP", "MBUTTONORBIT-STATUS", CommandFlags.Transparent)]
    public void Status()
    {
        SyncState();
        var ed = CoreApp.DocumentManager?.MdiActiveDocument?.Editor;
        if (ed is null) return;

        string regText = _isRegistered ? "registered" : "NOT registered (run scripts\\Register-MButtonOrbit.ps1 as Admin)";
        string valText = _isRegistered
            ? (_orbitEnabled ? "1 (Shift+Middle-Click Orbit ON)" : "0 (Shift+Middle-Click acts as Middle-Click Pan)")
            : "0 (fallback default: Shift+Middle-Click acts as Middle-Click Pan)";

        object? mbPan = CoreApp.TryGetSystemVariable("MBUTTONPAN");
        string mbPanText = mbPan is null ? "?" : mbPan.ToString() ?? "?";

        ed.WriteMessage(
            $"\n[CadSetup .NET] MBUTTONORBIT = {valText} | MBUTTONPAN = {mbPanText} | Registry: {regText} | Hook: {(_hookAttached ? "active" : "inactive")}");
    }

    private static void DetachHandlers()
    {
        if (!_hookAttached) return;
        CoreApp.PreTranslateMessage -= OnPreTranslateMessage;
        CoreApp.SystemVariableChanged -= OnSystemVariableChanged;
        CoreApp.Idle -= OnApplicationIdle;
        RestoreShiftState();
        _hookAttached = false;
    }

    private static void OnSystemVariableChanged(object? sender, SystemVariableChangedEventArgs e)
    {
        if (string.Equals(e.Name, SysVarName, StringComparison.OrdinalIgnoreCase))
        {
            SyncState();
        }
    }

    private static void OnApplicationIdle(object? sender, EventArgs e)
    {
        if (!_shiftSuppressed) return;
        if ((GetAsyncKeyState(VK_MBUTTON) & 0x8000) != 0) return;

        bool timedOut = (Environment.TickCount64 - _replayTickMs) > 250;
        if ((!_awaitingSyntheticMiddleDown && !_swallowNextInjectedMiddleUp) || timedOut)
        {
            RestoreShiftState();
        }
    }

    private static void OnPreTranslateMessage(object? sender, PreTranslateMessageEventArgs e)
    {
        var msg = e.Message;

        switch (msg.message)
        {
            case WM_KEYDOWN:
            case WM_KEYUP:
            case WM_SYSKEYDOWN:
            case WM_SYSKEYUP:
            {
                if (GetMessageExtraInfo() == InjectedTagPtr)
                {
                    // Swallow our own Shift keyup/keydown so AutoCAD TEMPOVERRIDES does not flicker.
                    e.Handled = true;
                }
                return;
            }

            case WM_MBUTTONDOWN:
            case WM_MBUTTONDBLCLK:
            {
                if (GetMessageExtraInfo() == InjectedTagPtr)
                {
                    // Rewrite synthetic down back to single vs double-click because Win32
                    // promotes rapid MIDDLEUP+MIDDLEDOWN at the same point to WM_MBUTTONDBLCLK.
                    _awaitingSyntheticMiddleDown = false;
                    _swallowNextInjectedMiddleUp = false;
                    msg.message = _replayingAsDblClk ? WM_MBUTTONDBLCLK : WM_MBUTTONDOWN;
                    long injectedFlags = msg.wParam.ToInt64();
                    if ((injectedFlags & MK_SHIFT) != 0)
                    {
                        msg.wParam = new IntPtr(injectedFlags & ~MK_SHIFT);
                    }
                    e.Message = msg;
                    return;
                }

                long flags = msg.wParam.ToInt64();
                bool hasShift = (flags & MK_SHIFT) != 0
                    || (GetKeyState(VK_SHIFT) & 0x8000) != 0
                    || (GetAsyncKeyState(VK_SHIFT) & 0x8000) != 0;
                if (!hasShift)
                {
                    _awaitingSyntheticMiddleDown = false;
                    return;
                }

                if (!IsTargetInActiveDocumentWindow(msg.hwnd))
                {
                    return;
                }

                SyncState();
                if (_orbitEnabled)
                {
                    return;
                }

                // Swallow Shift+MiddleDown and replay a clean MiddleDown with Shift released
                // across wParam, GetKeyState, and GetAsyncKeyState so MBUTTONPAN triggers Pan.
                e.Handled = true;
                ReplayMiddleDownWithoutShift(isDoubleClick: msg.message == WM_MBUTTONDBLCLK);
                return;
            }

            case WM_MOUSEMOVE:
            {
                if (!_shiftSuppressed) return;

                long flags = msg.wParam.ToInt64();
                if ((flags & MK_MBUTTON) == 0)
                {
                    if (!_awaitingSyntheticMiddleDown)
                    {
                        RestoreShiftState();
                        return;
                    }
                }

                if ((flags & MK_SHIFT) != 0)
                {
                    msg.wParam = new IntPtr(flags & ~MK_SHIFT);
                    e.Message = msg;
                }
                return;
            }

            case WM_MBUTTONUP:
            {
                if (_swallowNextInjectedMiddleUp && GetMessageExtraInfo() == InjectedTagPtr)
                {
                    _swallowNextInjectedMiddleUp = false;
                    e.Handled = true;
                    return;
                }

                if (_awaitingSyntheticMiddleDown)
                {
                    // Swallow an orphaned physical WM_MBUTTONUP queued before synthetic MIDDLEDOWN arrives.
                    e.Handled = true;
                    return;
                }

                if (!_shiftSuppressed) return;

                long flags = msg.wParam.ToInt64();
                if ((flags & MK_SHIFT) != 0)
                {
                    msg.wParam = new IntPtr(flags & ~MK_SHIFT);
                    e.Message = msg;
                }

                Dispatcher.CurrentDispatcher.BeginInvoke(DispatcherPriority.Normal, RestoreShiftState);
                return;
            }
        }
    }

    private static void ReplayMiddleDownWithoutShift(bool isDoubleClick)
    {
        bool leftDown = (GetAsyncKeyState(VK_LSHIFT) & 0x8000) != 0;
        bool rightDown = (GetAsyncKeyState(VK_RSHIFT) & 0x8000) != 0;
        bool genericOnly = !leftDown && !rightDown
            && ((GetAsyncKeyState(VK_SHIFT) & 0x8000) != 0 || (GetKeyState(VK_SHIFT) & 0x8000) != 0);

        _physicalLeftDown = leftDown;
        _physicalRightDown = rightDown;
        _physicalGenericDown = genericOnly;
        EnsureKeyboardHook();
        ClearThreadShiftState();

        var inputs = new List<INPUT>(5);
        if (leftDown) inputs.Add(CreateKeyInput(VK_LSHIFT, isKeyUp: true));
        if (rightDown) inputs.Add(CreateKeyInput(VK_RSHIFT, isKeyUp: true));
        if (genericOnly) inputs.Add(CreateKeyInput(VK_SHIFT, isKeyUp: true));

        bool middleStillDown = (GetAsyncKeyState(VK_MBUTTON) & 0x8000) != 0;
        if (middleStillDown)
        {
            // Toggle middle button 1->0->1 so Windows emits a fresh Shift-free press.
            inputs.Add(CreateMouseInput(MouseEventMiddleUp));
            inputs.Add(CreateMouseInput(MouseEventMiddleDown));
        }
        else
        {
            // Fast click already physically released before replay ran; emit complete clean click.
            inputs.Add(CreateMouseInput(MouseEventMiddleDown));
            inputs.Add(CreateMouseInput(MouseEventMiddleUp));
        }

        _replayingAsDblClk = isDoubleClick;
        _swallowNextInjectedMiddleUp = middleStillDown;
        _awaitingSyntheticMiddleDown = true;
        _replayTickMs = Environment.TickCount64;
        _shiftSuppressed = true;

        uint sent = SendInput((uint)inputs.Count, inputs.ToArray(), Marshal.SizeOf<INPUT>());
        if (sent != inputs.Count)
        {
            _replayingAsDblClk = false;
            _swallowNextInjectedMiddleUp = false;
            _awaitingSyntheticMiddleDown = false;
            RestoreShiftState();
            return;
        }

        Dispatcher.CurrentDispatcher.BeginInvoke(DispatcherPriority.Background, () =>
        {
            if (_shiftSuppressed && !_awaitingSyntheticMiddleDown && !_swallowNextInjectedMiddleUp
                && (GetAsyncKeyState(VK_MBUTTON) & 0x8000) == 0)
            {
                RestoreShiftState();
            }
        });
    }

    private static void RestoreShiftState()
    {
        bool restoreLeft = _physicalLeftDown;
        bool restoreRight = _physicalRightDown;
        bool restoreGeneric = _physicalGenericDown;
        _physicalLeftDown = false;
        _physicalRightDown = false;
        _physicalGenericDown = false;
        _replayingAsDblClk = false;
        _swallowNextInjectedMiddleUp = false;
        _awaitingSyntheticMiddleDown = false;
        _shiftSuppressed = false;
        UnhookKeyboardHook();

        if (restoreLeft || restoreRight || restoreGeneric)
        {
            var inputs = new List<INPUT>(2);
            if (restoreLeft) inputs.Add(CreateKeyInput(VK_LSHIFT, isKeyUp: false));
            if (restoreRight) inputs.Add(CreateKeyInput(VK_RSHIFT, isKeyUp: false));
            if (!restoreLeft && !restoreRight && restoreGeneric)
            {
                inputs.Add(CreateKeyInput(VK_SHIFT, isKeyUp: false));
            }
            SendInput((uint)inputs.Count, inputs.ToArray(), Marshal.SizeOf<INPUT>());
        }

        bool anyShift = restoreLeft || restoreRight || restoreGeneric;
        var state = new byte[256];
        if (GetKeyboardState(state))
        {
            SetKeyDownBit(state, VK_LSHIFT, restoreLeft);
            SetKeyDownBit(state, VK_RSHIFT, restoreRight);
            SetKeyDownBit(state, VK_SHIFT, anyShift);
            SetKeyboardState(state);
        }
    }

    private static void ClearThreadShiftState()
    {
        var state = new byte[256];
        if (!GetKeyboardState(state)) return;

        state[VK_SHIFT] &= 0x7F;
        state[VK_LSHIFT] &= 0x7F;
        state[VK_RSHIFT] &= 0x7F;
        SetKeyboardState(state);
    }

    private static void EnsureKeyboardHook()
    {
        if (_kbHook != IntPtr.Zero) return;
        _kbProc ??= KeyboardHookCallback;
        _kbHook = SetWindowsHookEx(WhKeyboardLl, _kbProc, GetModuleHandle(null), 0);
    }

    private static void UnhookKeyboardHook()
    {
        if (_kbHook == IntPtr.Zero) return;
        UnhookWindowsHookEx(_kbHook);
        _kbHook = IntPtr.Zero;
    }

    private static IntPtr KeyboardHookCallback(int nCode, IntPtr wParam, IntPtr lParam)
    {
        if (nCode >= 0)
        {
            var info = Marshal.PtrToStructure<KBDLLHOOKSTRUCT>(lParam);
            if ((info.flags & LlkhfInjected) == 0)
            {
                int msg = wParam.ToInt32();
                bool isDown = msg == WM_KEYDOWN || msg == WM_SYSKEYDOWN;
                bool isUp = msg == WM_KEYUP || msg == WM_SYSKEYUP;

                if (isDown || isUp)
                {
                    if (info.vkCode == VK_LSHIFT) _physicalLeftDown = isDown;
                    else if (info.vkCode == VK_RSHIFT) _physicalRightDown = isDown;
                    else if (info.vkCode == VK_SHIFT) _physicalGenericDown = isDown;
                }
            }
        }

        return CallNextHookEx(_kbHook, nCode, wParam, lParam);
    }

    private static INPUT CreateKeyInput(ushort vk, bool isKeyUp)
    {
        return new INPUT
        {
            type = InputKeyboard,
            U = new InputUnion
            {
                ki = new KEYBDINPUT
                {
                    wVk = vk,
                    wScan = (ushort)MapVirtualKey(vk, 0),
                    dwFlags = isKeyUp ? KeyEventKeyUp : 0,
                    time = 0,
                    dwExtraInfo = InjectedTag
                }
            }
        };
    }

    private static INPUT CreateMouseInput(uint flags)
    {
        return new INPUT
        {
            type = InputMouse,
            U = new InputUnion
            {
                mi = new MOUSEINPUT
                {
                    dx = 0,
                    dy = 0,
                    mouseData = 0,
                    dwFlags = flags,
                    time = 0,
                    dwExtraInfo = InjectedTag
                }
            }
        };
    }

    private static void SetKeyDownBit(byte[] state, int vk, bool isDown)
    {
        if (isDown) state[vk] |= 0x80;
        else state[vk] &= 0x7F;
    }

    private static bool IsTargetInActiveDocumentWindow(IntPtr targetHwnd)
    {
        if (targetHwnd == IntPtr.Zero) return false;

        try
        {
            var doc = CoreApp.DocumentManager?.MdiActiveDocument;
            var docWindow = doc?.Window;
            if (docWindow is null) return false;

            var docHwnd = docWindow.Handle;
            return docHwnd != IntPtr.Zero && (targetHwnd == docHwnd || IsChild(docHwnd, targetHwnd));
        }
        catch
        {
            return false;
        }
    }

    private static void SyncState()
    {
        try
        {
            object? raw = CoreApp.TryGetSystemVariable(SysVarName);
            if (raw is not null)
            {
                _isRegistered = true;
                _orbitEnabled = Convert.ToInt32(raw) != 0;
                return;
            }
        }
        catch
        {
            // Fall back to safe disabled state if AutoCAD is mid-transition or var is missing.
        }

        _isRegistered = false;
        _orbitEnabled = false;
    }
}

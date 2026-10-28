unit Forms.DoctorWait;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  // Modal while doctor runs in its own console, so the user knows why the main window waits, and can stop it.
  // Closes itself when the doctor process ends. Closing it stops doctor.
  TDoctorWaitForm = class(TForm)
    lbMessage: TLabel;
    Panel1: TPanel;
    btStop: TButton;
    btSwitch: TButton;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure btSwitchClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FProcess: THandle;
    function DoctorFinished: Boolean;
    function FindDoctorWindow: HWND;
  public
    /// <summary>Returns once the doctor process has ended, by itself or because the user stopped it.</summary>
    class procedure WaitFor(const Process: THandle);
  end;

implementation

{$R *.dfm}

class procedure TDoctorWaitForm.WaitFor(const Process: THandle);
begin
  var Form := TDoctorWaitForm.Create(Application);
  try
    Form.FProcess := Process;
    // Doctor may already be gone, e.g. it failed at start. ShowModal would still show for a timer tick.
    if not Form.DoctorFinished then
      Form.ShowModal;
  finally
    Form.Free;
  end;
end;

function TDoctorWaitForm.DoctorFinished: Boolean;
begin
  Result := WaitForSingleObject(FProcess, 0) <> WAIT_TIMEOUT;
end;

procedure TDoctorWaitForm.TimerTimer(Sender: TObject);
begin
  if DoctorFinished then
    ModalResult := mrOk;
end;

function TDoctorWaitForm.FindDoctorWindow: HWND;
begin
  Result := 0;
  var Pid := GetProcessId(FProcess);
  if Pid = 0 then Exit;
  // The console isn't ours, so we attach to it for a moment and ask for its window. We are a GUI app with no
  // console of our own, so this works. Under Windows Terminal it returns a pseudo window, owned by the terminal
  // window: GA_ROOTOWNER gets that one. For the classic console it is the window itself.
  if not AttachConsole(Pid) then Exit;
  try
    Result := GetConsoleWindow;
  finally
    FreeConsole;
  end;
  if Result <> 0 then
    Result := GetAncestor(Result, GA_ROOTOWNER);
end;

procedure TDoctorWaitForm.btSwitchClick(Sender: TObject);
begin
  // The console may not be on the taskbar, so with tmsgui maximized there is no other way back to it.
  var Wnd := FindDoctorWindow;
  if Wnd = 0 then Exit;
  if IsIconic(Wnd) then
    ShowWindow(Wnd, SW_RESTORE);
  SetForegroundWindow(Wnd);
end;

procedure TDoctorWaitForm.btStopClick(Sender: TObject);
begin
  Close; // FormCloseQuery stops doctor
end;

procedure TDoctorWaitForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Closing the form, with the button, the X or Alt+F4, stops doctor.
  // We only close once it has ended: WaitFor returning is what lets the GUI run tms again.
  if not DoctorFinished then
  begin
    TerminateProcess(FProcess, 1);
    WaitForSingleObject(FProcess, 5000); // TerminateProcess doesn't wait for the process to end
  end;
  CanClose := DoctorFinished;
end;

end.

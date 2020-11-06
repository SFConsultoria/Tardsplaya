unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, System.Generics.Collections,
  OtlThreadPool, OtlComm, OtlTask, OtlTaskControl,
  OtlParallel, OtlCollections, OtlCommon, Unit2,
  Winsock, ShellApi, Vcl.ComCtrls, System.DateUtils,
  Vcl.Menus, System.IniFiles,  System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent, Vcl.ExtCtrls,
  Vcl.AppEvnts;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    PopupMenu1: TPopupMenu;
    Clear1: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    ools1: TMenuItem;
    Settings1: TMenuItem;
    httpcli1: TNetHTTPClient;
    TrayIcon1: TTrayIcon;
    ApplicationEvents1: TApplicationEvents;
    PageControl2: TPageControl;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    lblChannel: TLabel;
    lblQuality: TLabel;
    lblClusterTitle: TLabel;
    lblClusterVal: TLabel;
    lblSectionsVal: TLabel;
    lblSectionsTitle: TLabel;
    edtChannel: TEdit;
    btnLoad: TButton;
    btnWatch: TButton;
    lstQuality: TListBox;
    btnIncSections: TButton;
    btnDecSections: TButton;
    TabSheet2: TTabSheet;
    chkAutoScroll: TCheckBox;
    chkEnableLogging: TCheckBox;
    lvLog: TListView;
    chkLogOnlyErrors: TCheckBox;
    TabSheet3: TTabSheet;
    Panel1: TPanel;
    lstFavorites: TListBox;
    lblFavorites: TLabel;
    btnAddFavorite: TButton;
    btnDeleteFavorite: TButton;
    btnEditFavorite: TButton;
    btnCheckVersion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnWatchClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure lstQualityClick(Sender: TObject);
    procedure lblTardsNetClick(Sender: TObject);
    procedure lstFavoritesClick(Sender: TObject);
    procedure lstFavoritesDblClick(Sender: TObject);
    procedure lstFavoritesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lstFavoritesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lstFavoritesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnEditFavoriteClick(Sender: TObject);
    procedure btnDeleteFavoriteClick(Sender: TObject);
    procedure btnAddFavoriteClick(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure btnIncSectionsClick(Sender: TObject);
    procedure btnDecSectionsClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Settings1Click(Sender: TObject);
    procedure httpcli1ValidateServerCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const Certificate: TCertificate;
      var Accepted: Boolean);
    procedure btnCheckVersionClick(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
  private
    FSectionCount: Integer;
    FChunkSize: Integer;
    FPlayerHandle: THandle;
    FHelloWorker: IOmniTaskControl;
    FWritingStream: Boolean;
    FStreamUrlQueue: TList<TStreamUrlQueueItem>;
    FStreamUrl: string;
    FStreamPath: string;
    startingPoint: TPoint;
    FHttpCliPool: TThreadList<TNetHTTPClient>;
    FOverrideServerCertificateValidation: Boolean;
    FOverrideUserAgent: string;
    FOverrideConnectionTimeout: Integer;
    procedure CheckFavorites();
    procedure SaveFavorites();
    procedure DeleteFavorite;
    procedure DoGetVideo;
    procedure TaskMessageProc(const task: IOmniTaskControl;
      const msg: TOmniMessage);
    procedure UpdateQueueText(q: Integer);
    procedure FreeQualities;
    procedure WriteToLog(str: String; isError: Boolean);

    procedure HttpDocDataEvent(Sender: TObject; Buffer: Pointer; Len: Integer);
    procedure HttpDocEndEvent(Sender: TObject);
    procedure GetSettings;
  public
    { Public declarations }
  end;

type
  TExtInf = class
    duration: Double;
    url: string;
    constructor Create(durationStr: string; url: string);
  end;

type
  THelloWorker = class(TOmniWorker)
  strict private
    FStreamUrl: string;

    FLastTime: TDateTime;

    // HttpCli1: THttpCli;
    DataIn: TMemoryStream;
    strList: TStringList;
    FExtMediaSequence: Integer;
    extInfs: TList<TExtInf>;
  private
  public
    constructor Create(const streamUrl: string);
    destructor Destroy; override;
    function Initialize: Boolean; override;
    procedure StartWork(var msg: TMessage); message MSG_START_WORK;
  end;

type
  TQuality = class
    name: string;
    url: string;
    resolution: string;
    // bitrate: string;
  end;

var
  Form1: TForm1;
  PlayerPath: string;
  PlayerCmd: string;
  AutoConfirmFavoriteDeletion: Boolean;
  MinimizeToTray: Boolean;
  defaultPlayerPath: string;
  defaultPlayerCmd: string;

procedure SetFormIcons(FormHandle: HWND; SmallIconName, LargeIconName: string);

implementation

uses
  System.UITypes, superobject, frmSettings;

{$R *.dfm}

function DateTimeToUNIXTimeFAST(DelphiTime: TDateTime): LongWord;
begin
  Result := Round((DelphiTime - 25569) * 86400);
end;

function SplitExt(line: string): TStringList;
var
  tmpStr: string;
begin
  Result := TStringList.Create;
  tmpStr := line;
  tmpStr := tmpStr.Replace('"', '');
  Result.CommaText := tmpStr;
end;

procedure TForm1.WriteToLog(str: String; isError: Boolean);
begin
  if chkEnableLogging.Checked then
  begin
    if ((chkLogOnlyErrors.Checked) and (isError)) or
      (not chkLogOnlyErrors.Checked) then
    begin
      with lvLog.Items.Add do
      begin
        Caption := FormatDateTime('hh:nn:ss.zzz', Now);
        SubItems.Add(str);
        if chkAutoScroll.Checked then
        begin
          MakeVisible(False);
        end;
      end;
    end;
  end;
end;

function DataInToString(DataIn: TMemoryStream): string;
var
  SR: TStreamReader;
begin
  DataIn.Seek(0, soBeginning);
  SR := TStreamReader.Create(DataIn, TEncoding.UTF8, True);
  try
    Result := SR.ReadToEnd;
  finally
    FreeAndNil(SR);
  end;
end;

procedure TForm1.FreeQualities();
var
  i: Integer;
begin
  lblClusterVal.Caption := '-';
  if lstQuality.Items.Count > 0 then
  begin
    for i := 0 to lstQuality.Items.Count - 1 do
    begin
      lstQuality.Items.Objects[i].Free;
    end;
    lstQuality.Items.Clear;
  end;
end;

procedure TForm1.HttpDocDataEvent(Sender: TObject; Buffer: Pointer;
  Len: Integer);

// procedure MsgProcCreateWriteChunkedStreamTask(itm: TStreamUrlQueueItem);
// begin
// WriteToLog(Format('Begin feeding chunk %d (partial size %d) to player', [itm.id, itm.contentLength]), False);
//
// CreateTask(WriteStreamToPlayer)
// .OnMessage(TaskMessageProc)
// .SetParameter('item', itm)
// .Unobserved
// .Schedule;
// end;
//
// var
// httpcli: THttpCli;
// item: TStreamUrlQueueItem;
// begin
// httpcli := Sender as THttpCli;
// item := TStreamUrlQueueItem.Create;
// item.id := httpcli.Tag;
// item.content := AllocMem(Len);
// item.contentLength := Len;
// Move(Buffer^, item.Content^, Len);
// MsgProcCreateWriteChunkedStreamTask(item);
begin
  // NO-OP
end;

procedure TForm1.HttpDocEndEvent(Sender: TObject);
begin
  // NO-OP
end;

procedure TForm1.GetSettings;
var
  IniFile: TIniFile;
  fn: string;

begin
  fn := ChangeFileExt(Application.ExeName, '.ini');
  if FileExists(fn) then
  begin
    IniFile := TIniFile.Create(fn);
    try
      PlayerPath := IniFile.ReadString('Settings', 'PlayerPath',
        defaultPlayerPath);
      PlayerCmd := IniFile.ReadString('Settings', 'PlayerCmd',
        defaultPlayerCmd);
      AutoConfirmFavoriteDeletion := IniFile.ReadBool('Settings',
        'AutoConfirmFavoriteDeletion', False);
      FChunkSize := IniFile.ReadInteger('Settings', 'ChunkSize', 15000);
      FOverrideServerCertificateValidation := not IniFile.ReadBool('Settings',
        'ServerCertValidation', True);
      FOverrideUserAgent := IniFile.ReadString('Settings', 'UserAgent',
        'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.154 Safari/537.36');
      FOverrideConnectionTimeout := IniFile.ReadInteger('Settings',
        'ConnectionTimeout', 10000);
      MinimizeToTray := IniFile.ReadBool('Settings', 'MinimizeToTray', True);

    finally
      IniFile.Free;
    end;
  end;
  fn := ExtractFilePath(ParamStr(0)) + 'favorites.txt';
  if FileExists(fn) then
  begin
    lstFavorites.Items.LoadFromFile(fn);
  end;
end;

procedure TForm1.lblTardsNetClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PWideChar('http://tards.net/'), '', '',
    SW_SHOWNORMAL);
end;

procedure TForm1.lstQualityClick(Sender: TObject);
begin
  if lstQuality.ItemIndex > -1 then
  begin
    btnWatch.Enabled := True;
  end
  else
  begin
    btnWatch.Enabled := False;
  end;
end;

procedure TForm1.httpcli1ValidateServerCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate;
  var Accepted: Boolean);
begin
  Accepted := Accepted or FOverrideServerCertificateValidation;
end;

procedure TForm1.lstFavoritesClick(Sender: TObject);
begin
  CheckFavorites;
end;

procedure TForm1.lstFavoritesDblClick(Sender: TObject);
begin
  if lstFavorites.ItemIndex > -1 then
  begin
    edtChannel.Text := lstFavorites.Items[lstFavorites.ItemIndex];
  end;
end;

procedure TForm1.lstFavoritesDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DropPosition, StartPosition: Integer;
  DropPoint: TPoint;
begin
  DropPoint.X := X;
  DropPoint.Y := Y;
  with Source as TListBox do
  begin
    StartPosition := ItemAtPos(startingPoint, True);
    DropPosition := ItemAtPos(DropPoint, True);
    if (StartPosition <> -1) and (DropPosition <> -1) then
    begin
      Items.Move(StartPosition, DropPosition);
      SaveFavorites();
    end;
  end;
end;

procedure TForm1.lstFavoritesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = lstFavorites;
end;

procedure TForm1.lstFavoritesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  startingPoint.X := X;
  startingPoint.Y := Y;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
const
  MSG_URL_ERROR = 1;
  MSG_URL_QUALITY = 2;
  MSG_URL_DONE = 3;
var
  channelName: string;
begin
  channelName := Trim(edtChannel.Text);
  channelName := channelName.ToLower;

  if (channelName = '') then
  begin
    MessageDlg('no channel', mtError, [mbOk], 0);
    exit;
  end;

  btnLoad.Enabled := False;
  btnLoad.Caption := 'Loading...';

  btnWatch.Enabled := False;
  lstQuality.Enabled := False;
  lvLog.Clear;

  FreeQualities();

  lstQuality.Enabled := False;

  CreateTask(
    procedure(const task: IOmniTask)
    var
      channel: String;
      url: string;
      DataIn: TMemoryStream;
      tmpStr: string;
      json: ISuperObject;
      strList: TStringList;
      i: Integer;
      quality: TQuality;
      s: string;
{$IFDEF DEBUG}
      sw: TStreamWriter;
{$ENDIF}
    begin
      channel := task.Param['channel'].AsString;

      DataIn := TMemoryStream.Create;
      // HttpCli1 := THttpCli.Create(nil);
      strList := TStringList.Create;

      try
        try
          httpcli1.UserAgent := FOverrideUserAgent;
          httpcli1.Accept := '*/*';
          httpcli1.ConnectionTimeout := FOverrideConnectionTimeout;

          DataIn.Clear;
          url :=
          // 'https://pwn.sh/tools/streamapi.py?url=twitch.tv/beyondthesummit';
            'https://pwn.sh/tools/streamapi.py?url=twitch.tv/' + channel;

          try
            httpcli1.Get(url, DataIn);
          except
            on E: Exception do
            begin
              if (E.Message = 'Not Found') then
              begin
                task.Comm.Send(MSG_URL_ERROR, '[1] Error: Channel not found');
              end
              else
              begin
                task.Comm.Send(MSG_URL_ERROR, Format('[1] Error: %s',
                  [E.Message]));
              end;
              exit;
            end;
          end;

          tmpStr := DataInToString(DataIn);
          if tmpStr = '' then
          begin
            task.Comm.Send(MSG_URL_ERROR, '[2] Error: access token empty');
            exit;
          end;

{$IFDEF DEBUF}
          try
            sw := TStreamWriter.Create('playlist.json');
            sw.Write(tmpStr);
            sw.Free;
          except
          end;
{$ENDIF}
          try
            json := SO(tmpStr);
          except
            on E: Exception do
            begin
              task.Comm.Send(MSG_URL_ERROR, Format('[3] Error: %s',
                [E.Message]));
              exit;
            end;
          end;

          DataIn.Clear;

          if (not json.o['success'].AsBoolean) then
          begin
            task.Comm.Send(MSG_URL_ERROR, '[1] Error: ' + json.o['error']
              .AsString);
            exit;
          end;

          // HttpCli1.URL := 'http://usher.twitch.tv/api/channel/hls/' + channel + '.m3u8?sig=' + UrlEncode(json.AsObject.S['sig']) + '&token=' + UrlEncode(json.AsObject.S['token']) + '&allow_source=true&type=any&private_code=&rnd=' + IntToStr(DateTimeToUNIXTimeFAST(Now()));
          // try
          // HttpCli1.Get;
          // except
          // on E: Exception do
          // begin
          // if (E.Message = 'Not Found') then
          // begin
          // task.Comm.Send(MSG_URL_ERROR, '[4] Error: Stream not online');
          // end
          // else
          // begin
          // task.Comm.Send(MSG_URL_ERROR, Format('[4] Error: %s', [E.Message]));
          // end;
          // Exit;
          // end;
          // end;
          { *

            for i := 0 to strList.Count - 1 do
            begin
            if strList[i].StartsWith('#EXT-X') then
            begin
            if strList[i].StartsWith('#EXT-X-MEDIA:') then
            begin
            ext := SplitExt(strList[i].Replace('#EXT-X-MEDIA:', ''));
            tmpName := ext.Values['NAME'];
            ext.Free;
            end
            else if strList[i].StartsWith('#EXT-X-STREAM-INF:') then
            begin
            ext := SplitExt(strList[i].Replace('#EXT-X-STREAM-INF:', ''));
            tmpResolution := ext.Values['RESOLUTION'];
            tmpBitrate := ext.Values['BANDWIDTH'];
            ext.Free;
            end
            else if strList[i].StartsWith('#EXT-X-TWITCH-INFO:') then
            begin
            ext := SplitExt(strList[i].Replace('#EXT-X-TWITCH-INFO:', ''));
            tmpCluster := ext.Values['CLUSTER'];
            ext.Free;
            end;
            end
            else
            begin
            if strList[i].Contains('://') then
            begin
            tmpUrl := strList[i];

            if tmpResolution <> '' then
            tmpName := tmpName + ' - ' + tmpResolution + ' ' + IntToStr(StrToInt(tmpBitrate) div 1024) + ' kbps'
            else
            tmpName := tmpName + ' - ' + IntToStr(StrToInt(tmpBitrate) div 1024) + ' kbps';
            * }
          // s:= json.o['urls'].AsObject.GetNames.AsArray.ToString;
          for i := 0 to json.o['urls'].AsObject.GetNames.AsArray.Length - 1 do
          begin
            s := json.o['urls'].AsObject.GetNames.AsArray[i].AsString;
            quality := TQuality.Create;
            quality.resolution := json.o['urls'].AsObject.GetNames.AsArray
              [i].AsString;
            quality.name := quality.resolution;
            quality.url := json.o['urls'].AsObject.GetValues.AsArray[i]
              .AsString;;
            task.Comm.Send(MSG_URL_QUALITY, quality);
          end;
          // end;
          // end;

          task.Comm.Send(MSG_URL_DONE, '');

        except
          on E: Exception do
          begin
            task.Comm.Send(MSG_URL_ERROR, Format('[6] Error: %s', [E.Message]));
          end;
        end;
      finally
        FreeAndNil(strList);
        FreeAndNil(DataIn);
        // FreeAndNil(HttpCli1);
      end;
    end).OnMessage(
    procedure(const task: IOmniTaskControl; const msg: TOmniMessage)
    var
      quality: TQuality;
    begin
      case msg.MsgID of

        MSG_URL_ERROR:
          begin
            btnLoad.Enabled := True;
            btnLoad.Caption := '1. Load';
            MessageBox(0, PWideChar(msg.MsgData.AsString), 'Tardsplaya',
              MB_OK or MB_ICONEXCLAMATION);
          end;

        MSG_URL_QUALITY:
          begin
            quality := TQuality(msg.MsgData.AsObject);
            lstQuality.Items.AddObject(quality.name, quality);
          end;

        MSG_URL_DONE:
          begin
            btnLoad.Enabled := True;
            btnLoad.Caption := '1. Load';
            // lblClusterVal.Caption := msg.MsgData.AsString;
            lstQuality.Enabled := True;
          end;

      end;
    end).SetParameter('channel', channelName).Unobserved.Schedule;

end;

procedure TForm1.btnWatchClick(Sender: TObject);
var
  index: Integer;
begin
  FStreamUrl := TQuality(lstQuality.Items.Objects[lstQuality.ItemIndex]).url;

   edtChannel.Enabled := False;
   btnLoad.Enabled := False;
   lstQuality.Enabled := False;
   btnWatch.Enabled := False;

  index := FStreamUrl.LastIndexOf('/');
  FStreamPath := FStreamUrl.Substring(0, index + 1);

  if not InitWinsock then
  begin
    exit
  end;
  DoGetVideo;
end;

procedure TForm1.ApplicationEvents1Minimize(Sender: TObject);
begin
  if MinimizeToTray then
  begin
    Hide();
    WindowState := wsMinimized;
    TrayIcon1.Visible := True;
    TrayIcon1.ShowBalloonHint;
  end;

end;

procedure TForm1.btnAddFavoriteClick(Sender: TObject);
var
  value: string;
begin
  value := InputBox('Add Favorite', 'Channel', '');
  if value <> '' then
  begin
    lstFavorites.Items.Add(value);
    SaveFavorites();
  end;
  CheckFavorites();
end;

procedure TForm1.btnCheckVersionClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'https://github.com/Zero3K/tardsplaya/releases',
    nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.btnDecSectionsClick(Sender: TObject);
begin
  FSectionCount := FSectionCount - 1;
  if FSectionCount = 1 then
  begin
    btnDecSections.Enabled := False;
  end;
  btnIncSections.Enabled := True;
  lblSectionsVal.Caption := IntToStr(FSectionCount);
  WriteToLog('Section count changed to ' + lblSectionsVal.Caption, False);
end;

procedure TForm1.btnIncSectionsClick(Sender: TObject);
begin
  FSectionCount := FSectionCount + 1;
  if FSectionCount = 4 then
  begin
    btnIncSections.Enabled := False;
  end;
  btnDecSections.Enabled := True;
  lblSectionsVal.Caption := IntToStr(FSectionCount);
  WriteToLog('Section count changed to ' + lblSectionsVal.Caption, False);
end;

procedure TForm1.btnDeleteFavoriteClick(Sender: TObject);
begin
  if lstFavorites.ItemIndex > -1 then
  begin
    if AutoConfirmFavoriteDeletion then
    begin
      DeleteFavorite();
    end
    else
    begin
      if Application.MessageBox
        (PWideChar
        (Format('You sure you want to delete "%s" from your favorites?',
        [lstFavorites.Items[lstFavorites.ItemIndex]])), 'Delete Favorite',
        MB_YESNO + MB_ICONQUESTION) = IDYES then
      begin
        DeleteFavorite();
      end;
    end;
  end;
  CheckFavorites();
end;

procedure TForm1.btnEditFavoriteClick(Sender: TObject);
var
  value: string;
begin
  if lstFavorites.ItemIndex > -1 then
  begin
    value := InputBox('Edit Favorite', 'Channel',
      lstFavorites.Items[lstFavorites.ItemIndex]);
    if value <> '' then
    begin
      lstFavorites.Items[lstFavorites.ItemIndex] := value;
      SaveFavorites();
    end;
  end;
  CheckFavorites();
end;

procedure TForm1.DoGetVideo;
begin
  FHelloWorker := CreateTask(THelloWorker.Create(FStreamUrl))
    .OnMessage(TaskMessageProc)
    .OnTerminated(procedure
    begin
   edtChannel.Enabled := true;
   btnLoad.Enabled := true;
   lstQuality.Enabled := true;
   btnWatch.Enabled := true;
    end)
  // .SetParameter('ChannelName', channel.ToLower)
    .Unobserved.Schedule;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  ExitProcess(0);
end;

procedure SetFormIcons(FormHandle: HWND; SmallIconName, LargeIconName: string);
var
  hIconS, hIconL: Integer;
begin
  hIconS := LoadIcon(hInstance, PChar(SmallIconName));
  if hIconS > 0 then
  begin
    hIconS := SendMessage(FormHandle, WM_SETICON, ICON_SMALL, hIconS);
    if hIconS > 0 then
      DestroyIcon(hIconS);
  end;
  hIconL := LoadIcon(hInstance, PChar(LargeIconName));
  if hIconL > 0 then
  begin
    hIconL := SendMessage(FormHandle, WM_SETICON, ICON_BIG, hIconL);
    if hIconL > 0 then
      DestroyIcon(hIconL);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;

  FHttpCliPool := TThreadList<TNetHTTPClient>.Create;

  FStreamUrlQueue := TList<TStreamUrlQueueItem>.Create;
  GlobalOmniThreadPool.MaxExecuting := 12;
  SetFormIcons(Handle, 'MAINICON', 'MAINICON');
{$IFDEF DEBUG}
  edtChannel.Text := 'end0re';
{$ENDIF}
{$IFDEF DEBUG}
  chkLogOnlyErrors.Checked := False;
  chkAutoScroll.Checked := True;
{$ENDIF}
  defaultPlayerPath := 'MPC-HC\mpc-hc.exe';
  defaultPlayerCmd := '-';
  PlayerPath := defaultPlayerPath;
  PlayerCmd := defaultPlayerCmd;
  AutoConfirmFavoriteDeletion := False;
  GetSettings;

  // FSectionCount := 4;
  FSectionCount := 1;

end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
// var
// i: Integer;
begin
  Form2.SetSettings;
  TerminateProcess(FPlayerHandle, 0);
  ExitProcess(0);
  // GlobalOmniThreadPool.CancelAll;
  // WSACleanup();
  // for i := 0 to FStreamUrlQueue.Count - 1 do
  // FStreamUrlQueue[i].Free;
  // FStreamUrlQueue.Free;
  // FreeQualities();
end;

{ TExtInf }

function IsNumericString(const inStr: string): Boolean;
var
  i: extended;
begin
  Result := TryStrToFloat(inStr, i);
end;

constructor TExtInf.Create(durationStr, url: string);
var
  spl: string;
  index: Integer;
  fmt: TFormatSettings;
begin
  fmt := TFormatSettings.Create;
  fmt.DecimalSeparator := '.';
  spl := durationStr.Split([','])[0];
  index := AnsiPos('.', spl);
  if index > 0 then
    spl := spl.Substring(0, index + 1);
  Self.duration := StrToFloat(spl, fmt);
  Self.url := url;
end;

procedure TForm1.UpdateQueueText(q: Integer);
begin
  StatusBar1.Panels[0].Text := Format('Chunk Queue: %d', [q]);
end;

procedure DoCheckPlayer(const task: IOmniTask);
var
  FHandle: THandle;
begin
  FHandle := THandle(task.Param['handle']);
  while WaitForSingleObject(FHandle, 50) <> 0 do
  begin

  end;
  // ExitProcess(0);
  task.Comm.Send(MSG_PLAYER_EXIT);

end;

procedure SaveBufferToFile(fileName: string; Buffer: Pointer; Len: Int64);
var
  fs: TFileStream;
begin
  if FileExists(fileName) then
    DeleteFile(fileName);
  fs := TFileStream.Create(fileName, fmCreate or fmShareDenyWrite);
  try
    fs.WriteBuffer(Buffer^, Len);
  finally
    fs.Free;
  end;
end;

procedure TForm1.TaskMessageProc(const task: IOmniTaskControl;
const msg: TOmniMessage);

  procedure MsgProcCreateDlStreamTask(itm: TStreamUrlQueueItem;
  startIndex: Int64; endIndex: Int64);

    function CreateHttpCli(): TNetHTTPClient;
    var
      http: TNetHTTPClient;
    begin
      http := TNetHTTPClient.Create(Self);
      http.UserAgent := FOverrideUserAgent;
      http.Accept := '*/*';
      http.ConnectionTimeout := FOverrideConnectionTimeout;

      Result := http;
    end;

    function GetHttpCli(): TOmniValue;
    var
      pool: TList<TNetHTTPClient>;
      http: TNetHTTPClient;
    begin
      http := nil;
      try
        pool := FHttpCliPool.LockList;
        if (pool.Count > 0) then
        begin
          WriteToLog('Using existing http download client', False);
          http := pool.ExtractAt(pool.Count - 1);
        end;
      finally
        FHttpCliPool.UnlockList;
      end;

      if (http = nil) then
      begin
        WriteToLog('Creating new http download client', False);
        http := CreateHttpCli();
      end;

      http.Tag := itm.id;

      Result.AsObject := http;
    end;

  var
    chunk: TStreamChunk;
    httpcli: TOmniValue;
  begin
    chunk := TStreamChunk.Create;
    chunk.queueItem := itm;
    chunk.startIndex := startIndex;
    chunk.endIndex := endIndex;

    WriteToLog(Format('Create part for task %d', [itm.id]), False);

    httpcli := GetHttpCli();

    CreateTask(DoDlStream).OnMessage(TaskMessageProc).SetParameter('chunk',
      chunk).SetParameter('httpcli', httpcli).Unobserved.Schedule;
  end;

  procedure MsgProcCreateWriteStreamTask(itm: TStreamUrlQueueItem);
  begin
    WriteToLog(Format('Begin feeding chunk %d to player', [itm.id]), False);

    CreateTask(WriteStreamToPlayer).OnMessage(TaskMessageProc)
      .SetParameter('item', itm).Unobserved.Schedule;
  end;

var
  qitem: TStreamUrlQueueItem;
  chunk: TStreamChunk;
  idx: Integer;


begin
  // task.Param['ChannelName'].AsString;

  case msg.MsgID of

    MSG_PLAYER_HANDLE:
      begin
        FPlayerHandle := msg.MsgData.AsInt64;
        CreateTask(DoCheckPlayer).OnMessage(TaskMessageProc)
          .SetParameter('handle', FPlayerHandle).Unobserved.Schedule;
      end;

    MSG_PLAYER_EXIT:
      begin
        // ExitProcess(0);
        WriteToLog('Player was closed?', True);
        FHelloWorker.Stop;
        WriteToLog('[Check new chunk task] stopped', True);
//        btnWatch.Enabled := True;
      end;

    MSG_ERROR:
      begin
        WriteToLog(msg.MsgData.AsString, True);
        MessageBox(0, PWideChar(msg.MsgData.AsString), 'Tardsplaya',
          MB_OK or MB_ICONEXCLAMATION);
        task.Stop;
//        btnWatch.Enabled := true;
      end;

    MSG_LOG_ERROR:
      begin
        WriteToLog(msg.MsgData.AsString, True);
      end;

    MSG_LOG_DEBUG:
      begin
        // {$IFDEF DEBUG}
        WriteToLog(msg.MsgData.AsString, False);
        // {$ENDIF}
      end;

    MSG_STREAM:
      begin
        qitem := TStreamUrlQueueItem.Create;
        qitem.url := msg.MsgData.AsArray[0].AsString;
        if (not qitem.url.StartsWith('http', True)) then
          qitem.url := FStreamPath + qitem.url;
        qitem.id := msg.MsgData.AsArray[1].AsInteger;
        qitem.content := nil;
        qitem.contentLength := -1;
        qitem.writtenChunks := 0;
        qitem.totalChunks := FSectionCount;

        // Start download stream
        MsgProcCreateDlStreamTask(qitem, 0, FChunkSize);
        FStreamUrlQueue.Add(qitem);
        UpdateQueueText(FStreamUrlQueue.Count);

      end;

    MSG_STREAM_BEGIN_DOWNLOAD:
      begin
        // chunk := TStreamChunk(msg.MsgData.AsObject);
        //
        // chunkSize := FChunkSize; //chunk.queueItem.contentLength div chunk.queueItem.totalChunks;
        //
        // WriteToLog(Format('Beginning chunk %d download', [chunk.queueItem.id, chunkSize]), False);
        // for i := 1 to chunk.queueItem.totalChunks - 1 do
        // begin
        // MsgProcCreateDlStreamTask(chunk.queueItem, chunkSize * i, (chunkSize * i) + chunkSize);
        // end;
      end;

    MSG_STREAM_CHUNK_DOWNLOADED:
      begin
        chunk := TStreamChunk(msg.MsgData.AsObject);
        chunk.queueItem.writtenChunks := chunk.queueItem.writtenChunks + 1;

        WriteToLog(Format('All parts from chunk %d downloaded',
          [chunk.queueItem.id]), False);

        idx := FStreamUrlQueue.IndexOf(chunk.queueItem);
        if (not FWritingStream) and (idx = 0) then
        begin
          MsgProcCreateWriteStreamTask(chunk.queueItem);
          // {$IFDEF DEBUG}
          // SaveBufferToFile('stream.ts', chunk.queueItem.content, chunk.queueItem.contentLength);
          // {$ENDIF}
        end;

        chunk.Free;
      end;

    MSG_PLAYER_FINISH_WRITE:
      begin
        WriteToLog(Format('Finished feeding data (chunk %d) to player',
          [msg.MsgData.AsInteger]), False);
      end;
    MSG_STREAM_ENDED:
      begin
        FStreamUrlQueue.Delete(0);
        UpdateQueueText(FStreamUrlQueue.Count);
        WriteToLog(Format('Finished with chunk %d',
          [msg.MsgData.AsArray[0].AsInteger]), False);

        FHttpCliPool.Add(msg.MsgData.AsArray[1].AsObject as TNetHTTPClient);

        if FStreamUrlQueue.Count = 0 then
          FWritingStream := False
        else
        begin
          if FStreamUrlQueue[0].writtenChunks = FStreamUrlQueue[0].totalChunks
          then
            MsgProcCreateWriteStreamTask(FStreamUrlQueue[0]);
        end;

      end;

  end;

  // task.ClearTimer(1);
  // task.Stop;
end;

procedure TForm1.TrayIcon1DblClick(Sender: TObject);
begin
  TrayIcon1.Visible := False;
  Show();
  WindowState := wsNormal;
  Application.BringToFront();
end;

{ THelloWorker }

constructor THelloWorker.Create(const streamUrl: string);
begin
  inherited Create;
  FStreamUrl := streamUrl;

  DataIn := TMemoryStream.Create();
  // HttpCli1 := THttpCli.Create(nil);
  strList := TStringList.Create;
  extInfs := TList<TExtInf>.Create;

  Form1.httpcli1.UserAgent := Form1.FOverrideUserAgent;
  Form1.httpcli1.Accept := '*/*';
  Form1.httpcli1.ConnectionTimeout := Form1.FOverrideConnectionTimeout;
end;

destructor THelloWorker.Destroy;
begin
  // FreeAndNil(HttpCli1);
  FreeAndNil(extInfs);
  FreeAndNil(strList);
  FreeAndNil(DataIn);

  inherited;
end;

function THelloWorker.Initialize: Boolean;
var
  mpchcHandle: THandle;
begin
  Result := True;
  if IsRelativePath(PlayerPath) then
    mpchcHandle := CreateMPCHC(ExtractFilePath(ParamStr(0)) + PlayerPath,
      PlayerCmd)
  else
    mpchcHandle := CreateMPCHC(PlayerPath, PlayerCmd);

  if mpchcHandle = 0 then
  begin
    task.Comm.Send(MSG_ERROR, 'Error: failed to create player');
    exit;
  end;

  task.Comm.Send(MSG_PLAYER_HANDLE, mpchcHandle);

  FLastTime := 0;

  FExtMediaSequence := 0;

  task.SetTimer(1, 1, MSG_START_WORK);
end;

procedure THelloWorker.StartWork(var msg: TMessage);
var
  i: Integer;
  TmpExtMediaSequence: Integer;
  errorMsg: string;
  ftime: Int64;
  response: IHTTPResponse;
begin
  task.ClearTimer(1);

  errorMsg := '';
  try
    try
      DataIn.Clear;
{$IFDEF DEBUG}
      Form1.WriteToLog(Format('Getting playlist', []), False);
{$ENDIF}
      try
        response := Form1.httpcli1.Get(FStreamUrl, DataIn);
{$IFDEF DEBUG}
        Form1.WriteToLog(Format('Playlist response %d "%s"',
          [response.StatusCode, response.StatusText]), False);
{$ENDIF}
      except
        on E: Exception do
        begin
          errorMsg := Format('StartWork() [1] Error: %s', [E.Message]);
          exit;
        end;
      end;

      strList.Text := DataInToString(DataIn);
      if not strList[0].StartsWith('#EXTM3U') then
      begin
        errorMsg := 'StartWork() [2] Error: no media info found';
        exit;
      end;

      for i := 0 to strList.Count - 1 do
      begin
        if strList[i].StartsWith('#EXT-X-MEDIA-SEQUENCE:') then
        begin
          TmpExtMediaSequence := StrToInt(strList[i].Substring(22));
          if TmpExtMediaSequence = FExtMediaSequence then
            exit
          else
            FExtMediaSequence := TmpExtMediaSequence;
        end
        else if strList[i].StartsWith('#EXTINF:') then
        begin
          extInfs.Add(TExtInf.Create(strList[i].Substring(8), strList[i + 1]));
        end;
      end;

      task.Comm.Send(MSG_STREAM, [extInfs[(extInfs.Count - 1)].url,
        FExtMediaSequence]);
      // errorMsg := 'stream found';

    except
      on E: Exception do
      begin
        errorMsg := Format('StartWork() [3] Error: %s', [E.Message]);
        exit;
      end;
    end;

  finally
    for i := 0 to extInfs.Count - 1 do
    begin
      extInfs[i].Free;
    end;
    extInfs.Clear;

    if errorMsg = '' then
    begin
      ftime := MillisecondsBetween(Now, FLastTime);
      FLastTime := Now;
      if ftime > 500 then
      begin
        task.SetTimer(1, 1, MSG_START_WORK);
      end
      else
      begin
        task.SetTimer(1, 500 - ftime, MSG_START_WORK);
      end;
    end
    else
    begin
      task.Comm.Send(MSG_LOG_ERROR, errorMsg);
      task.SetTimer(1, 1, MSG_START_WORK);
    end;
  end;

end;

procedure TForm1.CheckFavorites;
begin
  if lstFavorites.ItemIndex > -1 then
  begin
    btnDeleteFavorite.Enabled := True;
    btnEditFavorite.Enabled := True;
  end
  else
  begin
    btnDeleteFavorite.Enabled := False;
    btnEditFavorite.Enabled := False;
  end;
end;

procedure TForm1.Clear1Click(Sender: TObject);
begin
  lvLog.Clear;
end;

procedure TForm1.SaveFavorites;
begin
  lstFavorites.Items.SaveToFile(ExtractFilePath(ParamStr(0)) + 'favorites.txt');
end;

procedure TForm1.Settings1Click(Sender: TObject);
begin
  Form2 := TForm2.Create(Self);
  try
    Form2.ShowModal();
  finally
    Form2.Release;
  end;
end;

procedure TForm1.DeleteFavorite();
begin
  lstFavorites.Items.Delete(lstFavorites.ItemIndex);
  SaveFavorites();
end;

end.

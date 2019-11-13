{ 
  Desenvolvida por: Douglas Colombo
  Data: 13.11.2019
  Versão: 1.0.2.1
}

unit AdMob;

interface

uses
  {$IFDEF ANDROID}
  Androidapi.JNI.AdMob, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Widget, Androidapi.JNI.Location, Androidapi.JNI.App,
  Androidapi.JNI.Util, Androidapi.helpers, FMX.helpers.android, FMX.Platform.Android, Androidapi.JNI.Embarcadero, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.PlayServices,
  {$ENDIF}
  System.Types, System.UITypes, System.Classes, System.SysUtils, FMX.Dialogs, FMX.Advertising, FMX.Types;

type
  TAdBannerType = (Banner, Interstitial, Rewarded);
  TOnClosed = procedure (bannerType : TAdBannerType) of object;

  {$IFDEF ANDROID}
  TMyAdViewListener = class(TJavaLocal, JIAdListener)
  private
    FAD: JInterstitialAd;
    FisShown: Boolean;
    FTestMode: Boolean;
    FOnClosed: TOnClosed;
    FBannerType : TAdBannerType;

    procedure SetOnClosed(const Value: TOnClosed);  public
    property isShown : Boolean read FisShown;
    property Testmode : Boolean read FTestMode write FTestMode;
    destructor Destroy;
    constructor Create(AAD: JInterstitialAd);
  public
    property onBannerClosed : TOnClosed read FOnClosed write SetOnClosed;
    property BannerType : TAdBannerType read FBannerType write FBannerType;
    procedure onAdClosed; cdecl;
    procedure onAdFailedToLoad(errorCode: Integer); cdecl;
    procedure onAdLeftApplication; cdecl;
    procedure onAdOpened; cdecl;
    procedure onAdLoaded; cdecl;
  end;
  {$ENDIF}

  TAdBanner = class
  private
    FTestMode : Boolean;
    FAowner : TFmxObject;
    FBannerType : TAdBannerType;
    FAdBanner: TBannerAd;

    {InterStitial}
    {$IFDEF ANDROID}
      LAdViewListener: TMyAdViewListener;
      FAdMob : JInterstitialAd;
      LADRequestBuilder: JAdRequest_Builder;
      LadRequest: JAdRequest;
    {$ENDIF}
    FisShown: Boolean;
    FOnClosed : TOnClosed;

    procedure BannerAdDidFail(Sender: TObject; const Error: string);
    procedure BannerAdDidLoad(Sender: TObject);
    procedure SetOnClosed(const Value: TonClosed);
  public
    procedure setParent(AOwner : TFmxObject);
    constructor Create(AOWner : TFmxObject; AdUnitId : String; BannerType : TAdBannerType);
    destructor Destroy;

    property onBannerClosed : TOnClosed read FOnClosed write SetOnClosed;
    property BannerType : TAdBannerType read FBannerType;
    property Testmode : Boolean read FTestMode write FTestMode;
    property isShown : Boolean read FisShown;
    procedure show(testMode : Boolean = true; Aligment : TAlignLayout = TAlignLayout.Top);
    procedure hide;
  end;

implementation

procedure RaiseException(testMode : Boolean; MessageError : String);
begin
   if (testMode) then
   begin
      Log.D('RaiseException: '+ MessageError);
      TThread.Synchronize(TThread.CurrentThread, procedure
         begin
            Try
               if (MessageError <> '') then
                  showMessage( MessageError );
            Except end;
         end);
   end;
end;

{ TMyAdViewListener }

{$IFDEF ANDROID}
constructor TMyAdViewListener.Create(AAD: JInterstitialAd);
begin
  Try
     inherited Create;
     FAD := AAD;
     FisShown  := false;
     FTestMode := false;
  Except
     RaiseException(Testmode, 'Sem Conexão de internet');
  end;
end;

destructor TMyAdViewListener.Destroy;
begin
  FAD := nil;
end;

procedure TMyAdViewListener.onAdClosed;
begin
  FisShown := false;

  TThread.Synchronize(nil, procedure
      begin
          if (Assigned(FOnClosed)) then
             FOnClosed(BannerType);
      end);
end;

procedure TMyAdViewListener.onAdFailedToLoad(errorCode: Integer);
begin
  FisShown := false;

  Log.D('Intersititial/Reward-FailedToLoad = '+ ErrorCode.ToString);
  if (errorCode = 0) then
     RaiseException(FTestMode, 'AdUnitId inválida.'+ sLineBreak+ 'ERROR_CODE_INTERNAL_ERROR' +sLineBreak+ 'Código de Erro: '+ ErrorCode.toString)
  else if (errorCode = 1) then
     RaiseException(FTestMode, 'AdUnitId inválida.'+ sLineBreak+ 'ERROR_CODE_INVALID_REQUEST' +sLineBreak+ 'Código de Erro: '+ ErrorCode.toString)
  else if (errorCode = 2) then
     RaiseException(FTestMode, 'AdUnitId válida, tempo de espera esgotado.'+ sLineBreak+ 'ERROR_CODE_NETWORK_ERROR' +sLineBreak+ 'Código de Erro: '+ ErrorCode.toString)
  else if (errorCode = 3) then {ESSE ERRO INFORMA QUE A COMUNICAÇÃO ESTÁ CORRETA COM O ADMOB, SÓ AGUARDAR AGORA ATÉ SUA CONTA SER ATIVADA}
     RaiseException(FTestMode, 'AdUnitId válida. Aguarde até 24 horas, após a criação da promoção, para ser validada.'+ sLineBreak+ 'ERROR_CODE_NO_FILL' +sLineBreak+ 'Código de Erro: '+ ErrorCode.toString);
end;

procedure TMyAdViewListener.onAdLeftApplication;
begin

end;

procedure TMyAdViewListener.onAdLoaded;
begin
  Try
     FAD.show;
     FisShown := true;
     Log.D('Intersititial/Reward-Load = Sucess');
  Except on E : Exception do
     RaiseException(FTestMode, 'onAdLoaded error: '+ E.Message);
  End;
end;

procedure TMyAdViewListener.onAdOpened;
begin

end;

procedure TMyAdViewListener.SetOnClosed(const Value: TOnClosed);
begin
  FOnClosed := Value;
end;

{$ENDIF}

{ TAdBanner }

procedure TAdBanner.BannerAdDidFail(Sender: TObject; const Error: string);
begin
  Log.D('BannerAdDidFail = '+ Error);
  FisShown := false;
  if (Error.Contains('0')) then
     RaiseException(FTestMode, 'AdUnitId inválida.'+ sLineBreak+ 'ERROR_CODE_INTERNAL_ERROR' +sLineBreak+ 'Mensagem: '+ Error)
  else if (Error.Contains('1')) then
     RaiseException(FTestMode, 'AdUnitId inválida.'+ sLineBreak+ 'ERROR_CODE_INVALID_REQUEST' +sLineBreak+ 'Mensagem: '+ Error)
  else if (Error.Contains('2')) then
     RaiseException(FTestMode, 'AdUnitId válida, tempo de espera esgotado.'+ sLineBreak+ 'ERROR_CODE_NETWORK_ERROR' +sLineBreak+ 'Mensagem: '+ Error)
  else if (Error.Contains('3')) then
     RaiseException(FTestMode, 'AdUnitId válida. Aguarde até 24 horas, após a criação da promoção, para ser validada.' +sLineBreak+ 'ERROR_CODE_NO_FILL' +sLineBreak+ 'Mensagem: '+ Error);
end;

procedure TAdBanner.BannerAdDidLoad(Sender: TObject);
begin
  FisShown := true;
  Log.D('BannerAdDidLoad = Sucess');
end;

constructor TAdBanner.Create(AOWner : TFmxObject; AdUnitId: String; BannerType: TAdBannerType);
begin
  Log.D('Create: 01');
  FisShown     := false;
  FTestMode    := false;
  FAowner      := AOWner;
  FBannerType  := BannerType;
  if (BannerType = TAdBannerType.Banner) then
  begin
     Log.D('Create Banner: 02');
     FAdBanner := TBannerAd.Create(AOWner);
     FAdBanner.Parent    := FAowner;
     FAdBanner.AdUnitID  := AdUnitId;
     FAdBanner.OnDidFail := BannerAdDidFail;
     FAdBanner.OnDidLoad := BannerAdDidLoad;
     Log.D('Create Banner: 03');
  end else if (BannerType = TAdBannerType.Interstitial) then
  begin
    Log.D('Create Interstitial: 04');
    {$IFDEF ANDROID}
       FAdMob := TJInterstitialAd.JavaClass.init(MainActivity);
       FAdMob.setAdUnitId(StringToJString( AdUnitId )); {Interstitial}
    {$ENDIF} {$IFDEF IOS}
       RaiseException(true, 'Desculpe, o banner "Interstitial" não foi implementado para IOS até o momento.');
    {$ENDIF}
    Log.D('Create Interstitial: 05');
  end else if (BannerType = TAdBannerType.Rewarded) then
  begin
    Log.D('Create Rewarded: 06');
    {$IFDEF ANDROID}
       FAdMob := TJInterstitialAd.JavaClass.init(MainActivity);
       FAdMob.setAdUnitId(StringToJString( AdUnitId )); {Interstitial}
    {$ENDIF} {$IFDEF IOS}
       RaiseException(true, 'Desculpe, o banner "Rewarded" não foi implementado para IOS até o momento.');
    {$ENDIF}
    Log.D('Create Rewarded: 07');
  end;

  Log.D('Create: 08');
end;

destructor TAdBanner.Destroy;
begin
{$IFDEF ANDROID}
  if (assigned(LAdViewListener)) then
  begin
     Try
       LAdViewListener.DisposeOf;
       LAdViewListener := nil;
     Except end;
  end;

  if (assigned(FAdBanner)) then
  begin
     Try
       FAdBanner.DisposeOf;
       FAdBanner := nil;
     Except end;
  end;

  if (assigned(LADRequestBuilder)) then
     LADRequestBuilder := nil;
  if (assigned(LadRequest)) then
     LadRequest := nil;
  if (assigned(FAdMob)) then
     FAdMob := nil;
{$ENDIF}
end;

procedure TAdBanner.hide;
begin
  Log.D('hide: 01');
  if (FBannerType = TAdBannerType.Banner) then
  begin
     FAdBanner.Hide;
     FAdBanner.Visible := false;
     FisShown := false;
  end else if (FBannerType = TAdBannerType.Banner) then
  begin
     {$IFDEF ANDROID}
     FisShown := LAdViewListener.isShown;
     {$ENDIF}
  end;
  Log.D('hide: 02');
end;

procedure TAdBanner.SetOnClosed(const Value: TonClosed);
begin
  FOnClosed := Value;
end;

procedure TAdBanner.setParent(AOwner: TFmxObject);
begin
  FAowner := AOwner;
  if (FBannerType = TAdBannerType.Banner) then
     FAdBanner.Parent := FAowner
  else if (FBannerType = TAdBannerType.Interstitial) or (FBannerType = TAdBannerType.Rewarded) then
  begin
     {$IFDEF ANDROID}
     FisShown := LAdViewListener.isShown;
     {$ENDIF}
  end;
end;

procedure TAdBanner.show(testMode: Boolean; Aligment : TAlignLayout);
begin
  Log.D('show: 01');

  FTestMode := testMode;

  if (FBannerType = TAdBannerType.Interstitial) or (FBannerType = TAdBannerType.Rewarded) then
  begin
     Log.D('show Interstitial/Rewarded: 02');
     {$IFDEF ANDROID}
         Try
             LADRequestBuilder := TJAdRequest_Builder.Create;
             if (testMode) then
                LADRequestBuilder.addTestDevice(MainActivity.getDeviceID);
             LadRequest                     := LADRequestBuilder.build();
             LAdViewListener                := TMyAdViewListener.Create(FAdMob);
             LAdViewListener.Testmode       := FTestMode;
             LAdViewListener.BannerType     := BannerType;
             LAdViewListener.onBannerClosed := onBannerClosed;
             CallInUIThread(
               procedure
               begin
                 Try
                   FAdMob.setAdListener(TJAdListenerAdapter.JavaClass.init(LAdViewListener));
                   FAdMob.loadAd(LadRequest);
                   self.FisShown := LAdViewListener.isShown;
                 Except end;
               end);
         Except end;
     {$ENDIF}
     Log.D('show Interstitial/Rewarded: 03');
  end else if (FBannerType = TAdBannerType.Banner) then
  begin
     Log.D('show Banner: 04');
     FAdBanner.AdSize   := TBannerAdSize.Auto;
     FAdBanner.Enabled  := true;
     FAdBanner.Align    := Aligment;
     FAdBanner.TestMode := testMode;
     FAdBanner.Visible  := true;
     if not (FAdBanner.IsLoaded) then
        FAdBanner.LoadAd
     else
        FAdBanner.Show;
     FisShown           := true;
     Log.D('show Banner: 05');
  end;
  Log.D('show: 06');
end;

end.

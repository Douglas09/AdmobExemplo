unit UPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  Admob, FMX.StdCtrls, FMX.Objects;

type
  TForm2 = class(TForm)
    btnBanner: TButton;
    btnIntersticial: TButton;
    btnPremiado: TButton;
    btnAberturaApp: TButton;
    procedure btnBannerClick(Sender: TObject);
    procedure btnIntersticialClick(Sender: TObject);
    procedure btnPremiadoClick(Sender: TObject);
    procedure btnAberturaAppClick(Sender: TObject);
  private
    procedure aoFecharAnuncio(bannerTipo : TAdBannerType);
  public
    banner : TAdBanner;
    intersticial : TAdBanner;
    premiado : TAdBanner;
    aberturaApp : TAdBanner;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.aoFecharAnuncio(bannerTipo: TAdBannerType);
begin
  btnPremiado.TintColor := TAlphaColorRec.Red;
end;

procedure TForm2.btnAberturaAppClick(Sender: TObject);
begin
  aberturaApp := TAdBanner.Create(Form2, 'ca-app-pub-3940256099942544/3419835294', TAdBannerType.AppOpen);
  aberturaApp.show(true);
end;

procedure TForm2.btnBannerClick(Sender: TObject);
begin
  banner := TAdBanner.Create(Form2, 'ca-app-pub-3940256099942544/6300978111', TAdBannerType.Banner);
  banner.show(true, TAlignLayout.MostBottom);
end;

procedure TForm2.btnIntersticialClick(Sender: TObject);
begin
  intersticial := TAdBanner.Create(Form2, 'ca-app-pub-3940256099942544/1033173712', TAdBannerType.Interstitial);
  intersticial.show(true);
end;

procedure TForm2.btnPremiadoClick(Sender: TObject);
begin
  premiado := TAdBanner.Create(Form2, 'ca-app-pub-3940256099942544/5224354917', TAdBannerType.Rewarded);
  premiado.onBannerClosed := aoFecharAnuncio;
  premiado.show(true);
end;

end.

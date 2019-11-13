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
    procedure btnBannerClick(Sender: TObject);
    procedure btnIntersticialClick(Sender: TObject);
    procedure btnPremiadoClick(Sender: TObject);
  private
    procedure aoFecharAnuncio(bannerTipo : TAdBannerType);
  public
    banner : TAdBanner;
    intersticial : TAdBanner;
    premiado : TAdBanner;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.aoFecharAnuncio(bannerTipo: TAdBannerType);
begin
  btnPremiado.TintColor := TAlphaColorRec.Red;
end;

procedure TForm2.btnBannerClick(Sender: TObject);
begin
  banner := TAdBanner.Create(self, 'ca-app-pub-3940256099942544/6300978111', TAdBannerType.Banner);
  banner.show(true, TAlignLayout.MostBottom);
end;

procedure TForm2.btnIntersticialClick(Sender: TObject);
begin
  intersticial := TAdBanner.Create(self, 'ca-app-pub-3940256099942544/1033173712', TAdBannerType.Interstitial);
  intersticial.show(true);
end;

procedure TForm2.btnPremiadoClick(Sender: TObject);
begin
  premiado := TAdBanner.Create(self, 'ca-app-pub-3940256099942544/5224354917', TAdBannerType.Rewarded);
  premiado.onBannerClosed := aoFecharAnuncio;
  premiado.show(true);
end;

end.

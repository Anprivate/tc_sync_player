unit UnitEntry;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.Buttons;

type
  TFormEntry = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    MaskEdit1: TMaskEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEntry: TFormEntry;

implementation

{$R *.dfm}

end.

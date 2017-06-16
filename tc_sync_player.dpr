program tc_sync_player;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormTCreader},
  UnitChannel in 'UnitChannel.pas',
  UnitClip in 'UnitClip.pas',
  UnitEntry in 'UnitEntry.pas' {FormEntry},
  UnitTelnet in 'UnitTelnet.pas',
  DeckLink in 'DeckLink.pas',
  reader_unit in 'reader_unit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTCreader, FormTCreader);
  Application.CreateForm(TFormEntry, FormEntry);
  Application.Run;
end.

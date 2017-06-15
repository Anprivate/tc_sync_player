program tc_sync_player;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FormTCreader},
  reader_thread in 'reader_thread.pas',
  UnitChannel in 'UnitChannel.pas',
  UnitClip in 'UnitClip.pas',
  UnitEntry in 'UnitEntry.pas' {FormEntry},
  UnitTelnet in 'UnitTelnet.pas',
  DeckLink in 'DeckLink.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTCreader, FormTCreader);
  Application.CreateForm(TFormEntry, FormEntry);
  Application.Run;
end.

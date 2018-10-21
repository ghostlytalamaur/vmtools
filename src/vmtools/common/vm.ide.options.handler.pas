unit vm.ide.options.handler;

interface

uses
  forms, vmsys, toolsapi;

type
  TVMOptionsHandler = class(TExtInterfacedObject, INTAAddInOptions)
  private
    FCaption: string;
    FFrameClass: TCustomFrameClass;

  protected
    { INTAAddInOptions }
    { Indicates where this option page should appear in the treeview in the
      Tools | Options dialog.  If this function returns an empty string, this
      page will appear under the Third Party area.  It is strongly suggested
      that you return an empty string from this function. }
    function GetArea: string;
    { Indicates the name of the node that should appear in the treeview in the
      Tools | Options dialog.  This node will appear under the node specified by
      "GetArea".  To nest multiple levels below the Area, include a dot in the
      Caption.  For instance if you return a blank string from GetArea and you
      return "MyAddin.MyOptions" from GetCaption, your options page will
      appear as follows in the treeview:
      Third Party
      |--MyAddin
         |--MyOptions
    }
    function GetCaption: string;
    { Returns the class of the frame that you want embedded in this options page }
    function GetFrameClass: TCustomFrameClass;
    { Called when the instance of the specified frame class is created }
    procedure FrameCreated(AFrame: TCustomFrame); virtual;
    { Called when the user closes the Options dialog that contains this page.
      The "Accepted" parameter is True if the user clicked OK, or False if the
      user clicked Cancel }
    procedure DialogClosed(Accepted: Boolean); virtual;
    { Called before the dialog is closed. Allows you to validate the input on
      your option page.  If there is invalid input, you should display an error
      message and return False.  Return True if there are no errors }
    function ValidateContents: Boolean; virtual;
    { Return the Help Context for this options page }
    function GetHelpContext: Integer;
    { Indicates whether or not this page will be automatically included in IDE
      Insight.  If True, it will be included in the "Preferences" node like all
      built-in pages from the Tools | Options dialog.  It is recommended that
      you return True. }
    function IncludeInIDEInsight: Boolean;
  public
    constructor Create(aCaption: string; aFrameClass: TCustomFrameClass);
  end;

implementation

{ TVMOptionsHandler }

constructor TVMOptionsHandler.Create(aCaption: string; aFrameClass: TCustomFrameClass);
begin
  inherited Create;
  FCaption := aCaption;
  FFrameClass := aFrameClass;
end;

procedure TVMOptionsHandler.DialogClosed(Accepted: Boolean);
begin
end;

procedure TVMOptionsHandler.FrameCreated(AFrame: TCustomFrame);
begin
end;

function TVMOptionsHandler.GetArea: string;
begin
  Result := '';
end;

function TVMOptionsHandler.GetCaption: string;
begin
  Result := 'VMTools.' + FCaption;
end;

function TVMOptionsHandler.GetFrameClass: TCustomFrameClass;
begin
  Result := FFrameClass;
end;

function TVMOptionsHandler.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TVMOptionsHandler.IncludeInIDEInsight: Boolean;
begin
  Result := True;
end;

function TVMOptionsHandler.ValidateContents: Boolean;
begin
  Result := True;
end;

end.

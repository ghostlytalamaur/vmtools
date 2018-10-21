unit vm_custom_dock_frm;

{$I cond_define.inc}

interface

uses
  vmsys, toolsapi, menus, ActnList, forms, imglist, comctrls, inifiles, designintf;

type
  IVMCustomDockForm = interface(INTACustomDockableForm)
  ['{E42C45DC-1D70-4C42-AA75-068992D0CE54}']
    function GetMainFrame: TCustomFrame;
  end;

  TVMCustomDockForm = class(TInterfacedObject, IVMCustomDockForm, INTACustomDockableForm)
  private
    FMainFrameClass: TCustomFrameClass;
    FCaption: string;
    FIdentifier: string;
    FMainFrame: IObjectHolder<TCustomFrame>;
  public
    constructor Create(aFrameClass: TCustomFrameClass; const aCaption, aIdentifier: string);
    { Returns the Caption for the Dockable Form }
    function GetCaption: string; virtual;
    { Returns a unique identifier for this form.  This should not be translated.
      This identifier is used as the section name when saving information for
      this form in the desktop state file }
    function GetIdentifier: string; virtual;
    { Returns the class of the frame that you want embedded in the dockable form }
    function GetFrameClass: TCustomFrameClass; virtual;
    { Called when an instance of the specified frame class is created }
    procedure FrameCreated(AFrame: TCustomFrame); virtual;
    { Returns an action list that is used to populate the form's context menu.
      By default the context menu will have 2 items that are common to all
      dockable forms in the IDE: "Stay on top" and "Dockable".  If the form
      has a toolbar, there will also be a "Toolbar" menu item.  If this
      function returns a non-nil action list, the items in the action list will
      be added to the menu (above the default items).  To specify sub-menus, use
      categories for the actions contained in the Action List.  Any action that
      has a Category set, will appear on a sub-menu in the context menu.  The
      Caption of the Parent menu will be the Category name. }
    function GetMenuActionList: TCustomActionList;
    { Returns an image list that contains the images associated with the action
      list returned by GetMenuActionList }
    function GetMenuImageList: TCustomImageList;
    { Called when the popup menu is about to be shown.  This allows further
      customization beyond just adding items from an Action List }
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu);
    { Returns an action list that is used to populate a toolbar on the form.  If
      nil is returned, then the dockable form will not have a toolbar.  Items in
      the Action List that have '-' as the caption will be added to the toolbar
      as a separator }
    function GetToolBarActionList: TCustomActionList;
    { Returns an image list that contains the images associated with the action
      list returned by GetToolbarActionList }
    function GetToolBarImageList: TCustomImageList;
    { Called after the toolbar has been populated with the Action List returned
      from GetToolbarActionList.  This allows further customization beyond just
      adding items from an Action List }
    procedure CustomizeToolBar(ToolBar: TToolBar);
    { Called when state for this form is saved to a desktop file.  The Section
      paramter is passed in for convenience, but it should match the string
      returned by GetIdentifier.  This is only called for INTACustomDockableForm
      instances that have been registered using INTAServices.RegisterDockableForm.
      IsProject indicates whether the desktop being saved is a project desktop
      (as opposed to a dekstop state) }
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean); virtual;
    { Called when state for this form is loaded from a desktop file.  The
      Section paramter is passed in for convenience, but it should match the
      string returned by GetIdentifier.  This is only called for
      INTACustomDockableForm instances that have been registered using
      INTAServices.RegisterDockableForm }
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string); virtual;
    { Allows the form to control the enabled state of the clipboard commands on
      the IDE's "Edit" menu when this view is active }
    function GetEditState: TEditState;
    { Called when the user uses one of the clipboard commands on the IDE's "Edit"
      menu }
    function EditAction(Action: TEditAction): Boolean;

    function GetMainFrame: TCustomFrame;
  end;
implementation

{ TSearchDockForm }

constructor TVMCustomDockForm.Create(aFrameClass: TCustomFrameClass; const aCaption, aIdentifier: string);
begin
  inherited Create;
  FMainFrameClass := aFrameClass;
  FCaption := aCaption;
  FIdentifier := aIdentifier;
end;

procedure TVMCustomDockForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
end;

procedure TVMCustomDockForm.CustomizeToolBar(ToolBar: TToolBar);
begin
end;

function TVMCustomDockForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TVMCustomDockForm.FrameCreated(AFrame: TCustomFrame);
begin
  FMainFrame := TObjectHolder<TCustomFrame>.Create(aFrame, False);
end;

function TVMCustomDockForm.GetCaption: string;
begin
  Result := FCaption;
end;

function TVMCustomDockForm.GetEditState: TEditState;
begin
  Result := [];
end;

function TVMCustomDockForm.GetFrameClass: TCustomFrameClass;
begin
  Result := FMainFrameClass;
end;

function TVMCustomDockForm.GetIdentifier: string;
begin
  Result := FIdentifier
end;

function TVMCustomDockForm.GetMainFrame: TCustomFrame;
begin
  if (FMainFrame <> nil) and FMainFrame.IsAlive then
    Result := FMainFrame.Obj
  else
    Result := nil;
end;

function TVMCustomDockForm.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TVMCustomDockForm.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

function TVMCustomDockForm.GetToolBarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TVMCustomDockForm.GetToolBarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TVMCustomDockForm.LoadWindowState(Desktop: TCustomIniFile; const Section: string);
begin
end;

procedure TVMCustomDockForm.SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
begin
end;

end.

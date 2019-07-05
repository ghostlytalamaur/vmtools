{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2019 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnTree;
{* |<PRE>
================================================================================
* ������ƣ�CnPack ������Ԫ
* ��Ԫ���ƣ�ʵ�ֵ��������������������ֵ����������൥Ԫ
* ��Ԫ���ߣ���Х (liuxiao@cnpack.org)
* ��    ע���õ�ԪΪ TCnTree �� TCnLeaf �ĵ�����������ʵ�ֵ�Ԫ���Լ�������
*           ������ TCnBinaryTree/Leaf���ֵ������� TCnTrieTree/Leaf��
*           TCnTree/Leaf ������ TTreeNodes/Node �Ĺ�ϵ��֧����Ⱥ͹�����ȱ�����
*           ֧�ְ�������ȵ�˳��������ֵ����ʽֱ�ӷ��ʸ����ڵ㡣
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6 + 10.3.1
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.04.17 V1.8 by LiuXiao
*               ֧�� Win32/Win64/MacOS��֧�� VCL �� FMX �µ� TreeView ������
*           2015.05.30 V1.7 by LiuXiao
*               �ֵ������� Ansi ���ٲ���ģʽ��
*           2015.05.22 V1.6 by LiuXiao
*               �����ֵ�����ʵ�֡�
*           2015.05.03 V1.5 by LiuXiao
*               �����������ʵ�֡�
*           2015.03.16 V1.4 by LiuXiao
*               ����������ȱ����Ĵ��󣬽� Root �� Level �ĳ� 0��
*           2005.05.08 V1.3 by Alan
*               ���� LoadFromTreeView �������� Clear ����δ���� RootLeaf �����Ĵ���
*           2004.11.02 V1.2
*               ���������Ľӿ�
*           2004.09.04 V1.1
*               ����� TreeView �����Ĺ���
*           2004.05.29 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs {$IFDEF MSWINDOWS}, ComCtrls {$ENDIF}
  {$IFDEF SUPPORT_FMX}, FMX.TreeView {$ENDIF}, Math;
  // If ComCtrls not found, please add 'Vcl' to 'Unit Scope Names' in Project Options.

type

//==============================================================================
// ���Ļ��࣬������������ʵ��
//==============================================================================

  ECnTreeException = class(Exception);

  TCnTree = class;

  TCnLeaf = class(TPersistent)
  {* ��Ҷ����}
  private
    FData: Integer;
    FList: TList;
    FParent: TCnLeaf;
    FText: string;
    FTree: TCnTree;
    FObj: TObject;
    function GetTree: TCnTree;
    function GetAllNonNilCount: Integer;
    function GetSubTreeHeight: Integer; virtual;
  protected
    function GetAbsoluteIndex: Integer;
    function GetAllCount: Integer;
    function GetCount: Integer;
    function GetHasChildren: Boolean;
    function GetIndex: Integer;
    function GetItems(Index: Integer): TCnLeaf;
    procedure SetItems(Index: Integer; const Value: TCnLeaf);
    function GetLevel: Integer;

    procedure AssignTo(Dest: TPersistent); override;
    procedure DoDepthFirstTravel;
    procedure DoWidthFirstTravel;
    function SetChild(ALeaf: TCnLeaf; Index: Integer): TCnLeaf;
    {* ��ĳ�ڵ㸳ֵΪ�� Index ���ӽڵ㣬����ԭ�ڵ�ֵ }
    property AllNonNilCount: Integer read GetAllNonNilCount;
    {* ���з� nil ������ڵ���Ŀ }
  public
    constructor Create(ATree: TCnTree); virtual;
    {* ���췽������Ҫһ Tree ������ }
    destructor Destroy; override;
    {* �������� }
    function AddChild(ALeaf: TCnLeaf): TCnLeaf;
    {* ���һָ�����ӽڵ���Ϊ����ֱ���ӽڵ� }
    function AddChildFirst(ALeaf: TCnLeaf): TCnLeaf;
    {* ���һָ�����ӽڵ���Ϊ��һֱ���ӽڵ� }
    function InsertChild(ALeaf: TCnLeaf; AIndex: Integer): TCnLeaf;
    {* ��ָ����������������һ�ӽڵ� }
    procedure Clear; virtual;
    {* �������ֱ���ӽڵ㣬�ӽڵ�����Ҳ�ᱻɾ���ͷ�  }
    procedure DeleteChild(AIndex: Integer); virtual;
    {* ɾ��һֱ���ӽڵ㣬�ӽڵ����»ᱻɾ���ͷ� }
    procedure Delete;
    {* ɾ��������ӽڵ� }
    function ExtractChild(AIndex: Integer): TCnLeaf; overload;
    {* �����һֱ���ӽڵ㣬�ӽڵ����²��ᱻɾ���ͷ� }
    function ExtractChild(ALeaf: TCnLeaf): TCnLeaf; overload;
    {* �����ָ����һ�ӽڵ㣬�ӽڵ����²��ᱻɾ���ͷ� }

    // ��������ڵ�ķ���
    function GetFirstChild: TCnLeaf;
    {* ��õ�һ��ֱ���ӽڵ� }
    function GetLastChild: TCnLeaf;
    {* ������һ��ֱ���ӽڵ� }
    function GetNext: TCnLeaf;
    {* ��õ�һ�ӽڵ㣬���ޣ��򷵻�ͬ���ڵ�ĺ�һ���ڵ㣬���ޣ��򷵻� nil }
    function GetNextChild(Value: TCnLeaf): TCnLeaf;
    {* ���ĳ�ӽڵ�ĺ�һͬ���ڵ㣬���򷵻� nil }
    function GetNextSibling: TCnLeaf;
    {* ���ͬ���ĺ�һ�ӽڵ㣬���򷵻� nil }
    function GetPrev: TCnLeaf;
    {* ���ͬ���ڵ��ǰһ���ڵ㣬���ޣ��򷵻ظ��ڵ㣬���ޣ��򷵻� nil }
    function GetPrevChild(Value: TCnLeaf): TCnLeaf;
    {* ���ĳһ�ӽڵ��ǰһͬ���ڵ㣬���򷵻� nil }
    function GetPrevSibling: TCnLeaf;
    {* ���ͬ����ǰһ�ӽڵ㣬���򷵻� nil }
    function GetAbsoluteItems(AAbsoluteIndex: Integer): TCnLeaf;
    {* ����������ȵı���˳���õ� n ���ӽڵ㣬������ TreeNodes �еĻ��� }
    function GetAbsoluteIndexFromParent(IndirectParentLeaf: TCnLeaf): Integer;
    {* ��� Leaf �� IndirectParentLeaf ��������ȵı���˳���������0 ��ʼ��
      ��� IndirectParentLeaf ���Ǽ�ӻ�ֱ�� Parent �򷵻� -1}
    function HasAsParent(Value: TCnLeaf): Boolean;
    {* ָ���Ľڵ��Ƿ��Ǳ��ڵ���ϼ�����ϼ� }
    function IndexOf(ALeaf: TCnLeaf): Integer;
    {* ��ֱ���ӽڵ��в����Ƿ���ĳһ�ڵ㲢���������� }
    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    {* ���������е�����ֵ }
    property AllCount: Integer read GetAllCount;
    {* ��������ڵ���Ŀ }
    property Count: Integer read GetCount;
    {* ֱ���ӽڵ���Ŀ }
    property HasChildren: Boolean read GetHasChildren;
    {* �Ƿ����ӽڵ� }
    property Index: Integer read GetIndex;
    {* ��Ҷ�ڵ��ڸ��ڵ��б��е�˳���������� 0 ��ʼ���޸���Ϊ -1 }
    property Items[Index: Integer]: TCnLeaf read GetItems write SetItems; default;
    {* ֱ��Ҷ�ڵ����� }
    property SubTreeHeight: Integer read GetSubTreeHeight;
    {* �˽ڵ��������������߶ȣ����ӽڵ�ʱΪ 0}

    property Level: Integer read GetLevel;
    {* ���ڵ������Root �ڵ� Level Ϊ 0 }
    property Parent: TCnLeaf read FParent;
    {* ���ڵ㣬����д }
    property Tree: TCnTree read GetTree;
    {* ��������һ��Ҷ��������һ���� }
  published
    property Obj: TObject read FObj write FObj;
    {* ���Ա���һ��������}
    property Data: Integer read FData write FData;
    {* ���Ա���һ���������ԣ������� Tag }
    property Text: string read FText write FText;
    {* ���Ա���һ�ַ��������� }
  end;

  ICnTreeFiler = interface(IUnknown)
  {* �����������Ľӿ� }
    ['{E81A9CE0-2D1D-11D9-BA1C-5254AB35836A}']
    procedure LoadFromFile(Instance: TPersistent; const FileName: string);
    procedure SaveToFile(Instance: TPersistent; const FileName: string);
  end;

  TCnLeafClass = class of TCnLeaf;

{$IFDEF MSWINDOWS}
  TCnTreeNodeEvent = procedure (ALeaf: TCnLeaf; ATreeNode: TTreeNode;
    var Valid: Boolean) of object;
{$ENDIF}

{$IFDEF SUPPORT_FMX}
  TCnTreeViewItemEvent = procedure (ALeaf: TCnLeaf; ATreeItem: TTreeViewItem;
    var Valid: Boolean) of object;
{$ENDIF}

  TCnTree = class(TPersistent)
  {* ����������ʵ����}
  private
    FLeafClass: TCnLeafClass;
    FBatchUpdating: Boolean;
    FLeaves: TObjectList;
    FRoot: TCnLeaf;
    FOnWidthFirstTravelLeaf: TNotifyEvent;
    FOnDepthFirstTravelLeaf: TNotifyEvent;
{$IFDEF MSWINDOWS}
    FOnSaveANode: TCnTreeNodeEvent;
    FOnLoadANode: TCnTreeNodeEvent;
{$ENDIF}
{$IFDEF SUPPORT_FMX}
    FOnSaveAItem: TCnTreeViewItemEvent;
    FOnLoadAItem: TCnTreeViewItemEvent;
{$ENDIF}
    function GetMaxLevel: Integer;
    function GetHeight: Integer; virtual;
    procedure AssignLeafAndChildren(Source, DestLeaf: TCnLeaf; DestTree: TCnTree);
  protected
    function DefaultLeafClass: TCnLeafClass; virtual;

    function GetRoot: TCnLeaf;
    function GetItems(AbsoluteIndex: Integer): TCnLeaf;
    function GetCount: Integer;
    function GetRegisteredCount: Integer;

    procedure AssignTo(Dest: TPersistent); override;

    function CreateLeaf(ATree: TCnTree): TCnLeaf; virtual;
    procedure DoDepthFirstTravelLeaf(ALeaf: TCnLeaf); virtual;
    procedure DoWidthFirstTravelLeaf(ALeaf: TCnLeaf); virtual;
{$IFDEF MSWINDOWS}
    function DoLoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean; virtual;
    function DoSaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean; virtual;
{$ENDIF}

{$IFDEF SUPPORT_FMX}
    function DoLoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem): Boolean;
    function DoSaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem): Boolean;
{$ENDIF}

    procedure ValidateComingLeaf(AParent, AChild: TCnLeaf); virtual;
    {* ��ĳ�ڵ���Ҫ����һ���ӽڵ�ʱ�����ã�������������������׳��쳣�����ؿ���}

    procedure RegisterLeaf(ALeaf: TCnLeaf);
    {* ����Ҷ�ڵ���ã������еǼǴ�Ҷ�ڵ� }
    procedure UnRegisterLeaf(ALeaf: TCnLeaf);
    {* ����Ҷ�ڵ���ã�ȡ����Ҷ�ڵ�ĵǼ� }

{$IFDEF MSWINDOWS}
    procedure LoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); virtual;
    {* ��һ TreeNode �ڵ��������ӽڵ㣬���ݹ���� }
    procedure SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); virtual;
    {* ���ڵ㱾���Լ��ӽڵ�д��һ TreeNode�����ݹ���� }
{$ENDIF}

{$IFDEF SUPPORT_FMX}
    procedure LoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); virtual;
    {* ��һ TreeNode �ڵ��������ӽڵ㣬���ݹ���� }
    procedure SaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); virtual;
    {* ���ڵ㱾���Լ��ӽڵ�д��һ TreeNode�����ݹ���� }
{$ENDIF}
  public
    constructor Create; overload;
    {* ���췽�� }
    constructor Create(LeafClass: TCnLeafClass); overload;
    {* ��һ���췽��}
    destructor Destroy; override;
    {* �������� }
    procedure DepthFirstTravel;
    {* ����������ȱ��� }
    procedure WidthFirstTravel;
    {* ���й�����ȱ��� }
    function ExtractLeaf(ALeaf: TCnLeaf): TCnLeaf;
    {* �����а���һҶ�ڵ㲢������ }
    procedure Clear;
    {* ������ͷ�����Ҷ�ڵ㣬��������ͷţ���������������������֪ͨ���� }

    // ������ӷ���
    function AddChildFirst(AParent: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һ���ӽڵ� }
    function AddChild(AParent: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һβ�ӽڵ� }
    function InsertChild(AParent: TCnLeaf; AIndex: Integer): TCnLeaf;
    {* ��ָ���Ľڵ�����һָ��λ�õ��ӽڵ� }
    function AddFirst(ASibing: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һͬ������ǰ�ڵ� }
    function Add(ASibing: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һͬ�������ڵ� }

    procedure ExchangeWithChild(Leaf1, Leaf2: TCnLeaf); overload;
    {* �������ڵ�λ�ã����ӽڵ���һ�𽻻� }
    procedure ExchangeWithChild(AbsoluteIndex1, AbsoluteIndex2: Integer); overload;
    {* �������ڵ�λ�ã����ӽڵ���һ�𽻻� }
    procedure Exchange(Leaf1, Leaf2: TCnLeaf); overload;
    {* �����������ڵ�λ�� }
    procedure Exchange(AbsoluteIndex1, AbsoluteIndex2: Integer); overload;
    {* �������������������ڵ�λ�� }

{$IFDEF MSWINDOWS}
    // �� TreeView �Ľ���������ע�� Root �����뽻��
    procedure LoadFromTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* ��һ VCL �� TreeView ����ڵ����ݡ�RootNode ���ӽڵ㱻����� RootLeaf ��ָ����
    �ڵ���ӽڵ㣬RootNode Ϊ nil ��ʾ����ȫ�� TreeNodes��RootLeaf Ϊ nil ��ʾ
    �����Ϊ Tree.Root ��ֱ���ڵ㣬Ҳ�������нڵ�}
    procedure SaveToTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* ���ڵ�����д��һ VCL �� TreeView�� RootLeaf ���ӽڵ㱻д��� RootNode ��ָ����
    �ڵ���ӽڵ㣬RootLeaf Ϊ nil ��ʾд�� Root �������ӽڵ㣬��ʵҲ�������н�
    �㣬RootNode Ϊ nil ��ʾд��Ľ���Ϊ TreeView �ĸ� TreeNodes}
{$ENDIF}

{$IFDEF SUPPORT_FMX}
    procedure LoadFromTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* ��һ FMX �� TreeView ����ڵ����ݡ�RootItem ���ӽڵ㱻����� RootLeaf ��ָ����
    �ڵ���ӽڵ㣬RootItem Ϊ nil ��ʾ����ȫ�� TreeNodes��RootLeaf Ϊ nil ��ʾ
    �����Ϊ Tree.Root ��ֱ���ڵ㣬Ҳ�������нڵ�}
    procedure SaveToTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* ���ڵ�����д��һ FMX �� TreeView�� RootLeaf ���ӽڵ㱻д��� RootItem ��ָ����
    �ڵ���ӽڵ㣬RootLeaf Ϊ nil ��ʾд�� Root �������ӽڵ㣬��ʵҲ�������н�
    �㣬RootItem Ϊ nil ��ʾд��Ľ���Ϊ TreeView �ĸ� TreeNodes}
{$ENDIF}

    // ��������
    procedure LoadFromFile(Filer: ICnTreeFiler; const FileName: string); virtual;
    {* ���ļ����������ڵ㣬���ṩ�ӿڵĶ���ʵ�� }
    procedure SaveToFile(Filer: ICnTreeFiler; const FileName: string); virtual;
    {* �����ڵ㱣�����ļ������ṩ�ӿڵĶ���ʵ�� }

    property BatchUpdating: Boolean read FBatchUpdating write FBatchUpdating;
    {* �Ƿ����������£�Ϊ True ʱҶ�ڵ��ͷ�ʱ��֪ͨ Tree }
    property Root: TCnLeaf read GetRoot;
    {* ���ڵ㣬���Ǵ��� }
    property Items[AbsoluteIndex: Integer]: TCnLeaf read GetItems;
    {* ����������ȵı���˳���õ� n ���ӽڵ㣬������ TreeNodes �еĻ��ƣ�0 ���� Root }
    property Count: Integer read GetCount;
    {* �����������нڵ����Ŀ������ Root }
    property MaxLevel: Integer read GetMaxLevel;
    {* �������������ڵ�Ĳ�����Root Ϊ 0}
    property Height: Integer read GetHeight;
    {* ���߶ȣ�ֻ�� Root ʱΪ 1}
    property RegisteredCount: Integer read GetRegisteredCount;
    {* ������������ע������ӽڵ����Ŀ }
  published
    property OnDepthFirstTravelLeaf: TNotifyEvent read FOnDepthFirstTravelLeaf write FOnDepthFirstTravelLeaf;
    {* ������ȱ���ʱ������һ��Ҷ�ڵ�ʱ�Ĵ����¼���Sender �Ǵ˽ڵ� }
    property OnWidthFirstTravelLeaf: TNotifyEvent read FOnWidthFirstTravelLeaf write FOnWidthFirstTravelLeaf;
    {* ������ȱ���ʱ������һ��Ҷ�ڵ�ʱ�Ĵ����¼���Sender �Ǵ˽ڵ� }
{$IFDEF MSWINDOWS}
    property OnLoadANode: TCnTreeNodeEvent read FOnLoadANode write FOnLoadANode;
    {* �� VCL �� TreeView ������ڵ�ʱ���ÿһ���ڵ�Ĵ����¼� }
    property OnSaveANode: TCnTreeNodeEvent read FOnSaveANode write FOnSaveANode;
    {* ���ڵ���� VCL �� TreeView ʱ���ÿһ���ڵ�Ĵ����¼� }
{$ENDIF}
{$IFDEF SUPPORT_FMX}
    property OnLoadAItem: TCnTreeViewItemEvent read FOnLoadAItem write FOnLoadAItem;
    {* �� VCL �� TreeView ������ڵ�ʱ���ÿһ���ڵ�Ĵ����¼� }
    property OnSaveAItem: TCnTreeViewItemEvent read FOnSaveAItem write FOnSaveAItem;
    {* ���ڵ���� VCL �� TreeView ʱ���ÿһ���ڵ�Ĵ����¼� }
{$ENDIF}
  end;

//==============================================================================
// ��������ʵ��
//==============================================================================

  ECnBinaryTreeException = class(Exception);

  TCnBinaryTree = class;

  TCnBinaryLeaf = class(TCnLeaf)
  {* �������ڵ����࣬�������ӽڵ�ķ�װ}
  private
    function GetLeftLeaf: TCnBinaryLeaf;
    function GetRightLeaf: TCnBinaryLeaf;
    procedure SetLeftLeaf(const Value: TCnBinaryLeaf);
    procedure SetRightLeaf(const Value: TCnBinaryLeaf);
    function GetSubTreeHeight: Integer; override;
    function GetTree: TCnBinaryTree;
  protected
    procedure DoPreOrderTravel;
    procedure DoInOrderTravel;
    procedure DoPostOrderTravel;
  public
    constructor Create(ATree: TCnTree); override;
    function IsBalance: Boolean;
    {* �Դ˽ڵ�Ϊ���ڵ���Ӷ������Ƿ���ƽ�������}
    property LeftLeaf: TCnBinaryLeaf read GetLeftLeaf write SetLeftLeaf;
    {* ���ӽڵ㣬ʹ�õ� 0 ���ӽڵ㣬���򷵻� nil}
    property RightLeaf: TCnBinaryLeaf read GetRightLeaf write SetRightLeaf;
    {* ���ӽڵ㣬ʹ�õ� 1 ���ӽڵ㣬���򷵻� nil}

    property Tree: TCnBinaryTree read GetTree;
    {* ��������һ��Ҷ��������һ���� }
  end;

  TCnBinaryLeafClass = class of TCnBinaryLeaf;

  TCnBinaryTree = class(TCnTree)
  {* ������ʵ����}
  private
    FOnPostOrderTravelLeaf: TNotifyEvent;
    FOnInOrderTravelLeaf: TNotifyEvent;
    FOnPreOrderTravelLeaf: TNotifyEvent;
    function GetHeight: Integer; override;
  protected
    function DefaultLeafClass: TCnLeafClass; override;
    procedure ValidateComingLeaf(AParent, AChild: TCnLeaf); override;

    function GetRoot: TCnBinaryLeaf;
    function GetCount: Integer;

    procedure DoPreOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;
    procedure DoInOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;
    procedure DoPostOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;

{$IFDEF MSWINDOWS}
    procedure LoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); override;
    {* ��һ VCL �� TreeNode �ڵ��������ӽڵ㣬���ݹ���ã��ϻ������������ӽڵ������ }
    procedure SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); override;
    {* ���ڵ㱾���Լ��ӽڵ�д��һ VCL �� TreeNode�����ݹ���� }
{$ENDIF}

{$IFDEF SUPPORT_FMX}
    procedure LoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); override;
    {* ��һ FMX �� TreeViewItem �ڵ��������ӽڵ㣬���ݹ���ã��ϻ������������ӽڵ������ }
    procedure SaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); override;
    {* ���ڵ㱾���Լ��ӽڵ�д��һ FMX �� TreeViewItem�����ݹ���� }
{$ENDIF}
  public
    constructor Create; overload;
    {* ���췽�� }
    constructor Create(LeafClass: TCnBinaryLeafClass); overload;
    {* ��һ���췽��}

    function AddLeftChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
    {* ��ָ���ڵ��������ӽڵ㣬���Ѵ����򷵻� nil}
    function AddRightChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
    {* ��ָ���ڵ��������ӽڵ㣬���Ѵ����򷵻� nil}
    procedure DeleteLeftChild(AParent: TCnBinaryLeaf);
    {* ɾ��ָ���ڵ�����ӽڵ㣬Ҳ������ nil}
    procedure DeleteRightChild(AParent: TCnBinaryLeaf);
    {* ɾ��ָ���ڵ�����ӽڵ㣬Ҳ������ nil}

{$IFDEF MSWINDOWS}
    // �� TreeView �Ľ���������ע�� Root �����뽻��
    procedure LoadFromTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* ��һ VCL �� TreeView ����ڵ����ݡ�RootNode ���ӽڵ㱻����� RootLeaf ��ָ����
    �ڵ���ӽڵ㣬RootNode Ϊ nil ��ʾ�Ӹ�ɨ��ȫ�� TreeNodes��RootLeaf Ϊ nil ��ʾ
    �����Ϊ Tree.Root ��ֱ���ڵ㣬Ҳ�������нڵ㡣
    ��������һ�� TreeNode�����һ���ӽڵ���Ϊ���������ڶ�����Ϊ�����������������ĺ���}
    procedure SaveToTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* ���ڵ�����д��һ VCL �� TreeView�� RootLeaf ���ӽڵ㱻д��� RootNode ��ָ����
    �ڵ���ӽڵ㣬RootLeaf Ϊ nil ��ʾд�� Root �������ӽڵ㣬��ʵҲ�������н�
    �㣬RootNode Ϊ nil ��ʾд��Ľ���Ϊ TreeView �ĸ� TreeNodes}
{$ENDIF}

{$IFDEF SUPPORT_FMX}
    procedure LoadFromTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* ��һ FMX �� TreeView ����ڵ����ݡ�RootNode ���ӽڵ㱻����� RootLeaf ��ָ����
    �ڵ���ӽڵ㣬RootItem Ϊ nil ��ʾ�Ӹ�ɨ��ȫ�� TreeNodes��RootLeaf Ϊ nil ��ʾ
    �����Ϊ Tree.Root ��ֱ���ڵ㣬Ҳ�������нڵ㡣
    ��������һ�� TreeNode�����һ���ӽڵ���Ϊ���������ڶ�����Ϊ�����������������ĺ���}
    procedure SaveToTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* ���ڵ�����д��һ FMX �� TreeView�� RootLeaf ���ӽڵ㱻д��� RootItem ��ָ����
    �ڵ���ӽڵ㣬RootLeaf Ϊ nil ��ʾд�� Root �������ӽڵ㣬��ʵҲ�������н�
    �㣬RootNode Ϊ nil ��ʾд��Ľ���Ϊ TreeView �ĸ� TreeNodes}
{$ENDIF}

    function IsFull: Boolean;
    {* �Ƿ����������������еײ�Ҷ�ڵ��ȫ�����Ҳ����ͬ}
    function IsComplete: Boolean;
    {* �Ƿ�����ȫ������}
    function IsBalance: Boolean;
    {* �Ƿ���ƽ�������}

    procedure PreOrderTravel;
    {* �ȸ����������������}
    procedure InOrderTravel;
    {* �и����������������}
    procedure PostOrderTravel;
    {* ������������������}

    property Root: TCnBinaryLeaf read GetRoot;
    {* ���ڵ㣬���Ǵ��� }
    property Count: Integer read GetCount;
    {* �����������нڵ����Ŀ������ Root }
    property Height: Integer read GetHeight;
    {* ���߶ȣ�ֻ�и��ڵ�ʱΪ 1}

    property OnPreOrderTravelLeaf: TNotifyEvent read FOnPreOrderTravelLeaf
      write FOnPreOrderTravelLeaf;
    {* �ȸ��������ʱ�������¼�}
    property OnInOrderTravelLeaf: TNotifyEvent read FOnInOrderTravelLeaf
      write FOnInOrderTravelLeaf;
    {* �и��������ʱ�������¼�}
    property OnPostOrderTravelLeaf: TNotifyEvent read FOnPostOrderTravelLeaf
      write FOnPostOrderTravelLeaf;
    {* ����������ʱ�������¼�}
  end;

//==============================================================================
// �ֵ�����ʵ��
//==============================================================================

  TCnTrieTree = class;

  TCnTrieLeaf = class(TCnLeaf)
  {* �ֵ�������Ҷ�࣬ʹ�� Data �洢ǰ׺�ַ�}
  private
    FCharacter: Char;
    function GetCharacter: Char;
    procedure SetCharacter(const Value: Char);
    function GetTree: TCnTrieTree;
  protected
    function GetItems(Index: Integer): TCnTrieLeaf;
    procedure SetItems(Index: Integer; const Value: TCnTrieLeaf);

    function DoInsertChar(P: PChar): TCnTrieLeaf;
    function DoSearchChar(P: PChar): TCnTrieLeaf;
  public
    property Character: Char read GetCharacter write SetCharacter;
    property Items[Index: Integer]: TCnTrieLeaf read GetItems write SetItems; default;
    {* ת�������͵�ֱ��Ҷ�ڵ����� }
    property Tree: TCnTrieTree read GetTree;
    {* ת�������͵���������һ��Ҷ��������һ���� }
  end;

  TCnTrieTree = class(TCnTree)
  {* �ֵ���ʵ����}
  private
    FCaseSensitive: Boolean;
    FOnlyChar: Boolean;
    FAnsiFastMode: Boolean;
    function ConvertCharWithCase(C: Char): Char; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* ���ݴ�Сд���ã�����ԭ�ַ����д��ĸ�������Сд��ĸ��}
  protected
    function GetRoot: TCnTrieLeaf;
    function DefaultLeafClass: TCnLeafClass; override;
    function CreateTrieLeaf: TCnTrieLeaf;
  public
    constructor Create(ACaseSensitive: Boolean = True;
      AOnlyChar: Boolean = False; AnAnsiFastMode: Boolean = False); reintroduce; virtual;
    {* �ֵ������캯��������ָ����
      ACaseSensitive���Ƿ����ִ�Сд
      AOnlyChar���ڵ����Ƿ�ֻ�洢��ĸ����Ϊ����ڵ�� Text ���Ի�洢�ַ���ֵ
      AnAnsiFastMode���Ƿ�ʹ�� Ansi ����ģʽ���� Unicode ���뻷���´�������Ч
        ��Ϊ True����ڵ㽫��ǰ���� 256 ���հ��ӽڵ㹩����������������������洢}

    function InsertString(const Str: string): TCnTrieLeaf;
    {* �����ַ��������ز����Ҷ�ڵ㹩����������ݣ�����Ѵ����򷵻� nil}
    function SearchString(const Str: string): TCnTrieLeaf;
    {* �����ַ��������ز��ҵ���Ҷ�ڵ㣬���δ�ҵ��򷵻� nil}
    function StringExists(const Str: string): Boolean;
    {* �����ַ����������Ƿ����}

    property Root: TCnTrieLeaf read GetRoot;
    {* ���ڵ� }
    property OnlyChar: Boolean read FOnlyChar;
    {* �Ƿ�ֻ�洢��ĸ}
    property CaseSensitive: Boolean read FCaseSensitive;
    {* �Ƿ����ִ�Сд��ע�⣬��������ִ�Сд�� OnlyChar Ϊ False��
       ���ҵ��Ľڵ���洢���ַ������ݿ��ܲ����ϴ���ʱ�Ĵ�Сд���}
    property AnsiFastMode: Boolean read FAnsiFastMode;
    {* Ansi ģʽ���Ƿ�Ԥ�ȴ��� 255 ���ӽڵ㹩���ٶ�λ������ʹ����������}
  end;

implementation


{$IFDEF SUPPORT_FMX}

function GetNextSiblingItem(Item: TTreeViewItem): TTreeViewItem;
var
  P: TTreeViewItem;
  T: TCustomTreeView;
begin
  Result := nil;
  P := Item.ParentItem;
  if P <> nil then
  begin
    if (Item.Index >= 0) and (Item.Index < P.Count - 1) then
      Result := P.ItemByIndex(Item.Index + 1);
    Exit;
  end;

  T := Item.TreeView;
  if T <> nil then
  begin
    if (Item.Index >= 0) and (Item.Index < T.Count - 1) then
      Result := T.Items[Item.Index + 1];
  end;
end;

{$ENDIF}

//==============================================================================
// TCnLeaf
//==============================================================================

constructor TCnLeaf.Create(ATree: TCnTree);
begin
  inherited Create;
  Assert(ATree <> nil);
  FList := TList.Create;
  FTree := ATree;
  ATree.RegisterLeaf(Self);
end;

destructor TCnLeaf.Destroy;
var
  I: Integer;
begin
  if not FTree.BatchUpdating then
  begin
    for I := FList.Count - 1 downto 0 do
      DeleteChild(I);
    FTree.UnregisterLeaf(Self);
  end;
  FreeAndNil(FList);
  inherited;
end;

function TCnLeaf.AddChild(ALeaf: TCnLeaf): TCnLeaf;
begin
  Assert(ALeaf.Tree = Self.FTree);
  FTree.ValidateComingLeaf(Self, ALeaf);
  Result := ALeaf;
  FList.Add(Result);
  Result.FParent := Self;
end;

function TCnLeaf.AddChildFirst(ALeaf: TCnLeaf): TCnLeaf;
begin
  Assert(ALeaf.Tree = Self.FTree);
  FTree.ValidateComingLeaf(Self, ALeaf);
  Result := ALeaf;
  FList.Insert(0, Result);
  Result.FParent := Self;
end;

procedure TCnLeaf.Clear;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
  begin
    TCnLeaf(FList.Items[I]).Free;
    FList.Delete(I);
  end;
end;

procedure TCnLeaf.DeleteChild(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    TCnLeaf(FList.Items[AIndex]).Free;
    FList.Delete(AIndex);
  end;
end;

procedure TCnLeaf.Delete;
begin
  if FParent <> nil then
    FParent.DeleteChild(Index)
  else
    raise ECnTreeException.Create('Root can NOT be deleted.');
end;

function TCnLeaf.ExtractChild(ALeaf: TCnLeaf): TCnLeaf;
var
  AIndex: Integer;
begin
  if ALeaf.HasAsParent(Self) then
  begin
    AIndex := ALeaf.Index;
    Result := ALeaf.Parent.Items[AIndex];
    ALeaf.Parent.FList.Delete(AIndex);
  end
  else
    Result := nil;
end;

function TCnLeaf.ExtractChild(AIndex: Integer): TCnLeaf; 
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    Result := TCnLeaf(Items[AIndex]);
    Result.FParent := nil;
    FList.Delete(AIndex);
  end;
end;

procedure TCnLeaf.DoDepthFirstTravel;
var
  I: Integer;
begin
  if FTree <> nil then
    FTree.DoDepthFirstTravelLeaf(Self);
  for I := 0 to FList.Count - 1 do
    Items[I].DoDepthFirstTravel;
end;

procedure TCnLeaf.DoWidthFirstTravel;
var
  Queue: TQueue;
  I: Integer;
  Node: TCnLeaf;
begin
  // ������ȱ��������ӽڵ�ĵݹ飬���ǿ���һ���ʹ�ö���
  if FTree <> nil then
    FTree.DoWidthFirstTravelLeaf(Self);
  Queue := TQueue.Create;
  try
    for I := 0 to FList.Count - 1 do
      Queue.Push(Items[I]);

    while Queue.Count > 0 do
    begin
      Node := TCnLeaf(Queue.Pop);
      if FTree <> nil then
        FTree.DoWidthFirstTravelLeaf(Node);

      if Node.Count > 0 then
        for I := 0 to Node.Count - 1 do
          Queue.Push(Node.Items[I]);
    end;
  finally
    Queue.Free;
  end;
end;

function TCnLeaf.GetAbsoluteIndex: Integer;
begin
  if FParent <> nil then
    Result := Self.Index + FParent.AbsoluteIndex + 1
  else
    Result := 0;
end;

function TCnLeaf.GetAbsoluteItems(AAbsoluteIndex: Integer): TCnLeaf;
var
  I, ACount, IndexCount: Integer;
begin
  Result := nil;
  if AAbsoluteIndex < 0 then
    Exit
  else
  begin
    IndexCount := 0;
    for I := 0 to Count - 1 do
    begin
      if IndexCount = AAbsoluteIndex then
      begin
        Result := Items[I];
        Exit;
      end;

      if Items[I] <> nil then
        ACount := Items[I].AllCount + 1
      else
        ACount := 1;

      if IndexCount + ACount > AAbsoluteIndex then
      begin
        Result := Items[I].GetAbsoluteItems(AAbsoluteIndex - IndexCount - 1);
        Exit;
      end
      else
        Inc(IndexCount, ACount);
    end;
  end;
end;

function TCnLeaf.GetAllCount: Integer;
var
  I: Integer;
begin
  Result := Count;
  for I := 0 to Self.Count - 1 do
    if Items[I] <> nil then
      Result := Result + Self.Items[I].AllCount;
end;

function TCnLeaf.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCnLeaf.GetFirstChild: TCnLeaf;
begin
  if HasChildren then
    Result := TCnLeaf(FList.Items[0])
  else
    Result := nil;
end;

function TCnLeaf.GetHasChildren: Boolean;
begin
  Result := FList.Count > 0;
end;

function TCnLeaf.GetIndex: Integer;
begin
  if FParent <> nil then
    Result := FParent.IndexOf(Self)
  else
    Result := -1;
end;

function TCnLeaf.GetItems(Index: Integer): TCnLeaf;
begin
  Result := TCnLeaf(FList.Items[Index]);
end;

function TCnLeaf.GetLastChild: TCnLeaf;
begin
  if HasChildren then
    Result := TCnLeaf(FList.Items[Count - 1])
  else
    Result := nil;
end;

function TCnLeaf.GetLevel: Integer;
begin
  if FParent = nil then
    Result := 0
  else
    Result := FParent.Level + 1;
end;

function TCnLeaf.GetAllNonNilCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Self.Count - 1 do
    if Items[I] <> nil then
      Result := Result + Self.Items[I].AllNonNilCount + 1;
end;

function TCnLeaf.GetSubTreeHeight: Integer;
var
  I, MaxChildHeight: Integer;
begin
  Result := 0;
  if not HasChildren then
    Exit;

  MaxChildHeight := 0;
  for I := 0 to FList.Count - 1 do
  begin
    if FList[I] <> nil then
    begin
      if MaxChildHeight = 0 then // �в�Ϊ nil ���ӽڵ㣬������� 1
        MaxChildHeight := 1;

      if TCnLeaf(FList[I]).SubTreeHeight > MaxChildHeight then
        MaxChildHeight := TCnLeaf(FList[I]).SubTreeHeight;
    end;
  end;
  Result := MaxChildHeight + 1;
end;

function TCnLeaf.GetTree: TCnTree;
begin
  Result := FTree;
end;

function TCnLeaf.HasAsParent(Value: TCnLeaf): Boolean;
var
  AParent: TCnLeaf;
begin
  Result := False;
  if Value.Tree <> Self.Tree then
    Exit;
    
  AParent := FParent;
  while AParent <> nil do
  begin
    if AParent = Value then
    begin
      Result := True;
      Exit;
    end
    else
      AParent := AParent.Parent;
  end;
end;

function TCnLeaf.IndexOf(ALeaf: TCnLeaf): Integer;
begin
  Result := FList.IndexOf(ALeaf);
end;

function TCnLeaf.InsertChild(ALeaf: TCnLeaf; AIndex: Integer): TCnLeaf;
begin
  if (ALeaf <> nil) and (AIndex >= 0) and (AIndex <= Count) then
  begin
    Result := ALeaf;
    FList.Insert(AIndex, ALeaf);
    ALeaf.FParent := Self;
  end
  else
    Result := nil;
end;

function TCnLeaf.GetNext: TCnLeaf;
begin
  Result := GetFirstChild;
  if Result = nil then
    Result := GetNextSibling;
end;

function TCnLeaf.GetNextChild(Value: TCnLeaf): TCnLeaf;
begin
  Result := nil;
  if Value.Parent = Self then
    if Value.Index < Self.Count - 1 then
      Result := Items[Value.Index + 1];
end;

function TCnLeaf.GetNextSibling: TCnLeaf;
begin
  Result := nil;
  if Parent <> nil then
    if Index < Parent.Count - 1 then
      Result := Parent.Items[Index + 1];
end;

function TCnLeaf.GetPrev: TCnLeaf;
begin
  Result := GetPrevSibling;
  if Result = nil then
    Result := Parent;
end;

function TCnLeaf.GetPrevChild(Value: TCnLeaf): TCnLeaf;
begin
  Result := nil;
  if Value.Parent = Self then
    if Value.Index > 0 then
      Result := Items[Value.Index - 1];
end;

function TCnLeaf.GetPrevSibling: TCnLeaf;
begin
  Result := nil;
  if Parent <> nil then
    if Index > 0 then
      Result := Parent.Items[Index - 1];
end;

function TCnLeaf.SetChild(ALeaf: TCnLeaf; Index: Integer): TCnLeaf;
begin
  if (ALeaf <> nil) and (ALeaf.Tree = Self.FTree) and
    (Index >= 0) and (Index < Count) then
  begin
    Result := FList.Items[Index];
    FList.Items[Index] := ALeaf;
    ALeaf.FParent := Self;
  end
  else
    Result := nil;
end;

procedure TCnLeaf.SetItems(Index: Integer; const Value: TCnLeaf);
begin
  if (Index >= 0) and (Index < Count) then
  begin
    FList.Items[Index] := Value;
    Value.FParent := Self;
  end;
end;

procedure TCnLeaf.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnLeaf then
  begin
    TCnLeaf(Dest).Data := FData;
    TCnLeaf(Dest).Text := FText;
    TCnLeaf(Dest).Obj := FObj;
  end
  else
    inherited;
end;

function TCnLeaf.GetAbsoluteIndexFromParent(IndirectParentLeaf: TCnLeaf): Integer;
var
  I, Idx: Integer;
begin
  Result := -1;
  if FParent = IndirectParentLeaf then
  begin
    Idx := Index;
    Result := 0;
    for I := 0 to Idx - 1 do
      Result := Result + IndirectParentLeaf.Items[I].AllCount + 1;
  end
  else if HasAsParent(IndirectParentLeaf) then
  begin
    Result := FParent.GetAbsoluteIndexFromParent(IndirectParentLeaf)
      + GetAbsoluteIndexFromParent(FParent) + 1;
  end;
end;

//==============================================================================
// TCnTree
//==============================================================================

constructor TCnTree.Create;
begin
  inherited;
  FLeaves := TObjectList.Create(True);
  if FLeafClass = nil then
    FLeafClass := DefaultLeafClass;
  FRoot := CreateLeaf(Self);
end;

constructor TCnTree.Create(LeafClass: TCnLeafClass);
begin
  FLeafClass := LeafClass;
  Create;
end;

destructor TCnTree.Destroy;
begin
  FBatchUpdating := True;
  FLeaves.Free;
  inherited;
end;

procedure TCnTree.DepthFirstTravel;
begin
  FRoot.DoDepthFirstTravel;
end;

function TCnTree.CreateLeaf(ATree: TCnTree): TCnLeaf;
begin
  try
    Result := TCnLeaf(FLeafClass.NewInstance);
    Result.Create(ATree);
  except
    Result := nil;
  end;
end;

procedure TCnTree.DoDepthFirstTravelLeaf(ALeaf: TCnLeaf);
begin
  if Assigned(FOnDepthFirstTravelLeaf) then
    FOnDepthFirstTravelLeaf(ALeaf);
end;

procedure TCnTree.DoWidthFirstTravelLeaf(ALeaf: TCnLeaf);
begin
  if Assigned(FOnWidthFirstTravelLeaf) then
    FOnWidthFirstTravelLeaf(ALeaf);
end;

function TCnTree.GetRoot: TCnLeaf;
begin
  Result := FRoot;
end;

procedure TCnTree.RegisterLeaf(ALeaf: TCnLeaf);
begin
  if FLeaves.IndexOf(ALeaf) < 0 then
    FLeaves.Add(ALeaf);
end;

procedure TCnTree.WidthFirstTravel;
begin
  FRoot.DoWidthFirstTravel;
end;

procedure TCnTree.UnRegisterLeaf(ALeaf: TCnLeaf);
begin
  FLeaves.Extract(ALeaf);
end;

procedure TCnTree.Clear;
begin
  FBatchUpdating := True;
  try
    FLeaves.Clear;
    // FRoot �Ѿ��� Fleaves �ͷţ������ٴ��ͷ�.
    FRoot := CreateLeaf(Self);
  finally
    FBatchUpdating := False;
  end;
end;

function TCnTree.ExtractLeaf(ALeaf: TCnLeaf): TCnLeaf;
begin
  Result := nil;
  if ALeaf.Tree = Self then
  begin
    Self.UnRegisterLeaf(ALeaf);
    if ALeaf.Parent <> nil then
      Result := ALeaf.Parent.ExtractChild(ALeaf.Index);
  end;
end;

function TCnTree.AddChild(AParent: TCnLeaf): TCnLeaf;
begin
  if AParent.Tree = Self then
  begin
    Result := CreateLeaf(Self);
    AParent.AddChild(Result);
  end
  else
    Result := nil;
end;

function TCnTree.AddChildFirst(AParent: TCnLeaf): TCnLeaf;
begin
  if AParent.Tree = Self then
  begin
    Result := CreateLeaf(Self);
    AParent.AddChildFirst(Result);
  end
  else
    Result := nil;
end;

function TCnTree.InsertChild(AParent: TCnLeaf; AIndex: Integer): TCnLeaf;
begin
  if AParent.Tree = Self then
  begin
    Result := CreateLeaf(Self);
    if AParent.InsertChild(Result, AIndex) = nil then
    begin
      Result.Free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

function TCnTree.AddFirst(ASibing: TCnLeaf): TCnLeaf;
begin
  if (ASibing <> nil) and (ASibing.Tree = Self) and (ASibing.Parent <> nil) then
  begin
    Result := CreateLeaf(Self);
    if ASibing.Parent.AddChildFirst(Result) = nil then
    begin
      Result.Free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

function TCnTree.Add(ASibing: TCnLeaf): TCnLeaf;
begin
  if (ASibing <> nil) and (ASibing.Tree = Self) and (ASibing.Parent <> nil) then
  begin
    Result := CreateLeaf(Self);
    if ASibing.Parent.AddChild(Result) = nil then
    begin
      Result.Free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

procedure TCnTree.Exchange(AbsoluteIndex1, AbsoluteIndex2: Integer); 
begin
  Exchange(Items[AbsoluteIndex1], Items[AbsoluteIndex2]);
end;

procedure TCnTree.ExchangeWithChild(AbsoluteIndex1,
  AbsoluteIndex2: Integer);
begin
  ExchangeWithChild(Items[AbsoluteIndex1], Items[AbsoluteIndex2]);
end;

procedure TCnTree.ExchangeWithChild(Leaf1, Leaf2: TCnLeaf); 
var
  Parent2: TCnLeaf;
  Index2: Integer;
begin
  if (Leaf1 <> nil) and (Leaf2 <> nil) and (Leaf1 <> Leaf2)
    and (Leaf1.Tree = Self) and (Leaf2.Tree = Self) then
  begin
    if Leaf1.HasAsParent(Leaf2) or Leaf2.HasAsParent(Leaf1) then
      Exit; // Ϊ���ӹ�ϵ�Ĳ�������
    Parent2 := Leaf2.Parent;
    Index2 := Leaf2.Index;

    Leaf1.Parent.SetChild(Leaf2, Leaf1.Index);
    Parent2.SetChild(Leaf1, Index2);
  end;
end;

procedure TCnTree.Exchange(Leaf1, Leaf2: TCnLeaf); 
var
  Parent2: TCnLeaf;
  I, Index2: Integer;
  AList: TList;
begin
  if (Leaf1 <> nil) and (Leaf2 <> nil) and (Leaf1 <> Leaf2)
    and (Leaf1.Tree = Self) and (Leaf2.Tree = Self) then
  begin
    // ���������ڵ���ӽڵ��б����ڵ㽻��������
    Parent2 := Leaf2.Parent;
    Index2 := Leaf2.Index;

    AList := nil;
    try
      AList := TList.Create;
      for I := 0 to Leaf1.Count - 1 do
        AList.Add(Leaf1.Items[I]);

      Leaf1.FList.Clear;
      for I := 0 to Leaf2.Count - 1 do
        Leaf1.FList.Add(Leaf2.Items[I]);
      for I := 0 to AList.Count - 1 do
        Leaf2.FList.Add(AList.Items[I]);
    finally
      AList.Free;
    end;

    if Leaf1.Parent <> nil then
      Leaf1.Parent.SetChild(Leaf2, Leaf1.Index)
    else
      Leaf2.FParent := nil;
    if Parent2 <> nil then
      Parent2.SetChild(Leaf1, Index2)
    else
      Leaf1.FParent := nil;

    // ˳���жϸ��ڵ�
    if FRoot = Leaf1 then
      FRoot := Leaf2
    else if FRoot = Leaf2 then
      FRoot := Leaf1;
  end;
end;

function TCnTree.GetItems(AbsoluteIndex: Integer): TCnLeaf;
begin
  if AbsoluteIndex < 0 then
    Result := nil
  else if AbsoluteIndex = 0 then
    Result := FRoot
  else
    Result := FRoot.GetAbsoluteItems(AbsoluteIndex - 1);
end;

function TCnTree.GetCount: Integer;
begin
  Result := FRoot.AllCount + 1;
end;

function TCnTree.GetMaxLevel: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if (Items[I] <> nil) and (Items[I].Level > Result) then
      Result := Items[I].Level;
end;

function TCnTree.GetRegisteredCount: Integer;
begin
  Result := FLeaves.Count;
end;

{$IFDEF MSWINDOWS}

procedure TCnTree.LoadFromTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode;
  RootLeaf: TCnLeaf);
var
  ANode: TTreeNode;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootNode <> nil) and (RootNode.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    if RootLeaf = nil then
      Self.Clear
    else
      RootLeaf.Clear;

    if ATreeView.Items.Count > 0 then
    begin
      if RootNode = nil then
        ANode := ATreeView.Items[0]
      else
        ANode := RootNode;
      // ��һ���ڵ�
      if RootLeaf = nil then
        RootLeaf := FRoot;

      ALeaf := Self.AddChild(RootLeaf);
      LoadFromATreeNode(ALeaf, ANode);
      if RootNode <> nil then Exit;
      // ������ RootNode ʱ�� RootNode Ϊ�������Բ����� RootNode ��ͬ��ڵ�

      ANode := ANode.GetNextSibling; // �����˲��������̽ڵ�
      while ANode <> nil do
      begin
        ALeaf := Self.AddChild(RootLeaf);
        LoadFromATreeNode(ALeaf, ANode);
        ANode := ANode.GetNextSibling;
      end;
    end;
  end;
end;

procedure TCnTree.SaveToTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode;
  RootLeaf: TCnLeaf);
var
  I: Integer;
  ANode: TTreeNode;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootNode <> nil) and (RootNode.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    ATreeView.Items.BeginUpdate;
    try
      if RootNode <> nil then
        RootNode.DeleteChildren
      else
        ATreeView.Items.Clear;

      if RootLeaf = nil then
        RootLeaf := FRoot;
      if RootLeaf.Count > 0 then
      begin
        ANode := RootNode;
        for I := 0 to RootLeaf.Count - 1 do
        begin
          ALeaf := RootLeaf.Items[I]; // RootLeaf ���ӽڵ㣬RootLeaf �����뽻��
          if ALeaf = nil then
            Continue;
          ANode := ATreeView.Items.Add(ANode, '');
          SaveToATreeNode(ALeaf, ANode);
        end;
      end;
    finally
      ATreeView.Items.EndUpdate;
    end;
  end;
end;

{$ENDIF}

{$IFDEF SUPPORT_FMX}

procedure TCnTree.LoadFromTreeView(ATreeView: FMX.TreeView.TTreeView;
  RootItem: TTreeViewItem; RootLeaf: TCnLeaf);
var
  AItem: TTreeViewItem;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootItem <> nil) and (RootItem.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    if RootLeaf = nil then
      Self.Clear
    else
      RootLeaf.Clear;

    if ATreeView.GlobalCount > 0 then
    begin
      if RootItem = nil then
        AItem := ATreeView.Items[0]
      else
        AItem := RootItem;
      // ��һ���ڵ�
      if RootLeaf = nil then
        RootLeaf := FRoot;

      ALeaf := Self.AddChild(RootLeaf);
      LoadFromATreeViewItem(ALeaf, AItem);
      if RootItem <> nil then Exit;
      // ������ RootNode ʱ�� RootNode Ϊ�������Բ����� RootNode ��ͬ��ڵ�

      AItem := GetNextSiblingItem(AItem); // �����˲��������̽ڵ�
      while AItem <> nil do
      begin
        ALeaf := Self.AddChild(RootLeaf);
        LoadFromATreeViewItem(ALeaf, AItem);
        AItem := GetNextSiblingItem(AItem);
      end;
    end;
  end;
end;

procedure TCnTree.SaveToTreeView(ATreeView: FMX.TreeView.TTreeView;
  RootItem: TTreeViewItem; RootLeaf: TCnLeaf);
var
  I: Integer;
  AItem: TTreeViewItem;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootItem <> nil) and (RootItem.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    ATreeView.BeginUpdate;
    try
      if RootItem <> nil then
        RootItem.DeleteChildren
      else
        ATreeView.Clear;

      if RootLeaf = nil then
        RootLeaf := FRoot;
      if RootLeaf.Count > 0 then
      begin
        for I := 0 to RootLeaf.Count - 1 do
        begin
          ALeaf := RootLeaf.Items[I]; // RootLeaf ���ӽڵ㣬RootLeaf �����뽻��
          if ALeaf = nil then
            Continue;

          AItem := TTreeViewItem.Create(ATreeView);
          if RootItem = nil then
            AItem.Parent := ATreeView
          else
            AItem.Parent := RootItem;

          SaveToATreeViewItem(ALeaf, AItem);
        end;
      end;
    finally
      ATreeView.EndUpdate;
    end;
  end;
end;

{$ENDIF}

procedure TCnTree.LoadFromFile(Filer: ICnTreeFiler;
  const FileName: string);
begin
  if Filer <> nil then
    Filer.LoadFromFile(Self, FileName);
end;

procedure TCnTree.SaveToFile(Filer: ICnTreeFiler; const FileName: string);
begin
  if Filer <> nil then
    Filer.SaveToFile(Self, FileName);
end;

{$IFDEF MSWINDOWS}

procedure TCnTree.LoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode);
var
  I: Integer;
  Leaf: TCnLeaf;
begin
  if (ANode <> nil) and (ALeaf <> nil) then
  begin
    if DoLoadFromATreeNode(ALeaf, ANode) then
    begin
      for I := 0 to ANode.Count - 1 do
      begin
        Leaf := Self.AddChild(ALeaf);
        LoadFromATreeNode(Leaf, ANode.Item[I]);
      end;
    end
    else
    begin
      ALeaf.Delete;
    end;
  end;
end;

procedure TCnTree.SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode);
var
  I: Integer;
  Node: TTreeNode;
begin
  if (ANode <> nil) and (ALeaf <> nil) and (ANode.TreeView is ComCtrls.TTreeView) then
  begin
    if DoSaveToATreeNode(ALeaf, ANode) then
    begin
      for I := 0 to ALeaf.Count - 1 do
      begin
        if ALeaf.Items[I] = nil then
          Continue;
        Node := (ANode.TreeView as ComCtrls.TTreeView).Items.AddChild(ANode, '');
        SaveToATreeNode(ALeaf.Items[I], Node);
      end;
    end
    else
    begin
      ANode.Delete;
    end;
  end;
end;

function TCnTree.DoLoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnLoadANode) then
    FOnLoadANode(ALeaf, ANode, Result)
  else
  begin
    ALeaf.Text := ANode.Text;
    ALeaf.Data := Integer(ANode.Data);
  end;
end;

function TCnTree.DoSaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnSaveANode) then
  begin
    FOnSaveANode(ALeaf, ANode, Result);
  end
  else
  begin
    ANode.Text := ALeaf.Text;
    ANode.Data := Pointer(ALeaf.Data);
  end;
end;

{$ENDIF}

{$IFDEF SUPPORT_FMX}

procedure TCnTree.LoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem);
var
  I: Integer;
  Leaf: TCnLeaf;
begin
  if (AItem <> nil) and (ALeaf <> nil) then
  begin
    if DoLoadFromATreeViewItem(ALeaf, AItem) then
    begin
      for I := 0 to AItem.Count - 1 do
      begin
        Leaf := AddChild(ALeaf);
        LoadFromATreeViewItem(Leaf, AItem.Items[I]);
      end;
    end
    else
    begin
      ALeaf.Delete;
    end;
  end;
end;

procedure TCnTree.SaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem);
var
  I: Integer;
  Item: TTreeViewItem;
begin
  if (AItem <> nil) and (ALeaf <> nil) and (AItem.TreeView is TTreeView) then
  begin
    if DoSaveToATreeViewItem(ALeaf, AItem) then
    begin
      for I := 0 to ALeaf.Count - 1 do
      begin
        if ALeaf.Items[I] = nil then
          Continue;

        Item := TTreeViewItem.Create(AItem.TreeView);
        Item.Parent := AItem;
        SaveToATreeViewItem(ALeaf.Items[I], Item);
      end;
    end
    else
    begin
      AItem.Free;
    end;
  end;
end;

function TCnTree.DoLoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem): Boolean;
begin
  Result := True;
  if Assigned(FOnLoadAItem) then
    FOnLoadAItem(ALeaf, AItem, Result)
  else
  begin
    ALeaf.Text := AItem.Text;
    try
      ALeaf.Data := AItem.Data.AsInteger;
    except
      ALeaf.Data := 0;
    end;
  end;
end;

function TCnTree.DoSaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem): Boolean;
begin
  Result := True;
  if Assigned(FOnSaveAItem) then
  begin
    FOnSaveAItem(ALeaf, AItem, Result);
  end
  else
  begin
    AItem.Text := ALeaf.Text;
    AItem.Tag := ALeaf.Data; // Data ��Ӱ�� Text������ Tag
  end;
end;

{$ENDIF}

procedure TCnTree.ValidateComingLeaf(AParent, AChild: TCnLeaf);
begin

end;

procedure TCnTree.AssignLeafAndChildren(Source, DestLeaf: TCnLeaf; DestTree: TCnTree);
var
  I: Integer;
  Leaf: TCnLeaf;
begin
  if (Source <> nil) and (DestLeaf <> nil) and (DestTree <> nil) then
  begin
    DestLeaf.Assign(Source);
    DestLeaf.Clear;
    for I := 0 to Source.Count - 1 do
    begin
      Leaf := DestTree.CreateLeaf(DestTree);
      DestLeaf.AddChild(Leaf);
      AssignLeafAndChildren(Source.Items[I], Leaf, DestTree);
    end;
  end;
end;

procedure TCnTree.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnTree then
  begin
    TCnTree(Dest).Clear;
    // ��ȫ��¡���ڵ�Ľṹ
    AssignLeafAndChildren(FRoot, TCnTree(Dest).Root, TCnTree(Dest));
  end
  else
    inherited;
end;

//==============================================================================
// TCnBinaryTree
//==============================================================================

constructor TCnBinaryTree.Create;
begin
  inherited;

end;

function TCnBinaryTree.AddLeftChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
begin
  if (AParent.Tree = Self) and (AParent.LeftLeaf = nil) then
  begin
    Result := TCnBinaryLeaf(CreateLeaf(Self));
    AParent.LeftLeaf := Result;
  end
  else
    Result := nil;
end;

function TCnBinaryTree.AddRightChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
begin
  if (AParent.Tree = Self) and (AParent.RightLeaf = nil) then
  begin
    Result := TCnBinaryLeaf(CreateLeaf(Self));
    AParent.RightLeaf := Result;
  end
  else
    Result := nil;
end;

constructor TCnBinaryTree.Create(LeafClass: TCnBinaryLeafClass);
begin
  inherited Create(LeafClass);
end;

function TCnBinaryTree.DefaultLeafClass: TCnLeafClass;
begin
  Result := TCnBinaryLeaf;
end;

function TCnBinaryTree.IsBalance: Boolean;
begin
  if Root = nil then
    Result := True
  else
    Result := Root.IsBalance;
end;

function TCnBinaryTree.IsComplete: Boolean;
var
  Queue: TQueue;
  Node: TCnBinaryLeaf;
begin
  Result := True;
  Queue := TQueue.Create;
  try
    Queue.Push(Root);
    Node := TCnBinaryLeaf(Queue.Pop);
    while Node <> nil do
    begin
      Queue.Push(Node.LeftLeaf);
      Queue.Push(Node.RightLeaf);
      Node := TCnBinaryLeaf(Queue.Pop);
    end;

    // ���й�����ȱ�������һ������ Node �� nil ʱ������һ��Ľڵ�ĺ����ӽڵ��Ѿ������˶���
    // ��ʱ�Ҷ����еķ� nil �㣬����У�˵������ȫ

    if Queue.Count = 0 then // ������б����Ľڵ㶼���� nil��˵������������������
      Exit;

    // ��ʱ���� nil �ˣ��Ҷ������ʣ��ڵ�
    while Queue.Count > 0 do
    begin
      Node := TCnBinaryLeaf(Queue.Pop);
      if Node <> nil then // ������У�������ȫ������
      begin
        Result := False;
        Exit;
      end;
    end;
  finally
    Queue.Free;
  end;
end;

function TCnBinaryTree.IsFull: Boolean;
var
  Deep: Integer;
begin
  Deep := MaxLevel + 1;
  Result := Count = Power(2, Deep - 1);
end;

procedure TCnBinaryTree.ValidateComingLeaf(AParent, AChild: TCnLeaf);
begin
  if AParent.Count >= 2 then
    raise ECnBinaryTreeException.Create('Binary TreeNode Can Only Contains 2 Child.');
end;

function TCnTree.DefaultLeafClass: TCnLeafClass;
begin
  Result := TCnLeaf;
end;

procedure TCnBinaryTree.DeleteLeftChild(AParent: TCnBinaryLeaf);
begin
  if (AParent.Tree = Self) then
    AParent.LeftLeaf := nil;
end;

procedure TCnBinaryTree.DeleteRightChild(AParent: TCnBinaryLeaf);
begin
  if (AParent.Tree = Self) then
    AParent.RightLeaf := nil;
end;

procedure TCnBinaryTree.DoInOrderTravelLeaf(ALeaf: TCnBinaryLeaf);
begin
  if Assigned(FOnInOrderTravelLeaf) then
    FOnInOrderTravelLeaf(ALeaf);
end;

procedure TCnBinaryTree.DoPostOrderTravelLeaf(ALeaf: TCnBinaryLeaf);
begin
  if Assigned(FOnPostOrderTravelLeaf) then
    FOnPostOrderTravelLeaf(ALeaf);
end;

procedure TCnBinaryTree.DoPreOrderTravelLeaf(ALeaf: TCnBinaryLeaf);
begin
  if Assigned(FOnPreOrderTravelLeaf) then
    FOnPreOrderTravelLeaf(ALeaf);
end;

procedure TCnBinaryTree.InOrderTravel;
begin
  Root.DoInOrderTravel;
end;

procedure TCnBinaryTree.PostOrderTravel;
begin
  Root.DoPostOrderTravel;
end;

procedure TCnBinaryTree.PreOrderTravel;
begin
  Root.DoPreOrderTravel;
end;

function TCnBinaryTree.GetRoot: TCnBinaryLeaf;
begin
  Result := TCnBinaryLeaf(inherited GetRoot);
end;

{$IFDEF MSWINDOWS}

procedure TCnBinaryTree.LoadFromATreeNode(ALeaf: TCnLeaf;
  ANode: TTreeNode);
var
  Leaf: TCnLeaf;
begin
  if (ANode <> nil) and (ALeaf <> nil) then
  begin
    if DoLoadFromATreeNode(ALeaf, ANode) then
    begin
      if ANode.Count > 0 then
      begin
        Leaf := AddLeftChild(ALeaf as TCnBinaryLeaf);
        LoadFromATreeNode(Leaf, ANode.Item[0]);
      end;
      if ANode.Count > 1 then
      begin
        Leaf := AddRightChild(ALeaf as TCnBinaryLeaf);
        LoadFromATreeNode(Leaf, ANode.Item[1]);
      end;
    end
    else
    begin
      ALeaf.Delete;
    end;
  end;
end;

procedure TCnBinaryTree.LoadFromTreeView(ATreeView: ComCtrls.TTreeView;
  RootNode: TTreeNode; RootLeaf: TCnBinaryLeaf);
var
  ANode: TTreeNode;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootNode <> nil) and (RootNode.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    if RootLeaf = nil then
      Self.Clear
    else
      RootLeaf.Clear;

    if ATreeView.Items.Count > 0 then
    begin
      if RootNode = nil then
        ANode := ATreeView.Items[0]
      else
        ANode := RootNode;
      // ��һ���ڵ�
      if RootLeaf = nil then
        RootLeaf := Root;

      ALeaf := AddLeftChild(RootLeaf);
      LoadFromATreeNode(ALeaf, ANode);
      if RootNode <> nil then Exit;
      // ������ RootNode ʱ�� RootNode Ϊ�������Բ����� RootNode ��ͬ��ڵ�

      ANode := ANode.GetNextSibling; // �˲�������һ����̽ڵ㣬��������
      if ANode <> nil then
      begin
        ALeaf := AddRightChild(RootLeaf);
        LoadFromATreeNode(ALeaf, ANode);
      end;
    end;
  end;
end;

procedure TCnBinaryTree.SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode);
begin
  inherited SaveToATreeNode(ALeaf, ANode);
end;

procedure TCnBinaryTree.SaveToTreeView(ATreeView: ComCtrls.TTreeView;
  RootNode: TTreeNode; RootLeaf: TCnBinaryLeaf);
begin
  inherited SaveToTreeView(ATreeView, RootNode, RootLeaf);
end;

{$ENDIF}

{$IFDEF SUPPORT_FMX}

procedure TCnBinaryTree.LoadFromATreeViewItem(ALeaf: TCnLeaf;
  AItem: TTreeViewItem);
var
  Leaf: TCnLeaf;
begin
  if (AItem <> nil) and (ALeaf <> nil) then
  begin
    if DoLoadFromATreeViewItem(ALeaf, AItem) then
    begin
      if AItem.Count > 0 then
      begin
        Leaf := AddLeftChild(ALeaf as TCnBinaryLeaf);
        LoadFromATreeViewItem(Leaf, AItem.Items[0]);
      end;
      if AItem.Count > 1 then
      begin
        Leaf := AddRightChild(ALeaf as TCnBinaryLeaf);
        LoadFromATreeViewItem(Leaf, AItem.Items[1]);
      end;
    end
    else
    begin
      ALeaf.Delete;
    end;
  end;
end;

procedure TCnBinaryTree.LoadFromTreeView(ATreeView: FMX.TreeView.TTreeView;
  RootItem: TTreeViewItem; RootLeaf: TCnBinaryLeaf);
var
  AItem: TTreeViewItem;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootItem <> nil) and (RootItem.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    if RootLeaf = nil then
      Self.Clear
    else
      RootLeaf.Clear;

    if ATreeView.GlobalCount > 0 then
    begin
      if RootItem = nil then
        AItem := ATreeView.Items[0]
      else
        AItem := RootItem;
      // ��һ���ڵ�
      if RootLeaf = nil then
        RootLeaf := Root;

      ALeaf := AddLeftChild(RootLeaf);
      LoadFromATreeViewItem(ALeaf, AItem);
      if RootItem <> nil then Exit;
      // ������ RootNode ʱ�� RootNode Ϊ�������Բ����� RootNode ��ͬ��ڵ�

      AItem := GetNextSiblingItem(AItem); // �˲�������һ����̽ڵ㣬��������
      if AItem <> nil then
      begin
        ALeaf := AddRightChild(RootLeaf);
        LoadFromATreeViewItem(ALeaf, AItem);
      end;
    end;
  end;
end;

procedure TCnBinaryTree.SaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem);
begin
  inherited SaveToATreeViewItem(ALeaf, AItem);
end;

procedure TCnBinaryTree.SaveToTreeView(ATreeView: FMX.TreeView.TTreeView;
  RootItem: TTreeViewItem; RootLeaf: TCnBinaryLeaf);
begin
  inherited SaveToTreeView(ATreeView, RootItem, RootLeaf);
end;

{$ENDIF}

function TCnBinaryTree.GetCount: Integer;
begin
  Result := Root.AllNonNilCount + 1;
end;

function TCnBinaryTree.GetHeight: Integer;
begin
  if Root = nil then
    Result := 0
  else
    Result := Root.SubTreeHeight + 1;
end;

//==============================================================================
// TCnBinaryLeaf
//==============================================================================

constructor TCnBinaryLeaf.Create(ATree: TCnTree);
begin
  inherited;
  FList.Add(nil);  // ���ӽڵ�
  FList.Add(nil);  // ���ӽڵ�
end;

procedure TCnBinaryLeaf.DoInOrderTravel;
begin
  if LeftLeaf <> nil then
    LeftLeaf.DoInOrderTravel;
  Tree.DoInOrderTravelLeaf(Self);
  if RightLeaf <> nil then
    RightLeaf.DoInOrderTravel;
end;

procedure TCnBinaryLeaf.DoPostOrderTravel;
begin
  if LeftLeaf <> nil then
    LeftLeaf.DoPostOrderTravel;
  if RightLeaf <> nil then
    RightLeaf.DoPostOrderTravel;
  Tree.DoPostOrderTravelLeaf(Self);
end;

procedure TCnBinaryLeaf.DoPreOrderTravel;
begin
  Tree.DoPreOrderTravelLeaf(Self);
  if LeftLeaf <> nil then
    LeftLeaf.DoPreOrderTravel;
  if RightLeaf <> nil then
    RightLeaf.DoPreOrderTravel;
end;

function TCnBinaryLeaf.GetLeftLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if Count > 0 then
    Result := TCnBinaryLeaf(Items[0]);
end;

function TCnBinaryLeaf.GetRightLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if Count > 1 then
    Result := TCnBinaryLeaf(Items[1]);
end;

function TCnBinaryLeaf.GetSubTreeHeight: Integer;
var
  L, R: Integer;
begin
  Result := 0;
  if Self = nil then
    Exit;

  if (LeftLeaf = nil) and (RightLeaf = nil) then
    Result := 0
  else
  begin
    if LeftLeaf = nil then
      L := 0
    else
      L := LeftLeaf.SubTreeHeight;
    if RightLeaf = nil then
      R := 0
    else
      R := RightLeaf.SubTreeHeight;

    Result := Max(L, R) + 1;
  end;
end;

function TCnBinaryLeaf.GetTree: TCnBinaryTree;
begin
  Result := TCnBinaryTree(inherited GetTree);
end;

function TCnBinaryLeaf.IsBalance: Boolean;
var
  L, R: Integer;
  LB, RB: Boolean;
begin
  L := 0;
  R := 0;
  LB := True;
  RB := True;

  if LeftLeaf <> nil then
  begin
    L := LeftLeaf.SubTreeHeight;
    LB := LeftLeaf.IsBalance;
  end;
  if RightLeaf <> nil then
  begin
    R := RightLeaf.SubTreeHeight;
    RB := RightLeaf.IsBalance;
  end;

  Result := LB and RB and ((L - R) <= 1) and ((L - R) >= -1);
end;

procedure TCnBinaryLeaf.SetLeftLeaf(const Value: TCnBinaryLeaf);
begin
  if Value <> nil then
    Assert(Value.Tree = Self.FTree);

  if (Value <> Items[0]) and (Items[0] <> nil) then
    Items[0].Delete;

  Items[0] := Value;
  if Value <> nil then
    Value.FParent := Self;
end;

procedure TCnBinaryLeaf.SetRightLeaf(const Value: TCnBinaryLeaf);
begin
  if Value <> nil then
    Assert(Value.Tree = Self.FTree);

  if (Value <> Items[1]) and (Items[1] <> nil) then
    Items[1].Delete;

  Items[1] := Value;
  if Value <> nil then
    Value.FParent := Self;
end;

function TCnTree.GetHeight: Integer;
begin
  if Root = nil then
    Result := 0
  else
    Result := Root.SubTreeHeight + 1;
end;

//==============================================================================
// TCnTrieLeaf
//==============================================================================

function TCnTrieLeaf.DoInsertChar(P: PChar): TCnTrieLeaf;
var
  C, CaseC: Char;
  I, Idx, Gt: Integer;
  Leaf: TCnTrieLeaf;
begin
  Result := nil;
  if (P = nil) or (P^ = #0) then
    Exit;

  C := P^;
  CaseC := Tree.ConvertCharWithCase(C);

  if Tree.AnsiFastMode then
  begin
    I := Ord(CaseC);
    if Items[I] = nil then // �޴���ĸ��Ӧ�ӽڵ㣬ֱ�Ӵ���
    begin
      Leaf := Tree.CreateTrieLeaf;
      Leaf.Character := CaseC;
      Items[I] := Leaf;

      if not Tree.OnlyChar then
        Leaf.Text := Leaf.Parent.Text + C;

      Inc(P);
      if P^ = #0 then
        Result := Leaf
      else
        Result := Leaf.DoInsertChar(P);
    end
    else
    begin
      Inc(P);
      if P^ = #0 then // �������ַ����Ѿ�����
        Result := nil
      else
        Result := Items[I].DoInsertChar(P);
    end;
    Exit;
  end;

  if Count = 0 then // ���ӽڵ㣬ֱ�Ӵ���
  begin
    Leaf := Tree.CreateTrieLeaf;
    Leaf.Character := CaseC;
    AddChild(Leaf);
    if not Tree.OnlyChar then
      Leaf.Text := Leaf.Parent.Text + C;

    Inc(P);
    if P^ = #0 then
      Result := Leaf
    else
      Result := Leaf.DoInsertChar(P);
    Exit;
  end;

  Idx := -1;
  Gt := -1;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Character = CaseC then
    begin
      Idx := I;
      Break;
    end
    else if Items[I].Character > CaseC then
    begin
      Gt := I;
      Break;
    end;
  end;

  if Idx >= 0 then // �ҵ�������ַ��Ľڵ�
  begin
    Inc(P);
    if P^ = #0 then // �������ַ����Ѿ�����
      Result := nil
    else
      Result := Items[Idx].DoInsertChar(P);
  end
  else // û����ַ��Ľڵ㣬Ҫ����
  begin
    Leaf := Tree.CreateTrieLeaf;
    Leaf.Character := CaseC; // �����Сд�����У��� Character �д洢��д��ĸ

    if Gt = -1 then  // û�б����ַ���Ľڵ㣬��������
      AddChild(Leaf)
    else
      InsertChild(Leaf, Gt); // ������ڵ�һ��������ַ���Ľڵ��ǰ��

    if not Tree.OnlyChar then
      Leaf.Text := Leaf.Parent.Text + C; // ��ʵ���ֱ���ԭ��

    Inc(P);
    if P^ = #0 then
      Result := Leaf
    else
      Result := Leaf.DoInsertChar(P);
  end;
end;

function TCnTrieLeaf.DoSearchChar(P: PChar): TCnTrieLeaf;
var
  CaseC: Char;
  I: Integer;
begin
  Result := nil;
  if (P = nil) or (P^ = #0) then
    Exit;

  CaseC := Tree.ConvertCharWithCase(P^);
  if Tree.AnsiFastMode then
  begin
    I := Ord(CaseC);
    if Items[I] <> nil then
    begin
      Inc(P);
      if P^ = #0 then
        Result := Items[I]
      else
        Result := Items[I].DoSearchChar(P);
    end;
  end
  else
  begin
    for I := 0 to Count - 1 do
    begin
      if Items[I].Character = CaseC then
      begin
        Inc(P);
        if P^ = #0 then
          Result := Items[I]
        else
          Result := Items[I].DoSearchChar(P);
      end;
    end;
  end;
end;

function TCnTrieLeaf.GetCharacter: Char;
begin
  Result := FCharacter;
end;

function TCnTrieLeaf.GetItems(Index: Integer): TCnTrieLeaf;
begin
  Result := TCnTrieLeaf(inherited GetItems(Index));
end;

function TCnTrieLeaf.GetTree: TCnTrieTree;
begin
  Result := TCnTrieTree(inherited GetTree);
end;

procedure TCnTrieLeaf.SetCharacter(const Value: Char);
begin
  FCharacter := Value;
end;

procedure TCnTrieLeaf.SetItems(Index: Integer; const Value: TCnTrieLeaf);
begin
  inherited SetItems(Index, Value);
end;

//==============================================================================
// TCnTrieTree �ֵ�������
//==============================================================================

function TCnTrieTree.ConvertCharWithCase(C: Char): Char;
begin
  Result := C;
  if not FCaseSensitive and
    {$IFDEF COMPILER12_UP}(Ord(C) <= $FF) and{$ENDIF}
    (AnsiChar(C) in ['a'..'z']) then
    Dec(Result, 32);
end;

constructor TCnTrieTree.Create(ACaseSensitive: Boolean; AOnlyChar: Boolean;
  AnAnsiFastMode: Boolean);
var
  I: Char;
begin
  inherited Create(TCnTrieLeaf);
  FCaseSensitive := ACaseSensitive;
  FOnlyChar := AOnlyChar;

{$IFDEF UNICODE}
  FAnsiFastMode := False;
{$ELSE}
  FAnsiFastMode := AnAnsiFastMode;
{$ENDIF}
  if FAnsiFastMode then
    for I := Low(Char) to High(Char) do // Ԥ�ȼ��� 256 ��������
      Root.FList.Add(nil);
end;

function TCnTrieTree.CreateTrieLeaf: TCnTrieLeaf;
var
  I: Char;
begin
  Result := TCnTrieLeaf(CreateLeaf(Self));
  if FAnsiFastMode then
    for I := Low(Char) to High(Char) do // Ԥ�ȼ��� 256 ��������
      Result.FList.Add(nil);
end;

function TCnTrieTree.DefaultLeafClass: TCnLeafClass;
begin
  Result := TCnTrieLeaf;
end;

function TCnTrieTree.GetRoot: TCnTrieLeaf;
begin
  Result := TCnTrieLeaf(inherited GetRoot);
end;

function TCnTrieTree.InsertString(const Str: string): TCnTrieLeaf;
begin
  Result := Root.DoInsertChar(PChar(Str));
end;

function TCnTrieTree.SearchString(const Str: string): TCnTrieLeaf;
begin
  Result := Root.DoSearchChar(PChar(Str));
end;

function TCnTrieTree.StringExists(const Str: string): Boolean;
begin
  Result := (SearchString(Str) <> nil);
end;

end.

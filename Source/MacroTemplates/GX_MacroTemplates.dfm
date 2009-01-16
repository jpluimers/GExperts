object fmMacroTemplates: TfmMacroTemplates
  Left = 263
  Top = 193
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Macro Templates'
  ClientHeight = 419
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MinHeight = 387
  Constraints.MinWidth = 353
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 584
    Height = 383
    ActivePage = tabTemplates
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object tabTemplates: TTabSheet
      Caption = 'Templates'
      object splTemplates: TSplitter
        Left = 0
        Top = 170
        Width = 576
        Height = 8
        Cursor = crVSplit
        Align = alTop
        AutoSnap = False
        Beveled = True
      end
      object pnlList: TPanel
        Left = 0
        Top = 0
        Width = 576
        Height = 170
        Align = alTop
        BevelOuter = bvNone
        Constraints.MinHeight = 170
        Constraints.MinWidth = 82
        FullRepaint = False
        TabOrder = 0
        object lvTemplates: TListView
          Left = 0
          Top = 0
          Width = 491
          Height = 170
          Align = alClient
          Columns = <
            item
              Caption = 'Name'
              MinWidth = 40
              Width = 70
            end
            item
              Caption = 'Description'
              MinWidth = 40
              Width = 283
            end
            item
              Caption = 'Shortcut'
              MinWidth = 40
              Width = 70
            end
            item
              Caption = 'Position'
              MinWidth = 40
              Width = 60
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          SortType = stText
          TabOrder = 0
          ViewStyle = vsReport
          OnClick = lvTemplatesClick
          OnDblClick = lvTemplatesDblClick
          OnSelectItem = lvTemplatesSelectItem
        end
        object Panel2: TPanel
          Left = 491
          Top = 0
          Width = 85
          Height = 170
          Align = alRight
          BevelOuter = bvNone
          FullRepaint = False
          TabOrder = 1
          object btnAdd: TButton
            Left = 11
            Top = 3
            Width = 68
            Height = 25
            Action = actAdd
            TabOrder = 0
          end
          object btnEdit: TButton
            Left = 11
            Top = 30
            Width = 68
            Height = 25
            Action = actEdit
            TabOrder = 1
          end
          object btnDelete: TButton
            Left = 11
            Top = 57
            Width = 68
            Height = 25
            Action = actDelete
            TabOrder = 2
          end
          object btnExport: TButton
            Left = 11
            Top = 111
            Width = 68
            Height = 25
            Action = actExport
            TabOrder = 4
          end
          object btnImport: TButton
            Left = 11
            Top = 84
            Width = 68
            Height = 25
            Action = actImport
            TabOrder = 3
          end
          object btnClear: TButton
            Left = 11
            Top = 138
            Width = 68
            Height = 25
            Action = actClear
            TabOrder = 5
          end
        end
      end
      object pnlMacroDetails: TPanel
        Left = 0
        Top = 178
        Width = 576
        Height = 176
        Align = alClient
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 1
        object pnlEditMacro: TPanel
          Left = 0
          Top = 0
          Width = 576
          Height = 176
          Align = alClient
          BevelOuter = bvNone
          FullRepaint = False
          TabOrder = 0
          object splUses: TSplitter
            Left = 420
            Top = 0
            Width = 5
            Height = 176
            Cursor = crHSplit
            Align = alRight
            AutoSnap = False
            Beveled = True
          end
          object pnlMacroText: TPanel
            Left = 0
            Top = 0
            Width = 420
            Height = 176
            Align = alClient
            BevelOuter = bvNone
            BorderWidth = 5
            Caption = 'Memo is created at run-time'
            FullRepaint = False
            Font.Charset = EASTEUROPE_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
          object pnlUses: TPanel
            Left = 425
            Top = 0
            Width = 151
            Height = 176
            Align = alRight
            BevelOuter = bvNone
            BorderWidth = 5
            Constraints.MinHeight = 95
            FullRepaint = False
            TabOrder = 1
            OnResize = pnlUsesResize
            object pnlUsesImplementation: TPanel
              Left = 5
              Top = 83
              Width = 141
              Height = 88
              Align = alBottom
              BevelOuter = bvNone
              FullRepaint = False
              TabOrder = 1
              object lvLocalUses: TListView
                Left = 0
                Top = 18
                Width = 141
                Height = 70
                Align = alClient
                Columns = <
                  item
                    Caption = 'Uses'
                  end>
                DragMode = dmAutomatic
                GridLines = True
                IconOptions.Arrangement = iaLeft
                IconOptions.WrapText = False
                RowSelect = True
                PopupMenu = pmUses
                TabOrder = 1
                ViewStyle = vsList
                OnExit = TemplateCodeExit
                OnDragDrop = lvGlobalUsesDragDrop
                OnDragOver = lvGlobalUsesDragOver
              end
              object pnlImplementationHeader: TPanel
                Left = 0
                Top = 0
                Width = 141
                Height = 18
                Align = alTop
                BevelOuter = bvNone
                Caption = 'Add to Implementation'
                FullRepaint = False
                TabOrder = 0
              end
            end
            object pnlUsesInterface: TPanel
              Left = 5
              Top = 5
              Width = 141
              Height = 78
              Align = alClient
              BevelOuter = bvNone
              FullRepaint = False
              TabOrder = 0
              object lvGlobalUses: TListView
                Left = 0
                Top = 16
                Width = 141
                Height = 62
                Align = alClient
                Columns = <
                  item
                    Caption = 'Uses'
                  end>
                DragMode = dmAutomatic
                GridLines = True
                IconOptions.Arrangement = iaLeft
                IconOptions.WrapText = False
                RowSelect = True
                PopupMenu = pmUses
                TabOrder = 1
                ViewStyle = vsList
                OnExit = TemplateCodeExit
                OnDragDrop = lvGlobalUsesDragDrop
                OnDragOver = lvGlobalUsesDragOver
              end
              object pnlInterfaceHeader: TPanel
                Left = 0
                Top = 0
                Width = 141
                Height = 16
                Align = alTop
                BevelOuter = bvNone
                Caption = 'Add to Interface'
                FullRepaint = False
                TabOrder = 0
              end
            end
          end
        end
      end
    end
    object tabConfig: TTabSheet
      Caption = 'Configuration'
      object grpUserDetails: TGroupBox
        Left = 16
        Top = 16
        Width = 353
        Height = 69
        Caption = 'User Details'
        TabOrder = 0
        object lblFullName: TLabel
          Left = 10
          Top = 19
          Width = 52
          Height = 14
          Caption = '&Full Name'
          FocusControl = edProgrammerName
        end
        object lblInitials: TLabel
          Left = 267
          Top = 19
          Width = 33
          Height = 14
          Caption = 'I&nitials'
          FocusControl = edInitials
        end
        object edProgrammerName: TEdit
          Left = 10
          Top = 36
          Width = 243
          Height = 22
          TabOrder = 0
          OnChange = edProgrammerNameChange
        end
        object edInitials: TEdit
          Left = 267
          Top = 36
          Width = 68
          Height = 22
          TabOrder = 1
        end
      end
      object grpSequenceNumber: TGroupBox
        Left = 16
        Top = 97
        Width = 353
        Height = 65
        Caption = 'Sequence Number'
        TabOrder = 1
        object lblCurrentValue: TLabel
          Left = 13
          Top = 30
          Width = 75
          Height = 14
          Caption = 'Current &Value'
          FocusControl = edGenValue
        end
        object edGenValue: TEdit
          Left = 104
          Top = 26
          Width = 105
          Height = 22
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 0
        end
        object btnChange: TButton
          Left = 216
          Top = 24
          Width = 75
          Height = 25
          Caption = '&Change...'
          TabOrder = 1
          OnClick = btnChangeClick
        end
      end
      object gbxExpansion: TGroupBox
        Left = 16
        Top = 177
        Width = 353
        Height = 121
        Caption = 'Template Expansion'
        TabOrder = 2
        object lblExpandDelay: TLabel
          Left = 16
          Top = 44
          Width = 29
          Height = 14
          Caption = '&Delay'
          FocusControl = tbExpandDelay
        end
        object lb01Sec: TLabel
          Left = 16
          Top = 92
          Width = 29
          Height = 14
          Caption = '0 sec'
        end
        object lbl2Sec: TLabel
          Left = 264
          Top = 92
          Width = 29
          Height = 14
          Caption = '2 sec'
        end
        object lbl1Sec: TLabel
          Left = 140
          Top = 92
          Width = 29
          Height = 14
          Caption = '1 sec'
        end
        object cbExpandWithChar: TCheckBox
          Left = 16
          Top = 22
          Width = 332
          Height = 17
          Caption = 'Auto-e&xpand typed templates after pressing space'
          TabOrder = 0
        end
        object tbExpandDelay: TTrackBar
          Left = 8
          Top = 60
          Width = 289
          Height = 33
          Max = 20
          Orientation = trHorizontal
          Frequency = 1
          Position = 0
          SelEnd = 0
          SelStart = 0
          TabOrder = 1
          TickMarks = tmBottomRight
          TickStyle = tsAuto
        end
      end
    end
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 383
    Width = 584
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinHeight = 31
    Constraints.MinWidth = 237
    FullRepaint = False
    TabOrder = 1
    object pnlFooterButtons: TPanel
      Left = 320
      Top = 0
      Width = 264
      Height = 36
      Align = alRight
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object btnOK: TButton
        Left = 15
        Top = 6
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TButton
        Left = 99
        Top = 6
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnHelp: TButton
        Left = 183
        Top = 6
        Width = 75
        Height = 25
        Cancel = True
        Caption = '&Help'
        TabOrder = 2
        OnClick = btnHelpClick
      end
    end
  end
  object pmMacros: TPopupMenu
    Images = dmSharedImages.Images
    OnPopup = pmMacrosPopup
    Left = 488
    Top = 304
    object PROCNAMEMacro1: TMenuItem
      Caption = 'PROCNAME Macro'
      OnClick = InsertMacroClick
    end
    object PROCCLASS1: TMenuItem
      Caption = 'METHODCLASS Macro'
      OnClick = InsertMacroClick
    end
    object CLASSMacro1: TMenuItem
      Caption = 'CLASS Macro'
      OnClick = InsertMacroClick
    end
    object IDENTMacro1: TMenuItem
      Caption = 'IDENT Macro'
      OnClick = InsertMacroClick
    end
    object INTERFACEMacro1: TMenuItem
      Caption = 'INTERFACE Macro'
      OnClick = InsertMacroClick
    end
    object Procedurefunctionheader1: TMenuItem
      Caption = 'Procedure / function header'
      object ARGUMENTSMacro1: TMenuItem
        Caption = 'ARGUMENTS Macro'
        OnClick = InsertMacroClick
      end
      object RESULTMacro1: TMenuItem
        Caption = 'RESULT Macro'
        OnClick = InsertMacroClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object BEGINPARAMLISTMacro1: TMenuItem
        Caption = 'BEGINPARAMLIST Macro'
        OnClick = InsertMacroClick
      end
      object ENDPARAMLISTMacro1: TMenuItem
        Caption = 'ENDPARAMLIST Macro'
        OnClick = InsertMacroClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object PARAMNAMEMacro1: TMenuItem
        Caption = 'PARAMNAME Macro'
        OnClick = InsertMacroClick
      end
      object PARAMTYPEMacro1: TMenuItem
        Caption = 'PARAMTYPE Macro'
        OnClick = InsertMacroClick
      end
      object PARAMDEFMacro1: TMenuItem
        Caption = 'PARAMDEF Macro'
        OnClick = InsertMacroClick
      end
    end
    object N1: TMenuItem
      Caption = '-'
      OnClick = InsertMacroClick
    end
    object UNITMacro1: TMenuItem
      Caption = 'UNIT Macro'
      OnClick = InsertMacroClick
    end
    object ProjectMacros1: TMenuItem
      Caption = 'Project Macros'
      object PROJECTGROUPNAMEMacro1: TMenuItem
        Caption = 'PROJECTGROUPNAME Macro'
        OnClick = InsertMacroClick
      end
      object PROJECTGROUPDIRMacro1: TMenuItem
        Caption = 'PROJECTGROUPDIR Macro'
        OnClick = InsertMacroClick
      end
      object PROJECTDIRMacro1: TMenuItem
        Caption = 'PROJECTDIR Macro'
        OnClick = InsertMacroClick
      end
      object PROJECTNAME1: TMenuItem
        Caption = 'PROJECTNAME Macro'
        OnClick = InsertMacroClick
      end
    end
    object VersionMacros1: TMenuItem
      Caption = 'Version Macros'
      object VERPRODUCTVERSIONMacro1: TMenuItem
        Caption = 'VERPRODUCTVERSION Macro'
        OnClick = InsertMacroClick
      end
      object VERFILEVERSION1: TMenuItem
        Caption = 'VERFILEVERSION Macro'
        OnClick = InsertMacroClick
      end
      object VERMAJORMacro1: TMenuItem
        Caption = 'VERMAJOR Macro'
        OnClick = InsertMacroClick
      end
      object VERMINORMacro1: TMenuItem
        Caption = 'VERMINOR Macro'
        OnClick = InsertMacroClick
      end
      object VERRELEASEMacro1: TMenuItem
        Caption = 'VERRELEASE Macro'
        OnClick = InsertMacroClick
      end
      object VERBUILD1: TMenuItem
        Caption = 'VERBUILD Macro'
        OnClick = InsertMacroClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object VERPRODUCTNAME1: TMenuItem
        Caption = 'VERPRODUCTNAME Macro'
        OnClick = InsertMacroClick
      end
      object VERINTERNALNAME1: TMenuItem
        Caption = 'VERINTERNALNAME Macro'
        OnClick = InsertMacroClick
      end
      object VERFILEDESCRIPTIONMacro1: TMenuItem
        Caption = 'VERFILEDESCRIPTION Macro'
        OnClick = InsertMacroClick
      end
    end
    object N2: TMenuItem
      Caption = '-'
      OnClick = InsertMacroClick
    end
    object DATETIMEMacro1: TMenuItem
      Caption = 'DATETIME Macro'
      OnClick = InsertMacroClick
    end
    object DATE1: TMenuItem
      Caption = 'DATE Macro'
      OnClick = InsertMacroClick
    end
    object DateMacros1: TMenuItem
      Caption = 'Date Macros'
      object YEARMacro1: TMenuItem
        Caption = 'YEAR Macro'
        OnClick = InsertMacroClick
      end
      object MONTHMacro1: TMenuItem
        Caption = 'MONTH Macro'
        OnClick = InsertMacroClick
      end
      object DAYMacro1: TMenuItem
        Caption = 'DAY Macro'
        OnClick = InsertMacroClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MONTHSHORTNAMEMacro1: TMenuItem
        Caption = 'MONTHSHORTNAME Macro'
        OnClick = InsertMacroClick
      end
      object MONTHLONGNAMEMacro1: TMenuItem
        Caption = 'MONTHLONGNAME Macro'
        OnClick = InsertMacroClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object DAYSHORTNAMEMacro1: TMenuItem
        Caption = 'DAYSHORTNAME Macro'
        OnClick = InsertMacroClick
      end
      object DAYLONGNAMEMacro1: TMenuItem
        Caption = 'DAYLONGNAME Macro'
        OnClick = InsertMacroClick
      end
    end
    object TimeMacros1: TMenuItem
      Caption = 'Time Macros'
      object HOURMacro1: TMenuItem
        Caption = 'HOUR Macro'
        OnClick = InsertMacroClick
      end
      object MINUTEMacro1: TMenuItem
        Caption = 'MINUTE Macro'
        OnClick = InsertMacroClick
      end
      object SECONDMacro1: TMenuItem
        Caption = 'SECOND Macro'
        OnClick = InsertMacroClick
      end
    end
    object N3: TMenuItem
      Caption = '-'
      OnClick = InsertMacroClick
    end
    object UserIDMacros1: TMenuItem
      Caption = 'User Macros'
      object USER1: TMenuItem
        Caption = 'USER Macro'
        OnClick = InsertMacroClick
      end
      object PROGRAMMERNAMEMacro1: TMenuItem
        Caption = 'PROGRAMMERNAME Macro'
        OnClick = InsertMacroClick
      end
      object PROGRAMMERINITIALSMacro1: TMenuItem
        Caption = 'PROGRAMMERINITIALS Macro'
        OnClick = InsertMacroClick
      end
    end
    object Other1: TMenuItem
      Caption = 'Other'
      object PGENMacro1: TMenuItem
        Caption = 'PGEN Macro'
        OnClick = InsertMacroClick
      end
      object INPUTVARMacro1: TMenuItem
        Caption = 'INPUTVAR Macro'
        OnClick = INPUTVARMacro1Click
      end
      object BEFORE1: TMenuItem
        Caption = 'BEFORE Macro'
        OnClick = InsertMacroClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object CLIPBOARDMacro1: TMenuItem
        Caption = 'CLIPBOARD Macro'
        OnClick = InsertMacroClick
      end
      object SELECTIONMacro1: TMenuItem
        Caption = 'SELECTION Macro'
        OnClick = InsertMacroClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object CLIPBOARD1Macro1: TMenuItem
        Caption = 'CLIPBOARD1 Macro'
        OnClick = InsertMacroClick
      end
      object COPYSELECTIONMacro1: TMenuItem
        Caption = 'COPYSELECTION Macro'
        OnClick = InsertMacroClick
      end
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object miCopy: TMenuItem
      Action = actEditCopy
      ImageIndex = 6
    end
    object miCut: TMenuItem
      Action = actEditCut
      ImageIndex = 5
    end
    object miPaste: TMenuItem
      Action = actEditPaste
      ImageIndex = 7
    end
    object N16: TMenuItem
      Caption = '-'
    end
    object mitEditorHighlighting: TMenuItem
      Caption = 'Syntax &Highlighting'
      ImageIndex = 18
      object mitNone: TMenuItem
        Action = actSyntaxNone
        RadioItem = True
      end
      object mitPas: TMenuItem
        Tag = 1
        Action = actSyntaxPas
        RadioItem = True
      end
      object mitCPP: TMenuItem
        Tag = 2
        Action = actSyntaxCPP
        RadioItem = True
      end
      object mitHTML: TMenuItem
        Tag = 3
        Action = actSyntaxHTML
        RadioItem = True
      end
      object mitSQL: TMenuItem
        Tag = 4
        Action = actSyntaxSQL
        RadioItem = True
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'Template Files (*.xml)|*.xml|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a template import file'
    Left = 456
    Top = 116
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'Template Files (*.xml)|*.xml|All files (*.*)|*.*'
    Title = 'Export templates to'
    Left = 456
    Top = 60
  end
  object pmUses: TPopupMenu
    Images = dmSharedImages.Images
    OnPopup = pmUsesPopup
    Left = 428
    Top = 320
    object miAddToUses: TMenuItem
      Caption = 'Add to Uses'
      ImageIndex = 41
      OnClick = miAddToUsesClick
    end
    object miDeleteUses: TMenuItem
      Caption = 'Delete'
      ImageIndex = 42
      OnClick = miDeleteUsesClick
    end
  end
  object Actions: TActionList
    OnUpdate = ActionsUpdate
    Left = 476
    Top = 252
    object actSyntaxNone: TAction
      Category = 'Syntax'
      Caption = '&None'
      OnExecute = SetHighlighterClick
    end
    object actSyntaxPas: TAction
      Tag = 1
      Category = 'Syntax'
      Caption = '&Pascal'
      OnExecute = SetHighlighterClick
    end
    object actSyntaxCPP: TAction
      Tag = 2
      Category = 'Syntax'
      Caption = '&CPP'
      OnExecute = SetHighlighterClick
    end
    object actSyntaxHTML: TAction
      Tag = 3
      Category = 'Syntax'
      Caption = '&HTML'
      OnExecute = SetHighlighterClick
    end
    object actSyntaxSQL: TAction
      Tag = 4
      Category = 'Syntax'
      Caption = '&SQL'
      OnExecute = SetHighlighterClick
    end
    object actEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy'
      ImageIndex = 1
      ShortCut = 16451
      OnExecute = miCopyClick
    end
    object actEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut'
      ImageIndex = 0
      ShortCut = 16472
      OnExecute = miCutClick
    end
    object actEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste'
      ImageIndex = 2
      ShortCut = 16470
      OnExecute = miPasteClick
    end
    object actInsertCursorPos: TAction
      Category = 'InsertPos'
      Caption = 'Cursor Position'
      OnExecute = actInsertExecute
    end
    object actInsertUnitStart: TAction
      Tag = 1
      Category = 'InsertPos'
      Caption = 'Start of Unit'
      OnExecute = actInsertExecute
    end
    object actInsertLineStart: TAction
      Tag = 2
      Category = 'InsertPos'
      Caption = 'Start of Line'
      OnExecute = actInsertExecute
    end
    object actInsertLineEnd: TAction
      Tag = 3
      Category = 'InsertPos'
      Caption = 'End of Line'
      OnExecute = actInsertExecute
    end
    object actAdd: TAction
      Caption = '&Add...'
      OnExecute = actAddExecute
    end
    object actEdit: TAction
      Caption = '&Edit...'
      OnExecute = actEditExecute
    end
    object actDelete: TAction
      Caption = '&Delete'
      OnExecute = actDeleteExecute
    end
    object actImport: TAction
      Caption = '&Import...'
      OnExecute = actImportExecute
    end
    object actExport: TAction
      Caption = 'E&xport...'
      OnExecute = actExportExecute
    end
    object actClear: TAction
      Caption = 'C&lear...'
      OnExecute = actClearExecute
    end
  end
end

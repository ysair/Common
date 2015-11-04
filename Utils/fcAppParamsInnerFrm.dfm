object fcAppParamsInnerForm: TfcAppParamsInnerForm
  Left = 0
  Top = 0
  Caption = 'fcAppParamsInnerForm'
  ClientHeight = 188
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object tmrDispatchParams: TTimer
    Interval = 50
    OnTimer = tmrDispatchParamsTimer
    Left = 120
    Top = 48
  end
end

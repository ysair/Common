{*******************************************************
 作者：郑志强
 Email:hnstronger@gmail.com
 建立日期：2006-7-10
*******************************************************}
unit uDevExpressToChinese;

interface
uses
  cxClasses, cxGridStrs, cxExportStrs, cxLibraryStrs, cxGridPopupMenuConsts,
  {dxExtCtrlsStrs, dxPSRes,} cxFilterConsts, cxDataConsts,
  cxFilterControlStrs, cxEditConsts;

procedure toChinese;

implementation

procedure toChinese;
begin
 //cxExportStrs.pas
 //------------------------------------------------------------------
  cxSetResourceString(@scxUnsupportedExport, '不提供的输出类型: %1');
  //'Unsupported export type: %1');
  cxSetResourceString(@scxStyleManagerKill, '样式管理器正在被使用或不能释放');
  //'The Style Manager is currently being used elsewhere and can not be released at this stage');
  cxSetResourceString(@scxStyleManagerCreate, '不能创建样式管理器');
  //'Can''t create style manager');

  cxSetResourceString(@scxExportToHtml, '输出到网页 (*.html)');
  //'Export to Web page (*.html)');
  cxSetResourceString(@scxExportToXml, '输出到XML文档 (*.xml)');
  //'Export to XML document (*.xml)');
  cxSetResourceString(@scxExportToText, '输出到文本文件 (*.txt)');
  //'Export to text format (*.txt)');

  cxSetResourceString(@scxEmptyExportCache, '输出缓冲为空');
  //'Export cache is empty');
  cxSetResourceString(@scxIncorrectUnion, '不正确的单元组合');
  //'Incorrect union of cells');
  cxSetResourceString(@scxIllegalWidth, '非法的列宽');
  //'Illegal width of the column');
  cxSetResourceString(@scxInvalidColumnRowCount, '无效的行数或列数');
  //'Invalid column or row count');
  cxSetResourceString(@scxIllegalHeight, '非法的行高');
  //'Illegal height of the row');
  cxSetResourceString(@scxInvalidColumnIndex, '列标 %d 超出范围');
  //'The column index %d out of bounds');
  cxSetResourceString(@scxInvalidRowIndex, '行号 %d 超出范围');
  //'The row index %d out of bounds');
  cxSetResourceString(@scxInvalidStyleIndex, '无效的样式索引 %d');
  //'Invalid style index %d');

  cxSetResourceString(@scxExportToExcel, '输出到 MS Excel文件 (*.xls)');
  //'Export to MS Excel (*.xls)');
  cxSetResourceString(@scxWorkbookWrite, '写 XLS 文件出错');
  cxSetResourceString(@scxInvalidCellDimension, '无效的单元维度');
  //'Invalid cell dimension');
  cxSetResourceString(@scxBoolTrue, '真');
  //'True');
  cxSetResourceString(@scxBoolFalse, '假');
  //'False';
  //-------------------------------------------------------------------------

  //cxLibraryStrs.pas
  //-------------------------------------------------------------------------
  cxSetResourceString(@scxCantCreateRegistryKey, '不能创建registry key: \%s');
  //tt8//'Can''t create the registry key: \%s');
{$IFNDEF DELPHI5}
  cxSetResourceString(@scxInvalidPropertyElement, '无效的属性元素: %s');
  //tt8//'Invalid property element: %s');
{$ENDIF}
  cxSetResourceString(@scxConverterCantCreateStyleRepository, '不能创建Style Repository');
  //tt8//'Can''t create the Style Repository');
  //-------------------------------------------------------------------------

  //cxGridStrs.pas
  //-------------------------------------------------------------------------
  cxSetResourceString(@scxGridRecursiveLevels, '您不能创建递归层');
  //'You cannot create recursive levels');

  cxSetResourceString(@scxGridDeletingConfirmationCaption, '提示');
  //'Confirm');
  cxSetResourceString(@scxGridDeletingFocusedConfirmationText, '删除数据吗?');
  //'Delete record?');
  cxSetResourceString(@scxGridDeletingSelectedConfirmationText, '删除所有选定的记录吗?');

  cxSetResourceString(@scxGridNoDataInfoText, '<没有任何记录>');

  cxSetResourceString(@scxGridNewItemRowInfoText, '单击此处添加一新行');
  //'Click here to add a new row');

  cxSetResourceString(@scxGridFilterIsEmpty, '<数据过滤条件为空>');
  //'<Filter is Empty>');

  cxSetResourceString(@scxGridCustomizationFormCaption, '定制');
  //'Customization');
  cxSetResourceString(@scxGridCustomizationFormColumnsPageCaption, '列');
  cxSetResourceString(@scxGridGroupByBoxCaption, '把列标题拖放到此处使记录按此列进行分组');
  //'Drag a column header here to group by that column');
  cxSetResourceString(@scxGridFilterCustomizeButtonCaption, '定制...');
  //'Customize...');
  cxSetResourceString(@scxGridColumnsQuickCustomizationHint, '点击选择可视列');
  // 'Click here to select visible columns');

  cxSetResourceString(@scxGridCustomizationFormBandsPageCaption, '区域');
  // 'Bands');
  cxSetResourceString(@scxGridBandsQuickCustomizationHint, '点击选择可视区域');
  //'Click here to select visible bands');

  cxSetResourceString(@scxGridCustomizationFormRowsPageCaption, '行'); // 'Rows');

  cxSetResourceString(@scxGridConverterIntermediaryMissing, '缺少一个中间组件!'#13#10'请添加一个 %s 组件到窗体.');
  //'Missing an intermediary component!'#13#10'Please add a %s component to the form.');
  cxSetResourceString(@scxGridConverterNotExistGrid, 'cxGrid 不存在');
  //'cxGrid does not exist');
  cxSetResourceString(@scxGridConverterNotExistComponent, '组件不存在');
  //'Component does not exist');
  cxSetResourceString(@scxImportErrorCaption, '导入错误');
  //'Import error');

  cxSetResourceString(@scxNotExistGridView, 'Grid 视图不存在');
  //'Grid view does not exist');
  cxSetResourceString(@scxNotExistGridLevel, '活动的 grid 层不存在');
  //'Active grid level does not exist');
  cxSetResourceString(@scxCantCreateExportOutputFile, '不能建立导出文件');
  //'Can''t create the export output file');

  cxSetResourceString(@cxSEditRepositoryExtLookupComboBoxItem,
    'ExtLookupComboBox|Represents an ultra-advanced lookup using the QuantumGrid as its drop down control');

  cxSetResourceString(@scxGridChartValueHintFormat, '%s for %s is %s'); // series display text, category, value
  //-------------------------------------------------------------------------

  //cxGridPopupMenuConsts.pas
  //-------------------------------------------------------------------------
  cxSetResourceString(@cxSGridNone, '无');
  //'None');

//Header popup menu captions

  cxSetResourceString(@cxSGridSortColumnAsc, '升序');
  //'Sort Ascending');
  cxSetResourceString(@cxSGridSortColumnDesc, '降序');
  //'Sort Descending');
  cxSetResourceString(@cxSGridClearSorting, '清除排序');

  //'Clear Sorting');
  cxSetResourceString(@cxSGridGroupByThisField, '按照此字段分组');
  //'Group By This Field');
  cxSetResourceString(@cxSGridRemoveThisGroupItem, '从该组删除');
  //'Remove from grouping');
  cxSetResourceString(@cxSGridGroupByBox, '显示/隐藏分组框');
  //'Group By Box');
  cxSetResourceString(@cxSGridAlignmentSubMenu, '对齐');
  //'Alignment');
  cxSetResourceString(@cxSGridAlignLeft, '左对齐');
  //'Align Left');
  cxSetResourceString(@cxSGridAlignRight, '右对齐');
  //'Align Right');
  cxSetResourceString(@cxSGridAlignCenter, '居中对齐');
  //'Align Center');
  cxSetResourceString(@cxSGridRemoveColumn, '删除此列');
  //'Remove This Column');
  cxSetResourceString(@cxSGridFieldChooser, '选择字段');
  //'Field Chooser');
  cxSetResourceString(@cxSGridBestFit, '适合列宽');
  //'Best Fit');
  cxSetResourceString(@cxSGridBestFitAllColumns, '适合列宽 (所有列)');
  //'Best Fit (all columns)');
  cxSetResourceString(@cxSGridShowFooter, '脚注');
  //'Footer');
  cxSetResourceString(@cxSGridShowGroupFooter, '组脚注');
  //'Group Footers');

//Footer popup menu captions

  cxSetResourceString(@cxSGridSumMenuItem, '合计');
  //'Sum');
  cxSetResourceString(@cxSGridMinMenuItem, '最小');
  //'Min');
  cxSetResourceString(@cxSGridMaxMenuItem, '最大');
  //'Max');
  cxSetResourceString(@cxSGridCountMenuItem, '计数');
  //'Count');
  cxSetResourceString(@cxSGridAvgMenuItem, '平均');
  //'Average');
  cxSetResourceString(@cxSGridNoneMenuItem, '无');
  //'None');

  //-------------------------------------------------------------------------

  //dxExtCtrlsStrs.pas
  //-------------------------------------------------------------------------
  {cxSetResourceString(@sdxAutoColorText, '自动');
  //tt8//'Auto');
  cxSetResourceString(@sdxCustomColorText, '定制...');
  //tt8//'Custom...');

  cxSetResourceString(@sdxSysColorScrollBar, '滚动条');
  //tt8//'ScrollBar');
  cxSetResourceString(@sdxSysColorBackground, '背景');
  //tt8//'Background');
  cxSetResourceString(@sdxSysColorActiveCaption, '动作标题');
  //tt8//'Active Caption');
  cxSetResourceString(@sdxSysColorInactiveCaption, '不活动标题');
  //tt8//'Inactive Caption');
  cxSetResourceString(@sdxSysColorMenu, '菜单');
  //tt8//'Menu');
  cxSetResourceString(@sdxSysColorWindow, '窗口');
  //tt8//'Window');
  cxSetResourceString(@sdxSysColorWindowFrame, '窗口框架');
  //tt8//'Window Frame');
  cxSetResourceString(@sdxSysColorMenuText, '菜单文本');
  //tt8//'Menu Text');
  cxSetResourceString(@sdxSysColorWindowText, '窗口文本t');
  //tt8//'Window Text');
  cxSetResourceString(@sdxSysColorCaptionText, '标题文本');
  //tt8//'Caption Text');
  cxSetResourceString(@sdxSysColorActiveBorder, '活动边框');
  //tt8//'Active Border');
  cxSetResourceString(@sdxSysColorInactiveBorder, '不活动边框');
  //tt8//'Inactive Border');
  cxSetResourceString(@sdxSysColorAppWorkSpace, '程序工作空间');
  //tt8//'App Workspace');
  cxSetResourceString(@sdxSysColorHighLight, '高亮');
  //tt8//'Highlight');
  cxSetResourceString(@sdxSysColorHighLighText, '高亮文本');
  //tt8//'Highlight Text');
  cxSetResourceString(@sdxSysColorBtnFace, '按钮表面');
  //tt8//'Button Face');
  cxSetResourceString(@sdxSysColorBtnShadow, '按钮阴影');
  //tt8//'Button Shadow');
  cxSetResourceString(@sdxSysColorGrayText, '灰色文本');
  //tt8//'Gray Text');
  cxSetResourceString(@sdxSysColorBtnText, '按钮文本');
  //tt8//'Button Text');
  cxSetResourceString(@sdxSysColorInactiveCaptionText, '不活动的标题文本');
  //tt8//'Inactive Caption Text');
  cxSetResourceString(@sdxSysColorBtnHighligh, '按钮高亮');
  //tt8//'Button Highlight');
  cxSetResourceString(@sdxSysColor3DDkShadow, '3DDk 阴影');
  //tt8//'3DDk Shadow');
  cxSetResourceString(@sdxSysColor3DLight, '3D 明亮');
  //tt8//'3DLight');
  cxSetResourceString(@sdxSysColorInfoText, '信息文本');
  //tt8//'Info Text');
  cxSetResourceString(@sdxSysColorInfoBk, '信息背景');
  //tt8//'InfoBk');

  cxSetResourceString(@sdxPureColorBlack, '黑');
  //tt8//'Black');
  cxSetResourceString(@sdxPureColorRed, '红');
  //tt8//'Red');
  cxSetResourceString(@sdxPureColorLime, '橙');
  //tt8//'Lime');
  cxSetResourceString(@sdxPureColorYellow, '黄');
  //tt8//'Yellow');
  cxSetResourceString(@sdxPureColorGreen, '绿');
  //tt8//'Green');
  cxSetResourceString(@sdxPureColorTeal, '青');
  //tt8//'Teal');
  cxSetResourceString(@sdxPureColorAqua, '浅绿');
  //tt8//'Aqua');
  cxSetResourceString(@sdxPureColorBlue, '蓝');
  //tt8//'Blue');
  cxSetResourceString(@sdxPureColorWhite, '白');
  //tt8//'White');
  cxSetResourceString(@sdxPureColorOlive, '浅绿');
  //tt8//'Olive');
  cxSetResourceString(@sdxPureColorMoneyGreen, '黄绿');
  //tt8//'Money Green');
  cxSetResourceString(@sdxPureColorNavy, '藏青');
  //tt8//'Navy');
  cxSetResourceString(@sdxPureColorSkyBlue, '天蓝');
  //tt8//'Sky Blue');
  cxSetResourceString(@sdxPureColorGray, '灰');
  //tt8//'Gray');
  cxSetResourceString(@sdxPureColorMedGray, '中灰');
  //tt8//'Medium Gray');
  cxSetResourceString(@sdxPureColorSilver, '银');
  //tt8//'Silver');
  cxSetResourceString(@sdxPureColorMaroon, '茶色');
  //tt8//'Maroon');
  cxSetResourceString(@sdxPureColorPurple, '紫');
  //tt8//'Purple');
  cxSetResourceString(@sdxPureColorFuchsia, '紫红');
  //tt8//'Fuchsia');
  cxSetResourceString(@sdxPureColorCream, '米色');
  //tt8//'Cream');

  cxSetResourceString(@sdxBrushStyleSolid, '固体');
  //tt8//'Solid');
  cxSetResourceString(@sdxBrushStyleClear, '清除');
  //tt8//'Clear');
  cxSetResourceString(@sdxBrushStyleHorizontal, '水平');
  //tt8//'Horizontal');
  cxSetResourceString(@sdxBrushStyleVertical, '垂直');
  //tt8//'Vertical');
  cxSetResourceString(@sdxBrushStyleFDiagonal, 'F斜纹');
  //tt8//'FDiagonal');
  cxSetResourceString(@sdxBrushStyleBDiagonal, 'B斜纹');
  //tt8//'BDiagonal');
  cxSetResourceString(@sdxBrushStyleCross, '交叉');
  //tt8//'Cross');
  cxSetResourceString(@sdxBrushStyleDiagCross, '反交叉');
  //tt8//'DiagCross');   //}
  //-------------------------------------------------------------------------

  //cxFilterConsts.pas
  //-------------------------------------------------------------------------
  // base operators
  cxSetResourceString(@cxSFilterOperatorEqual, '等于');
  //'equals');
  cxSetResourceString(@cxSFilterOperatorNotEqual, '不等于');
  //'does not equal');
  cxSetResourceString(@cxSFilterOperatorLess, '小于');
  //'is less than');
  cxSetResourceString(@cxSFilterOperatorLessEqual, '小于等于');
  //'is less than or equal to');
  cxSetResourceString(@cxSFilterOperatorGreater, '大于');
  //'is greater than');
  cxSetResourceString(@cxSFilterOperatorGreaterEqual, '大于等于');
  //'is greater than or equal to');
  cxSetResourceString(@cxSFilterOperatorLike, '相似');
  //'like');
  cxSetResourceString(@cxSFilterOperatorNotLike, '不相似');
  //'not like');
  cxSetResourceString(@cxSFilterOperatorBetween, '在...之间');
  //'between');
  cxSetResourceString(@cxSFilterOperatorNotBetween, '不在...之间');
  //'not between');
  cxSetResourceString(@cxSFilterOperatorInList, '包含');
  //'in');
  cxSetResourceString(@cxSFilterOperatorNotInList, '不包含');
  //'not in');

  cxSetResourceString(@cxSFilterOperatorYesterday, '昨天');
  //'is yesterday');
  cxSetResourceString(@cxSFilterOperatorToday, '今天');
  //'is today');
  cxSetResourceString(@cxSFilterOperatorTomorrow, '明天');
  //'is tomorrow');

  cxSetResourceString(@cxSFilterOperatorLastWeek, '前一周');
  //'is last week');
  cxSetResourceString(@cxSFilterOperatorLastMonth, '前一月');
  //'is last month');
  cxSetResourceString(@cxSFilterOperatorLastYear, '前一年');
  //'is last year');

  cxSetResourceString(@cxSFilterOperatorThisWeek, '本周');
  //'is this week');
  cxSetResourceString(@cxSFilterOperatorThisMonth, '本月');
  //'is this month');
  cxSetResourceString(@cxSFilterOperatorThisYear, '本年');
  //'is this year');

  cxSetResourceString(@cxSFilterOperatorNextWeek, '下一周');
  //'is next week');
  cxSetResourceString(@cxSFilterOperatorNextMonth, '下一月');
  //'is next month');
  cxSetResourceString(@cxSFilterOperatorNextYear, '下一年');
  //'is next year');

  cxSetResourceString(@cxSFilterAndCaption, '并且');
  //'and');
  cxSetResourceString(@cxSFilterOrCaption, '或者');
  //'or');
  cxSetResourceString(@cxSFilterNotCaption, '非');
  //'not');
  cxSetResourceString(@cxSFilterBlankCaption, '空');
  //'blank');

  // derived
  cxSetResourceString(@cxSFilterOperatorIsNull, '为空');
  //'is blank');
  cxSetResourceString(@cxSFilterOperatorIsNotNull, '不为空');
  //'is not blank');
  cxSetResourceString(@cxSFilterOperatorBeginsWith, '起始于');
  //'begins with');
  cxSetResourceString(@cxSFilterOperatorDoesNotBeginWith, '不起始于');
  //'does not begin with');
  cxSetResourceString(@cxSFilterOperatorEndsWith, '结束于');
  //'ends with');
  cxSetResourceString(@cxSFilterOperatorDoesNotEndWith, '不结束于');
  //'does not end with');
  cxSetResourceString(@cxSFilterOperatorContains, '包含');
  //'contains');
  cxSetResourceString(@cxSFilterOperatorDoesNotContain, '不包含');
  //'does not contain');
  // filter listbox's values
  cxSetResourceString(@cxSFilterBoxAllCaption, '(全部显示)');
  //'(All)');
  cxSetResourceString(@cxSFilterBoxCustomCaption, '(定制过滤...)');
  //'(Custom...)');
  cxSetResourceString(@cxSFilterBoxBlanksCaption, '(为空)');
  //'(Blanks)');
  cxSetResourceString(@cxSFilterBoxNonBlanksCaption, '(不为空)');
  //'(NonBlanks)');
  //-------------------------------------------------------------------------

  //cxDataConsts.pas
  //-------------------------------------------------------------------------
   // Data
  cxSetResourceString(@cxSDataReadError, '输入流错误');
  //tt8//'Stream read error');
  cxSetResourceString(@cxSDataWriteError, '输出流错误');
  //tt8//'Stream write error');
  cxSetResourceString(@cxSDataItemExistError, '项目已经存在');
  //tt8//'Item already exists');
  cxSetResourceString(@cxSDataRecordIndexError, '记录索引超出范围');
  //tt8//'RecordIndex out of range');
  cxSetResourceString(@cxSDataItemIndexError, '项目索引超出范围');
  //tt8//'ItemIndex out of range');
  cxSetResourceString(@cxSDataProviderModeError, '数据提供者不提供该操作');
  //tt8//'This operation is not supported in provider mode');
  cxSetResourceString(@cxSDataInvalidStreamFormat, '错误的流格式');
  //tt8//'Invalid stream format');
  cxSetResourceString(@cxSDataRowIndexError, '行索引超出范围');
  //tt8//'RowIndex out of range');
//  cxSetResourceString(@cxSDataRelationItemExistError,'关联项目不存在');
  //tt8//'Relation Item already exists');
//  cxSetResourceString(@cxSDataRelationCircularReference,'细节数据控制器循环引用');
  //tt8//'Circular Reference on Detail DataController');
//  cxSetResourceString(@cxSDataRelationMultiReferenceError,'引用细节数据控制器已经存在');
  //tt8//'Reference on Detail DataController already exists');
  cxSetResourceString(@cxSDataCustomDataSourceInvalidCompare, 'GetInfoForCompare 没有实现');
  //tt8//'GetInfoForCompare not implemented');
  // DB
//  cxSDBDataSetNil,'数据集为空');
  //tt8//'DataSet is nil');
  cxSetResourceString(@cxSDBDetailFilterControllerNotFound, '细节数据控制器没有发现');
  //tt8//'DetailFilterController not found');
  cxSetResourceString(@cxSDBNotInGridMode, '数据控制器不在表格(Grid)模式e');
  //tt8//'DataController not in GridMode');
  cxSetResourceString(@cxSDBKeyFieldNotFound, 'Key Field not found');
  //-------------------------------------------------------------------------

  //cxFilterControlStrs.pas
  //-------------------------------------------------------------------------
  // cxFilterBoolOperator
  cxSetResourceString(@cxSFilterBoolOperatorAnd, '并且'); // all
  //'AND');        // all
  cxSetResourceString(@cxSFilterBoolOperatorOr, '或者'); // any
  //'OR');          // any
  cxSetResourceString(@cxSFilterBoolOperatorNotAnd, '非并且'); // not all
  //'NOT AND'); // not all
  cxSetResourceString(@cxSFilterBoolOperatorNotOr, '非或者'); // not any
  //'NOT OR');   // not any
  //
  cxSetResourceString(@cxSFilterRootButtonCaption, '过滤');
  //'Filter');
  cxSetResourceString(@cxSFilterAddCondition, '添加条件(&C)');
  //'Add &Condition');
  cxSetResourceString(@cxSFilterAddGroup, '添加组(&G)');
  //'Add &Group');
  cxSetResourceString(@cxSFilterRemoveRow, '删除行(&R)');
  //'&Remove Row');
  cxSetResourceString(@cxSFilterClearAll, '清除(&A)');
  //'Clear &All');
  cxSetResourceString(@cxSFilterFooterAddCondition, '按此按钮增加新条件');
  //'press the button to add a new condition');

  cxSetResourceString(@cxSFilterGroupCaption, '使用下面的条件');

  //'applies to the following conditions');
  cxSetResourceString(@cxSFilterRootGroupCaption, '<根>');
  //'<root>');
  cxSetResourceString(@cxSFilterControlNullString, '<空>');
  //'<empty>');

  cxSetResourceString(@cxSFilterErrorBuilding, '不能从源建立过滤');
  //'Can''t build filter from source');

  //FilterDialog
  cxSetResourceString(@cxSFilterDialogCaption, '定制过滤');
  //'Custom Filter');
  cxSetResourceString(@cxSFilterDialogInvalidValue, '输入值非法');
  //'Invalid value');
  cxSetResourceString(@cxSFilterDialogUse, '使用');
  //'Use');
  cxSetResourceString(@cxSFilterDialogSingleCharacter, '表示任何单个字符');
  //'to represent any single character');
  cxSetResourceString(@cxSFilterDialogCharactersSeries, '表示任意字符');
  //'to represent any series of characters');
  cxSetResourceString(@cxSFilterDialogOperationAnd, '并且');
  //'AND');
  cxSetResourceString(@cxSFilterDialogOperationOr, '或者');
  //'OR');
  cxSetResourceString(@cxSFilterDialogRows, '显示条件行:');
  //'Show rows where:');

  // FilterControlDialog
  cxSetResourceString(@cxSFilterControlDialogCaption, '过滤生成器');
  //'Filter builder');
  cxSetResourceString(@cxSFilterControlDialogNewFile, '未命名.flt');
  //'untitled.flt');
  cxSetResourceString(@cxSFilterControlDialogOpenDialogCaption, '打开一个已存文件');
  cxSetResourceString(@cxSFilterControlDialogSaveDialogCaption, '保存当前活动文件'); //'Save the active filter to file');
  cxSetResourceString(@cxSFilterControlDialogActionSaveCaption, '另存');
  cxSetResourceString(@cxSFilterControlDialogActionOpenCaption, '打开');
  cxSetResourceString(@cxSFilterControlDialogActionApplyCaption, '应用');
  cxSetResourceString(@cxSFilterControlDialogActionOkCaption, '确定');
  //'OK');
  cxSetResourceString(@cxSFilterControlDialogActionCancelCaption, '取消');
  cxSetResourceString(@cxSFilterControlDialogFileExt, 'flt');
  //'flt');
  cxSetResourceString(@cxSFilterControlDialogFileFilter, '过滤文件 (*.flt)|*.flt');

  //-------------------------------------------------------------------------

  //cxEditConsts.pas
  //-------------------------------------------------------------------------
  cxSetResourceString(@cxSEditButtonCancel, '取消');
  //sdl//'Cancel'
  cxSetResourceString(@cxSEditButtonOK, '确定');
  //sdl//'OK'
  cxSetResourceString(@cxSEditDateConvertError, 'Could not convert to date');
  cxSetResourceString(@cxSEditInvalidRepositoryItem, '此仓库项目不可接收');
  //tt8//'The repository item is not acceptable');
  cxSetResourceString(@cxSEditNumericValueConvertError, '不能转换到数字');
  //tt8//'Could not convert to numeric value');
  cxSetResourceString(@cxSEditPopupCircularReferencingError, '不允许循环引用');
  //tt8//'Circular referencing is not allowed');
  cxSetResourceString(@cxSEditPostError, '当提交编辑值时发生错误');
  //tt8//'An error occured during posting edit value');
  cxSetResourceString(@cxSEditTimeConvertError, '不能转换到时间');
  //tt8//'Could not convert to time');
  cxSetResourceString(@cxSEditValidateErrorText, '不正确的输入值. 使用ESC键放弃改变');
  //tt8//'Invalid input value. Use escape key to abandon changes');
  cxSetResourceString(@cxSEditValueOutOfBounds, '值超出范围');
  //tt8//'Value out of bounds');

  // TODO
  cxSetResourceString(@cxSEditCheckBoxChecked, '是');
  //tt8//'True');
  cxSetResourceString(@cxSEditCheckBoxGrayed, '');
  //tt8//'');
  cxSetResourceString(@cxSEditCheckBoxUnchecked, '否');
  //tt8//'False');
  cxSetResourceString(@cxSRadioGroupDefaultCaption, '');
  //tt8//'');

  cxSetResourceString(@cxSTextTrue, '是');
  //tt8//'True');
  cxSetResourceString(@cxSTextFalse, '否');
  //tt8//'False');

  // blob
  cxSetResourceString(@cxSBlobButtonOK, '确定(&O)');
  //tt8//'&OK');
  cxSetResourceString(@cxSBlobButtonCancel, '取消(&C)');
  //tt8//'&Cancel');
  cxSetResourceString(@cxSBlobButtonClose, '关闭(&C)');
  //tt8//'&Close');
  cxSetResourceString(@cxSBlobMemo, '(文本)');
  //tt8//'(MEMO)');
  cxSetResourceString(@cxSBlobMemoEmpty, '(空文本)');
  //tt8//'(memo)');
  cxSetResourceString(@cxSBlobPicture, '(图像)');
  //tt8//'(PICTURE)');
  cxSetResourceString(@cxSBlobPictureEmpty, '(空图像)');
  //tt8//'(picture)');

  // popup menu items
  cxSetResourceString(@cxSMenuItemCaptionCut, '剪切(&T)');
  //tt8//'Cu&t');
  cxSetResourceString(@cxSMenuItemCaptionCopy, '复制(&C)');
  //tt8//'&Copy');
  cxSetResourceString(@cxSMenuItemCaptionPaste, '粘贴(&P)');
  //tt8//'&Paste');
  cxSetResourceString(@cxSMenuItemCaptionDelete, '删除(&D)');
  //tt8//'&Delete');
  cxSetResourceString(@cxSMenuItemCaptionLoad, '装入(&L)...');
  //tt8//'&Load...');
  cxSetResourceString(@cxSMenuItemCaptionSave, '另存为(&A)...');
  //tt8//'Save &As...');

  // date
  cxSetResourceString(@cxSDatePopupClear, '清除');
  //tt8//'Clear');
  cxSetResourceString(@cxSDatePopupNow, 'Now');
  cxSetResourceString(@cxSDatePopupOK, 'OK');
  cxSetResourceString(@cxSDatePopupToday, '今天');
  //tt8//'Today');
  cxSetResourceString(@cxSDateError, '非法日期');
  //tt8//'Invalid Date');
  // smart input consts
  cxSetResourceString(@cxSDateToday, '今天');
  //tt8//'today');
  cxSetResourceString(@cxSDateYesterday, '昨天');
  //tt8//'yesterday');
  cxSetResourceString(@cxSDateTomorrow, '明天');
  //tt8//'tomorrow');
  cxSetResourceString(@cxSDateSunday, '星期日');
  //tt8//'Sunday');
  cxSetResourceString(@cxSDateMonday, '星期一');
  //tt8//'Monday');
  cxSetResourceString(@cxSDateTuesday, '星期二');
  //tt8//'Tuesday');
  cxSetResourceString(@cxSDateWednesday, '星期三');
  //tt8//'Wednesday');
  cxSetResourceString(@cxSDateThursday, '星期四');
  //tt8//'Thursday');
  cxSetResourceString(@cxSDateFriday, '星期五');
  //tt8//'Friday');
  cxSetResourceString(@cxSDateSaturday, '星期六');
  //tt8//'Saturday');
  cxSetResourceString(@cxSDateFirst, '第一');
  //tt8//'first');
  cxSetResourceString(@cxSDateSecond, '第二');
  //tt8//'second');
  cxSetResourceString(@cxSDateThird, '第三');
  //tt8//'third');
  cxSetResourceString(@cxSDateFourth, '第四');
  //tt8//'fourth');
  cxSetResourceString(@cxSDateFifth, '第五');
  //tt8//'fifth');
  cxSetResourceString(@cxSDateSixth, '第六');
  //tt8//'sixth');
  cxSetResourceString(@cxSDateSeventh, '第七');
  //tt8//'seventh');
  cxSetResourceString(@cxSDateBOM, '月初');
  //tt8//'bom');
  cxSetResourceString(@cxSDateEOM, '月末');
  //tt8//'eom');
  cxSetResourceString(@cxSDateNow, '现在');
  //tt8//'now');

  // calculator
  cxSetResourceString(@scxSCalcError, '错误');
  //tt8//'Error'

  // HyperLink
  cxSetResourceString(@scxSHyperLinkPrefix, 'http://');
  cxSetResourceString(@scxSHyperLinkDoubleSlash, '//');

  // edit repository
  cxSetResourceString(@scxSEditRepositoryBlobItem, 'BlobEdit|Represents the BLOB editor');
  cxSetResourceString(@scxSEditRepositoryButtonItem, 'ButtonEdit|Represents an edit control with embedded buttons');
  cxSetResourceString(@scxSEditRepositoryCalcItem, 'CalcEdit|Represents an edit control with a dropdown calculator window');
  cxSetResourceString(@scxSEditRepositoryCheckBoxItem, 'CheckBox|Represents a check box control that allows selecting an option');
  cxSetResourceString(@scxSEditRepositoryComboBoxItem, 'ComboBox|Represents the combo box editor');
  cxSetResourceString(@scxSEditRepositoryCurrencyItem, 'CurrencyEdit|Represents an editor enabling editing currency data');
  cxSetResourceString(@scxSEditRepositoryDateItem, 'DateEdit|Represents an edit control with a dropdown calendar');
  cxSetResourceString(@scxSEditRepositoryHyperLinkItem, 'HyperLink|Represents a text editor with hyperlink functionality');
  cxSetResourceString(@scxSEditRepositoryImageComboBoxItem,
    'ImageComboBox|Represents an editor displaying the list of images and text strings within the dropdown window');
  cxSetResourceString(@scxSEditRepositoryImageItem, 'Image|Represents an image editor');
  cxSetResourceString(@scxSEditRepositoryLookupComboBoxItem, 'LookupComboBox|Represents a lookup combo box control');
  cxSetResourceString(@scxSEditRepositoryMaskItem, 'MaskEdit|Represents a generic masked edit control.');
  cxSetResourceString(@scxSEditRepositoryMemoItem, 'Memo|Represents an edit control that allows editing memo data');
  cxSetResourceString(@scxSEditRepositoryMRUItem,
    'MRUEdit|Represents a text editor displaying the list of most recently used items (MRU) within a dropdown window');
  cxSetResourceString(@scxSEditRepositoryPopupItem, 'PopupEdit|Represents an edit control with a dropdown list');
  cxSetResourceString(@scxSEditRepositorySpinItem, 'SpinEdit|Represents a spin editor');
  cxSetResourceString(@scxSEditRepositoryRadioGroupItem, 'RadioGroup|Represents a group of radio buttons');
  cxSetResourceString(@scxSEditRepositoryTextItem, 'TextEdit|Represents a single line text editor');
  cxSetResourceString(@scxSEditRepositoryTimeItem, 'TimeEdit|Represents an editor displaying time values');

  cxSetResourceString(@scxRegExprLine, '行');
  //tt8//'Line');
  cxSetResourceString(@scxRegExprChar, '字符');
  //tt8//'Char');
  cxSetResourceString(@scxRegExprNotAssignedSourceStream, '此源流没有被赋值');
  //tt8//'The source stream is not assigned');
  cxSetResourceString(@scxRegExprEmptySourceStream, '此源流是空的');
  //tt8//'The source stream is empty');
  cxSetResourceString(@scxRegExprCantUsePlusQuantifier, '符号 ''+'' 不能应用到这');
  //tt8//'The ''+'' quantifier cannot be applied here');
  cxSetResourceString(@scxRegExprCantUseStarQuantifier, '符号 ''*'' 不能应用到这');
  //tt8//'The ''*'' quantifier cannot be applied here');
  cxSetResourceString(@scxRegExprCantCreateEmptyAlt, '二中择一不能为空');
  //tt8//'The alternative should not be empty');
  cxSetResourceString(@scxRegExprCantCreateEmptyBlock, '此块应该为空');
  //tt8//'The block should not be empty');
  cxSetResourceString(@scxRegExprIllegalSymbol, '不合规定的 ''%s''');
  //tt8//'Illegal ''%s''');
  cxSetResourceString(@scxRegExprIllegalQuantifier, '不合规定的量词 ''%s''');
  //tt8//'Illegal quantifier ''%s''');
  cxSetResourceString(@scxRegExprNotSupportQuantifier, '此参数量词不支持');
  //tt8//'The parameter quantifiers are not supported');
  cxSetResourceString(@scxRegExprIllegalIntegerValue, '不合法的整数值');
  //tt8//'Illegal integer value');
  cxSetResourceString(@scxRegExprTooBigReferenceNumber, '引用数太大');
  //tt8//'Too big reference number');
  cxSetResourceString(@scxRegExprCantCreateEmptyEnum, '不能创建空的枚举值');
  //tt8//'Can''t create empty enumeration');
  cxSetResourceString(@scxRegExprSubrangeOrder, '子串的开始字符位置不能超出结束字符位置');
  //tt8//'The starting character of the subrange must be less than the finishing one');
  cxSetResourceString(@scxRegExprHexNumberExpected0, '期待十六进制数');
  //tt8//'Hexadecimal number expected');
  cxSetResourceString(@scxRegExprHexNumberExpected, '期待十六进制数的位置发现了 ''%s'' ');
  //tt8//'Hexadecimal number expected but ''%s'' found');
  cxSetResourceString(@scxRegExprMissing, '缺少 ''%s''');
  //tt8//'Missing ''%s''');
  cxSetResourceString(@scxRegExprUnnecessary, '不必要的 ''%s''');
  //tt8//'Unnecessary ''%s''');
  cxSetResourceString(@scxRegExprIncorrectSpace, '在 ''\'' 后不能出现空格字符');
  //tt8//'The space character is not allowed after ''\''');
  cxSetResourceString(@scxRegExprNotCompiled, '规则表达式不能编译');
  //tt8//'Regular expression is not compiled');
  cxSetResourceString(@scxRegExprIncorrectParameterQuantifier, '错误的参数');
  //tt8//'Incorrect parameter quantifier');
  cxSetResourceString(@scxRegExprCantUseParameterQuantifier, '此参数不能应用在此处');
  //tt8//'The parameter quantifier cannot be applied here');

  cxSetResourceString(@scxMaskEditRegExprError, '规则表达式错误:');
  //tt8//'Regular expression errors:');
  cxSetResourceString(@scxMaskEditInvalidEditValue, '编辑值非法');
  //tt8//'The edit value is invalid');
  cxSetResourceString(@scxMaskEditNoMask, '没有');
  //tt8//'None');
  cxSetResourceString(@scxMaskEditIllegalFileFormat, '文件格式非法');
  //tt8//'Illegal file format');
  cxSetResourceString(@scxMaskEditEmptyMaskCollectionFile, '掩码格式文件为空');
  //tt8//'The mask collection file is empty');
  cxSetResourceString(@scxMaskEditMaskCollectionFiles, '掩码格式文件');
  //tt8//'Mask collection files');
  cxSetResourceString(@cxSSpinEditInvalidNumericValue, 'Invalid numeric value');
  //-------------------------------------------------------------------------

  //dxPSRes.pas
  //-------------------------------------------------------------------------
  {cxSetResourceString(@sdxBtnOK,'确定(&O)');
  //tt8//'OK');
  cxSetResourceString(@sdxBtnOKAccelerated,'确定(&O');
  //tt8//'&OK');
  cxSetResourceString(@sdxBtnCancel,'取消');
  //tt8//'Cancel');
  cxSetResourceString(@sdxBtnClose,'关闭');
  //tt8//'Close');
  cxSetResourceString(@sdxBtnApply,'应用(&A)');
  //tt8//'&Apply');
  cxSetResourceString(@sdxBtnHelp,'帮助(&H)');
  //tt8//'&Help');
  cxSetResourceString(@sdxBtnFix,'调整(&F)');
  //tt8//'&Fix');
  cxSetResourceString(@sdxBtnNew,'新建(&N)...');
  //tt8//'&New...');
  cxSetResourceString(@sdxBtnIgnore,'忽略(&I)');
  //tt8//'&Ignore');
  cxSetResourceString(@sdxBtnYes,'是(&Y)');
  //tt8//'&Yes');
  cxSetResourceString(@sdxBtnNo,'否(&N)');
  //tt8//'&No');
  cxSetResourceString(@sdxBtnEdit,'编辑(&E)...');
  //tt8//'&Edit...');
  cxSetResourceString(@sdxBtnReset,'复位(&R)');
  //tt8//'&Reset');
  cxSetResourceString(@sdxBtnAdd,'增加(&A');
  //tt8//'&Add');
  cxSetResourceString(@sdxBtnAddComposition,'增加布局(&C)');
  //tt8//'Add &Composition');
  cxSetResourceString(@sdxBtnDefault,'默认(&D)...');
  //tt8//'&Default...');
  cxSetResourceString(@sdxBtnDelete,'删除(&D)...');
  //tt8//'&Delete...');
  cxSetResourceString(@sdxBtnDescription,'描述(&D)...');
  //tt8//'&Description...');
  cxSetResourceString(@sdxBtnCopy,'复制(&C)...');
  //tt8//'&Copy...');
  cxSetResourceString(@sdxBtnYesToAll,'全部是(&A)');
  //tt8//'Yes To &All');
  cxSetResourceString(@sdxBtnRestoreDefaults,'恢复默认值(&R)');
  //tt8//'&Restore Defaults');
  cxSetResourceString(@sdxBtnRestoreOriginal,'还原(&O)');
  //tt8//'Restore &Original');
  cxSetResourceString(@sdxBtnTitleProperties,'标题属性...');
  //tt8//'Title Properties...');
  cxSetResourceString(@sdxBtnProperties,'属性(&R)...');
  //tt8//'P&roperties...');
  cxSetResourceString(@sdxBtnNetwork,'网络(&W)...');
  //tt8//'Net&work...');
  cxSetResourceString(@sdxBtnBrowse,'浏览(&B)...');
  //tt8//'&Browse...');
  cxSetResourceString(@sdxBtnPageSetup,'页面设置(&G)...');
  //tt8//'Pa&ge Setup...');
  cxSetResourceString(@sdxBtnPrintPreview,'打印预览(&V)...');
  //tt8//'Print Pre&view...');
  cxSetResourceString(@sdxBtnPreview,'预览(&V)...');
  //tt8//'Pre&view...');
  cxSetResourceString(@sdxBtnPrint,'打印...');
  //tt8//'Print...');
  cxSetResourceString(@sdxBtnOptions,'选项(&O)...');
  //tt8//'&Options...');
  cxSetResourceString(@sdxBtnStyleOptions,'样式选项...');
  //tt8//'Style Options...');
  cxSetResourceString(@sdxBtnDefinePrintStyles,'定义样式(&D)...');
  //tt8//'&Define Styles...');
  cxSetResourceString(@sdxBtnPrintStyles,'打印样式');
  //tt8//'Print Styles');
  cxSetResourceString(@sdxBtnBackground,'背景');
  //tt8//'Background');
  cxSetResourceString(@sdxBtnShowToolBar,'显示工具栏(&T)');
  //tt8//'Show &ToolBar');
  cxSetResourceString(@sdxBtnDesign,'设计(&E)...');
  //tt8//'D&esign...');
  cxSetResourceString(@sdxBtnMoveUp,'上移(&U)');
  //tt8//'Move &Up');
  cxSetResourceString(@sdxBtnMoveDown,'下移(&N)');
  //tt8//'Move Dow&n');

  cxSetResourceString(@sdxBtnMoreColors,'其他颜色(&M)...');
  //tt8//'&More Colors...');
  cxSetResourceString(@sdxBtnFillEffects,'填充效果(&F)...');
  //tt8//'&Fill Effects...');
  cxSetResourceString(@sdxBtnNoFill,'不填充');
  //tt8//'&No Fill');
  cxSetResourceString(@sdxBtnAutomatic,'自动(&A)');
  //tt8//'&Automatic');
  cxSetResourceString(@sdxBtnNone,'无(&N)');
  //tt8//'&None');

  cxSetResourceString(@sdxBtnOtherTexture,'其它纹理(&X)...');
  //tt8//'Other Te&xture...');
  cxSetResourceString(@sdxBtnInvertColors,'反转颜色(&N)');
  //tt8//'I&nvert Colors');
  cxSetResourceString(@sdxBtnSelectPicture,'选择图片(&L)...');
  //tt8//'Se&lect Picture...');

  cxSetResourceString(@sdxEditReports,'编辑报表');
  //tt8//'Edit Reports');
  cxSetResourceString(@sdxComposition,'布局');
  //tt8//'Composition');
  cxSetResourceString(@sdxReportTitleDlgCaption,'报表标题');
  //tt8//'Report Title');
  cxSetResourceString(@sdxMode,'模式(&M):');
  //tt8//'&Mode:');
  cxSetResourceString(@sdxText,'本文(&T)');
  //tt8//'&Text');
  cxSetResourceString(@sdxProperties,'属性(&P)');
  //tt8//'&Properties');
  cxSetResourceString(@sdxAdjustOnScale,'适合页面(&A)');
  //tt8//'&Adjust on Scale');
  cxSetResourceString(@sdxTitleModeNone,'无');
  //tt8//'None');
  cxSetResourceString(@sdxTitleModeOnEveryTopPage,'在每张顶页');
  //tt8//'On Every Top Page');
  cxSetResourceString(@sdxTitleModeOnFirstPage,'在第一页');
  //tt8//'On First Page');

  cxSetResourceString(@sdxEditDescription,'编辑描述');
  //tt8//'Edit Description');
  cxSetResourceString(@sdxRename,'重命名(&M)');
  //tt8//'Rena&me');
  cxSetResourceString(@sdxSelectAll,'全选');
  //tt8//'&Select All');

  cxSetResourceString(@sdxAddReport,'增加报表');
  //tt8//'Add Report');
  cxSetResourceString(@sdxAddAndDesignReport,'增加并设计报表(&D)...');
  //tt8//'Add and D&esign Report...');
  cxSetResourceString(@sdxNewCompositionCaption,'新建布局');
  //tt8//'New Composition');
  cxSetResourceString(@sdxName,'名字(&N):');
  //tt8//'&Name:');
  cxSetResourceString(@sdxCaption,'标题(&C):');
  //tt8//'&Caption:');
  cxSetResourceString(@sdxAvailableSources,'可用的源(&A)');
  //tt8//'&Available Source(s)');
  cxSetResourceString(@sdxOnlyComponentsInActiveForm,'只显示当前表单的组件');
  //tt8//'Only Components in Active &Form');
  cxSetResourceString(@sdxOnlyComponentsWithoutLinks,'只显示除现有报表链接以外的组件');
  //tt8//'Only Components &without Existing ReportLinks');
  cxSetResourceString(@sdxItemName,'名称');
  //tt8//'Name');
  cxSetResourceString(@sdxItemDescription,'描述');
  //tt8//'Description');

  cxSetResourceString(@sdxConfirmDeleteItem,'要删除下一个项目： %s 吗?');
  //tt8//'Do you want to delete next items: %s ?');
  cxSetResourceString(@sdxAddItemsToComposition,'增加项目到布局');
  //tt8//'Add Items to Composition');
  cxSetResourceString(@sdxHideAlreadyIncludedItems,'隐藏已包含项目');
  //tt8//'Hide Already &Included Items');
  cxSetResourceString(@sdxAvailableItems,'可用项目(&I)');
  //tt8//'A&vailable Items');
  cxSetResourceString(@sdxItems,'项目(&I)');
  //tt8//'&Items');
  cxSetResourceString(@sdxEnable,'允许(&E)');
  //tt8//'&Enable');
  cxSetResourceString(@sdxOptions,'选项');
  //tt8//'Options');
  cxSetResourceString(@sdxShow,'显示');
  //tt8//'Show');
  cxSetResourceString(@sdxPaintItemsGraphics,'绘制项目图示(&P)');
  //tt8//'&Paint Item Graphics');
  cxSetResourceString(@sdxDescription,'描述:');
  //tt8//'&Description:');

  cxSetResourceString(@sdxNewReport,'新报表');
  //tt8//'NewReport');

  cxSetResourceString(@sdxOnlySelected,'只是选定的(&S)');
  //tt8//'Only &Selected');
  cxSetResourceString(@sdxExtendedSelect,'扩展选定的(&E)');
  //tt8//'&Extended Select');
  cxSetResourceString(@sdxIncludeFixed,'包含固定区(&I)');
  //tt8//'&Include Fixed');

  cxSetResourceString(@sdxFonts,'字体');
  //tt8//'Fonts');
  cxSetResourceString(@sdxBtnFont,'字体(&N)...');
  //tt8//'Fo&nt...');
  cxSetResourceString(@sdxBtnEvenFont,'偶数行字体(&V)...');
  //tt8//'E&ven Font...');
  cxSetResourceString(@sdxBtnOddFont,'奇数行字体(&N)...');
  //tt8//'Odd Fo&nt...');
  cxSetResourceString(@sdxBtnFixedFont,'固定区字体(&I)...');
  //tt8//'F&ixed Font...');
  cxSetResourceString(@sdxBtnGroupFont,'组字体(&P)...');
  //tt8//'Grou&p Font...');
  cxSetResourceString(@sdxBtnChangeFont,'更换字体(&N)...');
  //tt8//'Change Fo&nt...');

  cxSetResourceString(@sdxFont,'字体');
  //tt8//'Font');
  cxSetResourceString(@sdxOddFont,'奇数行字体');
  //tt8//'Odd Font');
  cxSetResourceString(@sdxEvenFont,'偶数行字体');
  //tt8//'Even Font');
  cxSetResourceString(@sdxPreviewFont,'预览字体');
  //tt8//'Preview Font');
  cxSetResourceString(@sdxCaptionNodeFont,'层次标题字体');
  //tt8//'Level Caption Font');
  cxSetResourceString(@sdxGroupNodeFont,'组节点字体');
  //tt8//'Group Node Font');
  cxSetResourceString(@sdxGroupFooterFont,'组脚字体');
  //tt8//'Group Footer Font');
  cxSetResourceString(@sdxHeaderFont,'页眉字体');
  //tt8//'Header Font');
  cxSetResourceString(@sdxFooterFont,'页脚字体');
  //tt8//'Footer Font');
  cxSetResourceString(@sdxBandFont,'带区字体');
  //tt8//'Band Font');

  cxSetResourceString(@sdxTransparent,'透明(&T)');
  //tt8//'&Transparent');
  cxSetResourceString(@sdxFixedTransparent,'透明(&X)');
  //tt8//'Fi&xed Transparent');
  cxSetResourceString(@sdxCaptionTransparent,'标题透明');
  //tt8//'Caption Transparent');
  cxSetResourceString(@sdxGroupTransparent,'组透明');
  //tt8//'Group Transparent');

  cxSetResourceString(@sdxGraphicAsTextValue,'(图像)');
  //tt8//'(GRAPHIC)');
  cxSetResourceString(@sdxColors,'颜色');
  //tt8//'Colors');
  cxSetResourceString(@sdxColor,'颜色(&L):');
  //tt8//'Co&lor:');
  cxSetResourceString(@sdxOddColor,'奇数行颜色(&L):');
  //tt8//'Odd Co&lor:');
  cxSetResourceString(@sdxEvenColor,'偶数行颜色(&V):');
  //tt8//'E&ven Color:');
  cxSetResourceString(@sdxPreviewColor,'预览颜色(&P):');
  //tt8//'&Preview Color:');
  cxSetResourceString(@sdxBandColor,'带区颜色(&B):');
  //tt8//'&Band Color:');
  cxSetResourceString(@sdxLevelCaptionColor,'层次标题颜色(&V):');
  //tt8//'Le&vel Caption Color:');
  cxSetResourceString(@sdxHeaderColor,'标题颜色(&E):');
  //tt8//'H&eader Color:');
  cxSetResourceString(@sdxGroupNodeColor,'组节点颜色(&N):');
  //tt8//'Group &Node Color:');
  cxSetResourceString(@sdxGroupFooterColor,'组脚颜色(&G):');
  //tt8//'&Group Footer Color:');
  cxSetResourceString(@sdxFooterColor,'页脚颜色(&T):');
  //tt8//'Foo&ter Color:');
  cxSetResourceString(@sdxFixedColor,'固定颜色(&I):');
  //tt8//'F&ixed Color:');
  cxSetResourceString(@sdxGroupColor,'组颜色(&I):');
  //tt8//'Grou&p Color:');
  cxSetResourceString(@sdxCaptionColor,'标题颜色:');
  //tt8//'Caption Color:');
  cxSetResourceString(@sdxGridLinesColor,'网格线颜色(&D):');
  //tt8//'Gri&d Line Color:');

  cxSetResourceString(@sdxBands,'带区(&B)');
  //tt8//'&Bands');
  cxSetResourceString(@sdxLevelCaptions,'层次标题(&C)');
  //tt8//'Levels &Caption');
  cxSetResourceString(@sdxHeaders,'页眉(&E)');
  //tt8//'H&eaders');
  cxSetResourceString(@sdxFooters,'页脚(&R)');
  //tt8//'Foote&rs');
  cxSetResourceString(@sdxGroupFooters,'组脚(&G)');
  //tt8//'&Group Footers');
  cxSetResourceString(@sdxPreview,'预览(&W)');
  //tt8//'Previe&w');
  cxSetResourceString(@sdxPreviewLineCount,'预览行数(&T):');
  //tt8//'Preview Line Coun&t:');
  cxSetResourceString(@sdxAutoCalcPreviewLineCount,'自动计算预览行数(&U)');
  //tt8//'A&uto Calculate Preview Lines');

  cxSetResourceString(@sdxGrid,'网格(&D)');
  //tt8//'Grid Lines');
  cxSetResourceString(@sdxNodesGrid,'节点网格(&N)');
  //tt8//'Node Grid Lines');
  cxSetResourceString(@sdxGroupFooterGrid,'组脚网格(&P)');
  //tt8//'GroupFooter Grid Lines');

  cxSetResourceString(@sdxStateImages,'状态图像(&S)');
  //tt8//'&State Images');
  cxSetResourceString(@sdxImages,'图象(&I)');
  //tt8//'&Images');

  cxSetResourceString(@sdxTextAlign,'文本排列(&A)');
  //tt8//'Text&Align');
  cxSetResourceString(@sdxTextAlignHorz,'水平(&Z)');
  //tt8//'Hori&zontally');
  cxSetResourceString(@sdxTextAlignVert,'垂直(&V)');
  //tt8//'&Vertically');
  cxSetResourceString(@sdxTextAlignLeft,'靠左');
  //tt8//'Left');
  cxSetResourceString(@sdxTextAlignCenter,'居中');
  //tt8//'Center');
  cxSetResourceString(@sdxTextAlignRight,'靠右');
  //tt8//'Right');
  cxSetResourceString(@sdxTextAlignTop,'顶部');
  //tt8//'Top');
  cxSetResourceString(@sdxTextAlignVCenter,'居中');
  //tt8//'Center');
  cxSetResourceString(@sdxTextAlignBottom,'底部');
  //tt8//'Bottom');
  cxSetResourceString(@sdxBorderLines,'边框线条(&B)');
  //tt8//'&Border');
  cxSetResourceString(@sdxHorzLines,'水平线(&Z)');
  //tt8//'Hori&zontal Lines');
  cxSetResourceString(@sdxVertLines,'垂直线(&V)');
  //tt8//'&Vertical Lines');
  cxSetResourceString(@sdxFixedHorzLines,'固定水平线(&X)');
  //tt8//'Fi&xed Horizontal Lines');
  cxSetResourceString(@sdxFixedVertLines,'固定垂直线(&D)');
  //tt8//'Fixe&d Vertical Lines');
  cxSetResourceString(@sdxFlatCheckMarks,'平面检查框(&L)');
  //tt8//'F&lat CheckMarks');
  cxSetResourceString(@sdxCheckMarksAsText,'用文本显示检查框(&D)');
  //tt8//'&Display CheckMarks as Text');

  cxSetResourceString(@sdxRowAutoHeight,'自动计算行高(&W)');
  //tt8//'Ro&w AutoHeight');
  cxSetResourceString(@sdxEndEllipsis,'结束省略符(&E)');
  //tt8//'&EndEllipsis');

  cxSetResourceString(@sdxDrawBorder,'绘制边框(&D)');
  //tt8//'&Draw Border');
  cxSetResourceString(@sdxFullExpand,'完全展开(&E)');
  //tt8//'Full &Expand');
  cxSetResourceString(@sdxBorderColor,'边框颜色(&B):');
  //tt8//'&Border Color:');
  cxSetResourceString(@sdxAutoNodesExpand,'自动展开节点(&U)');
  //tt8//'A&uto Nodes Expand');
  cxSetResourceString(@sdxExpandLevel,'展开层次(&L):');
  //tt8//'Expand &Level:');
  cxSetResourceString(@sdxFixedRowOnEveryPage,'固定每页行数(&E)');
  //tt8//'Fixed Rows');

  cxSetResourceString(@sdxDrawMode,'绘制模式(&M):');
  //tt8//'Draw &Mode:');
  cxSetResourceString(@sdxDrawModeStrict,'精确');
  //tt8//'Strict');
  cxSetResourceString(@sdxDrawModeOddEven,'奇/偶行模式');
  //tt8//'Odd/Even Rows Mode');
  cxSetResourceString(@sdxDrawModeChess,'国际象棋模式');
  //tt8//'Chess Mode');
  cxSetResourceString(@sdxDrawModeBorrow,'从源借用');
  //tt8//'Borrow From Source');

  cxSetResourceString(@sdx3DEffects,'三维效果');
  //tt8//'3D Effects');
  cxSetResourceString(@sdxUse3DEffects,'使用三维效果(&3)');
  //tt8//'Use &3D Effects');
  cxSetResourceString(@sdxSoft3D,'柔和三维(&3)');
  //tt8//'Sof&t3D');

  cxSetResourceString(@sdxBehaviors,'性能');
  //tt8//'Behaviors');
  cxSetResourceString(@sdxMiscellaneous,'杂项');
  //tt8//'Miscellaneous');
  cxSetResourceString(@sdxOnEveryPage,'在每页');
  //tt8//'On Every Page');
  cxSetResourceString(@sdxNodeExpanding,'展开节点');
  //tt8//'Node Expanding');
  cxSetResourceString(@sdxSelection,'选择');
  //tt8//'Selection');
  cxSetResourceString(@sdxNodeAutoHeight,'节点自动调整高度(&N)');
  //tt8//'&Node Auto Height');
  cxSetResourceString(@sdxTransparentGraphics,'图形透明(&T)');
  //tt8//'&Transparent Graphics');
  cxSetResourceString(@sdxAutoWidth,'自动调整宽度(&W)');
  //tt8//'Auto &Width');

  cxSetResourceString(@sdxDisplayGraphicsAsText,'用文本形式显示图形(&T)');
  //tt8//'Display Graphic As &Text');
  cxSetResourceString(@sdxTransparentColumnGraphics,'图形透明(&G)');
  //tt8//'Transparent &Graphics');

  cxSetResourceString(@sdxBandsOnEveryPage,'每页显示带区');
  //tt8//'Bands');
  cxSetResourceString(@sdxHeadersOnEveryPage,'每页显示页眉');
  //tt8//'Headers');
  cxSetResourceString(@sdxFootersOnEveryPage,'每页显示页脚');
  //tt8//'Footers');
  cxSetResourceString(@sdxGraphics,'图形');
  //tt8//'&Graphics');

  // Common messages

  cxSetResourceString(@sdxOutOfResources,'资源不足');
  //tt8//'Out of Resources');
  cxSetResourceString(@sdxFileAlreadyExists,'文件 "%s" 已经存在。');
  //tt8//'File "%s" Already Exists.');
  cxSetResourceString(@sdxConfirmOverWrite,'文件 "%s" 已经存在。 覆盖吗 ?');
  //tt8//'File "%s" already exists. Overwrite ?');
  cxSetResourceString(@sdxInvalidFileName,'无效的文件名 "%s"');
  //tt8//'Invalid File Name "%s"');
  cxSetResourceString(@sdxRequiredFileName,'输入文件名称。');
  //tt8//'Enter file name.');
  cxSetResourceString(@sdxOutsideMarginsMessage,
    'One or more margins are set outside the printable area of the page.' + #13#10 +
    'Do you want to continue ?');
  cxSetResourceString(@sdxOutsideMarginsMessage2,
    'One or more margins are set outside the printable area of the page.' + #13#10 +
    'Choose the Fix button to increase the appropriate margins.');
  cxSetResourceString(@sdxInvalidMarginsMessage,
    'One or more margins are set to the invalid values.' + #13#10 +
    'Choose the Fix button to correct this problem.' + #13#10 +
    'Choose the Restore button to restore original values.');
  cxSetResourceString(@sdxInvalidMargins,'一个或多个页边距是无效值');
  //tt8//'One or more margins has invalid values');
  cxSetResourceString(@sdxOutsideMargins,'一个或多个页边距超出页面的可打印区域');
  //tt8//'One or more margins are set outside the printable area of the page');
  cxSetResourceString(@sdxThereAreNowItemsForShow,'没有项目');
  //tt8//'There are no items in this view');

  // Color palette

  cxSetResourceString(@sdxPageBackground,' 页面背景');
  //tt8//' Page Background');
  cxSetResourceString(@sdxPenColor,'铅笔颜色');
  //tt8//'Pen Color');
  cxSetResourceString(@sdxFontColor,'字体颜色');
  //tt8//'Font Color');
  cxSetResourceString(@sdxBrushColor,'刷子颜色');
  //tt8//'Brush Color');
  cxSetResourceString(@sdxHighLight,'加亮');
  //tt8//'HighLight');

  // Color names

  cxSetResourceString(@sdxColorBlack,'黑色');
  //tt8//'Black');
  cxSetResourceString(@sdxColorDarkRed,'深红');
  //tt8//'Dark Red');
  cxSetResourceString(@sdxColorRed,'红色');
  //tt8//'Red');
  cxSetResourceString(@sdxColorPink,'粉红');
  //tt8//'Pink');
  cxSetResourceString(@sdxColorRose,'玫瑰红');
  //tt8//'Rose');
  cxSetResourceString(@sdxColorBrown,'褐色');
  //tt8//'Brown');
  cxSetResourceString(@sdxColorOrange,'桔黄');
  //tt8//'Orange');
  cxSetResourceString(@sdxColorLightOrange,'浅桔黄');
  //tt8//'Light Orange');
  cxSetResourceString(@sdxColorGold,'金色');
  //tt8//'Gold');
  cxSetResourceString(@sdxColorTan,'棕黄');
  //tt8//'Tan');
  cxSetResourceString(@sdxColorOliveGreen,'橄榄绿');
  //tt8//'Olive Green');
  cxSetResourceString(@sdxColorDrakYellow,'深黄');
  //tt8//'Dark Yellow');
  cxSetResourceString(@sdxColorLime,'酸橙色');
  //tt8//'Lime');
  cxSetResourceString(@sdxColorYellow,'黄色');
  //tt8//'Yellow');
  cxSetResourceString(@sdxColorLightYellow,'浅黄');
  //tt8//'Light Yellow');
  cxSetResourceString(@sdxColorDarkGreen,'深绿');
  //tt8//'Dark Green');
  cxSetResourceString(@sdxColorGreen,'绿色');
  //tt8//'Green');
  cxSetResourceString(@sdxColorSeaGreen,'海绿');
  //tt8//'Sea Green');
  cxSetResourceString(@sdxColorBrighthGreen,'鲜绿');
  //tt8//'Bright Green');
  cxSetResourceString(@sdxColorLightGreen,'浅绿');
  //tt8//'Light Green');
  cxSetResourceString(@sdxColorDarkTeal,'深灰蓝');
  //tt8//'Dark Teal');
  cxSetResourceString(@sdxColorTeal,'青色');
  //tt8//'Teal');
  cxSetResourceString(@sdxColorAqua,'宝石蓝');
  //tt8//'Aqua');
  cxSetResourceString(@sdxColorTurquoise,'青绿');
  //tt8//'Turquoise');
  cxSetResourceString(@sdxColorLightTurquoise,'浅青绿');
  //tt8//'Light Turquoise');
  cxSetResourceString(@sdxColorDarkBlue,'深蓝');
  //tt8//'Dark Blue');
  cxSetResourceString(@sdxColorBlue,'蓝色');
  //tt8//'Blue');
  cxSetResourceString(@sdxColorLightBlue,'浅蓝');
  //tt8//'Light Blue');
  cxSetResourceString(@sdxColorSkyBlue,'天蓝');
  //tt8//'Sky Blue');
  cxSetResourceString(@sdxColorPaleBlue,'淡蓝');
  //tt8//'Pale Blue');
  cxSetResourceString(@sdxColorIndigo,'靛蓝');
  //tt8//'Indigo');
  cxSetResourceString(@sdxColorBlueGray,'蓝-灰');
  //tt8//'Blue Gray');
  cxSetResourceString(@sdxColorViolet,'紫色');
  //tt8//'Violet');
  cxSetResourceString(@sdxColorPlum,'梅红');
  //tt8//'Plum');
  cxSetResourceString(@sdxColorLavender,'淡紫');
  //tt8//'Lavender');
  cxSetResourceString(@sdxColorGray80,'灰色-80%');
  //tt8//'Gray-80%');
  cxSetResourceString(@sdxColorGray50,'灰色-50%');
  //tt8//'Gray-50%');
  cxSetResourceString(@sdxColorGray40,'灰色-40%');
  //tt8//'Gray-40%');
  cxSetResourceString(@sdxColorGray25,'灰色-25%');
  //tt8//'Gray-25%');
  cxSetResourceString(@sdxColorWhite,'白色');
  //tt8//'White');

  // FEF Dialog

  cxSetResourceString(@sdxTexture,'纹理(&T)');
  //tt8//'&Texture');                                                                                                                                                                                                          
  cxSetResourceString(@sdxPattern,'图案(&P)');
  //tt8//'&Pattern');                                                                                                                                                                                                          
  cxSetResourceString(@sdxPicture,'图片(&I)');
  //tt8//'P&icture');                                                                                                                                                                                                          
  cxSetResourceString(@sdxForeground,'前景(&F)');
  //tt8//'&Foreground');                                                                                                                                                                                                    
  cxSetResourceString(@sdxBackground,'背景(&B)');
  //tt8//'&Background');                                                                                                                                                                                                    
  cxSetResourceString(@sdxSample,'示范:');
  //tt8//'Sample:');                                                                                                                                                                                                               
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxFEFCaption,'填充效果');
  //tt8//'Fill Effects');                                                                                                                                                                                                   
  cxSetResourceString(@sdxPaintMode,'画图模式');
  //tt8//'Paint &Mode');                                                                                                                                                                                                     
  cxSetResourceString(@sdxPaintModeCenter,'居中');
  //tt8//'Center');                                                                                                                                                                                                        
  cxSetResourceString(@sdxPaintModeStretch,'拉伸');
  //tt8//'Stretch');                                                                                                                                                                                                      
  cxSetResourceString(@sdxPaintModeTile,'平铺');
  //tt8//'Tile');                                                                                                                                                                                                            
  cxSetResourceString(@sdxPaintModeProportional,'锁定比例');
  //tt8//'Proportional');

  // Pattern names

  cxSetResourceString(@sdxPatternGray5,'5%');
  //tt8//'5%');                                                                                                                                                                                                                 
  cxSetResourceString(@sdxPatternGray10,'10%');
  //tt8//'10%');                                                                                                                                                                                                              
  cxSetResourceString(@sdxPatternGray20,'20%');
  //tt8//'20%');                                                                                                                                                                                                              
  cxSetResourceString(@sdxPatternGray25,'25%');
  //tt8//'25%');                                                                                                                                                                                                              
  cxSetResourceString(@sdxPatternGray30,'30%');
  //tt8//'30%');                                                                                                                                                                                                              
  cxSetResourceString(@sdxPatternGray40,'40%');
  //tt8//'40%');                                                                                                                                                                                                              
  cxSetResourceString(@sdxPatternGray50,'50%');
  //tt8//'50%');                                                                                                                                                                                                              
  cxSetResourceString(@sdxPatternGray60,'60%');
  //tt8//'60%');                                                                                                                                                                                                              
  cxSetResourceString(@sdxPatternGray70,'70%');
  //tt8//'70%');                                                                                                                                                                                                              
  cxSetResourceString(@sdxPatternGray75,'75%');
  //tt8//'75%');                                                                                                                                                                                                              
  cxSetResourceString(@sdxPatternGray80,'80%');
  //tt8//'80%');                                                                                                                                                                                                              
  cxSetResourceString(@sdxPatternGray90,'90%');
  //tt8//'90%');                                                                                                                                                                                                              
  cxSetResourceString(@sdxPatternLightDownwardDiagonal,'浅色下对角线');
  //tt8//'Light downward diagonal');                                                                                                                                                                  
  cxSetResourceString(@sdxPatternLightUpwardDiagonal,'浅色上对角线');
  //tt8//'Light upward diagonal');                                                                                                                                                                      
  cxSetResourceString(@sdxPatternDarkDownwardDiagonal,'深色下对角线');
  //tt8//'Dark downward diagonal');                                                                                                                                                                    
  cxSetResourceString(@sdxPatternDarkUpwardDiagonal,'深色上对角线');
  //tt8//'Dark upward diagonal');                                                                                                                                                                        
  cxSetResourceString(@sdxPatternWideDownwardDiagonal,'宽下对角线');
  //tt8//'Wide downward diagonal');                                                                                                                                                                      
  cxSetResourceString(@sdxPatternWideUpwardDiagonal,'宽上对角线');
  //tt8//'Wide upward diagonal');                                                                                                                                                                          
  cxSetResourceString(@sdxPatternLightVertical,'浅色垂线');
  //tt8//'Light vertical');
  cxSetResourceString(@sdxPatternLightHorizontal,'浅色横线');
  //tt8//'Light horizontal');
  cxSetResourceString(@sdxPatternNarrowVertical,'窄竖线');
  //tt8//'Narrow vertical');
  cxSetResourceString(@sdxPatternNarrowHorizontal,'窄横线');
  //tt8//'Narrow horizontal');
  cxSetResourceString(@sdxPatternDarkVertical,'深色竖线');
  //tt8//'Dark vertical');
  cxSetResourceString(@sdxPatternDarkHorizontal,'深色横线');
  //tt8//'Dark horizontal');
  cxSetResourceString(@sdxPatternDashedDownward,'下对角虚线');
  //tt8//'Dashed downward');
  cxSetResourceString(@sdxPatternDashedUpward,'上对角虚线');
  //tt8//'Dashed upward');
  cxSetResourceString(@sdxPatternDashedVertical,'横虚线');
  //tt8//'Dashed vertical');
  cxSetResourceString(@sdxPatternDashedHorizontal,'竖虚线');
  //tt8//'Dashed horizontal');
  cxSetResourceString(@sdxPatternSmallConfetti,'小纸屑');
  //tt8//'Small confetti');
  cxSetResourceString(@sdxPatternLargeConfetti,'大纸屑');
  //tt8//'Large confetti');
  cxSetResourceString(@sdxPatternZigZag,'之字形');
  //tt8//'Zig zag');
  cxSetResourceString(@sdxPatternWave,'波浪线');
  //tt8//'Wave');
  cxSetResourceString(@sdxPatternDiagonalBrick,'对角砖形');
  //tt8//'Diagonal brick');
  cxSetResourceString(@sdxPatternHorizantalBrick,'横向砖形');
  //tt8//'Horizontal brick');
  cxSetResourceString(@sdxPatternWeave,'编织物');
  //tt8//'Weave');
  cxSetResourceString(@sdxPatternPlaid,'苏格兰方格呢');
  //tt8//'Plaid');
  cxSetResourceString(@sdxPatternDivot,'草皮');
  //tt8//'Divot');
  cxSetResourceString(@sdxPatternDottedGrid,'虚线网格');
  //tt8//'Dottedgrid');
  cxSetResourceString(@sdxPatternDottedDiamond,'点式菱形');
  //tt8//'Dotted diamond');
  cxSetResourceString(@sdxPatternShingle,'瓦形');
  //tt8//'Shingle');
  cxSetResourceString(@sdxPatternTrellis,'棚架');
  //tt8//'Trellis');
  cxSetResourceString(@sdxPatternSphere,'球体');
  //tt8//'Sphere');
  cxSetResourceString(@sdxPatternSmallGrid,'小网格');
  //tt8//'Small grid');
  cxSetResourceString(@sdxPatternLargeGrid,'大网格');
  //tt8//'Large grid');
  cxSetResourceString(@sdxPatternSmallCheckedBoard,'小棋盘');
  //tt8//'Small checked board');
  cxSetResourceString(@sdxPatternLargeCheckedBoard,'大棋盘');
  //tt8//'Large checked board');
  cxSetResourceString(@sdxPatternOutlinedDiamond,'轮廓式菱形');
  //tt8//'Outlined diamond');
  cxSetResourceString(@sdxPatternSolidDiamond,'实心菱形');
  //tt8//'Solid diamond');

  // Texture names

  cxSetResourceString(@sdxTextureNewSprint,'新闻纸');
  //tt8//'Newsprint');                                                                                                                                                                                                  
  cxSetResourceString(@sdxTextureGreenMarble,'绿色大理石');
  //tt8//'Green marble');                                                                                                                                                                                         
  cxSetResourceString(@sdxTextureBlueTissuePaper,'蓝色砂纸');
  //tt8//'Blue tissue paper');                                                                                                                                                                                  
  cxSetResourceString(@sdxTexturePapyrus,'纸莎草纸');
  //tt8//'Papyrus');                                                                                                                                                                                                    
  cxSetResourceString(@sdxTextureWaterDroplets,'水滴');
  //tt8//'Water droplets');                                                                                                                                                                                           
  cxSetResourceString(@sdxTextureCork,'软木塞');
  //tt8//'Cork');                                                                                                                                                                                                            
  cxSetResourceString(@sdxTextureRecycledPaper,'再生纸');
  //tt8//'Recycled paper');                                                                                                                                                                                         
  cxSetResourceString(@sdxTextureWhiteMarble,'白色大理石');
  //tt8//'White marble');                                                                                                                                                                                         
  cxSetResourceString(@sdxTexturePinkMarble,'粉色砂纸');
  //tt8//'Pink marble');                                                                                                                                                                                             
  cxSetResourceString(@sdxTextureCanvas,'画布');
  //tt8//'Canvas');                                                                                                                                                                                                          
  cxSetResourceString(@sdxTexturePaperBag,'纸袋');
  //tt8//'Paper bag');                                                                                                                                                                                                     
  cxSetResourceString(@sdxTextureWalnut,'胡桃');
  //tt8//'Walnut');                                                                                                                                                                                                          
  cxSetResourceString(@sdxTextureParchment,'羊皮纸');
  //tt8//'Parchment');                                                                                                                                                                                                  
  cxSetResourceString(@sdxTextureBrownMarble,'棕色大理石');
  //tt8//'Brown marble');                                                                                                                                                                                         
  cxSetResourceString(@sdxTexturePurpleMesh,'紫色网格');
  //tt8//'Purple mesh');                                                                                                                                                                                             
  cxSetResourceString(@sdxTextureDenim,'斜纹布');
  //tt8//'Denim');                                                                                                                                                                                                          
  cxSetResourceString(@sdxTextureFishFossil,'鱼类化石');
  //tt8//'Fish fossil');                                                                                                                                                                                             
  cxSetResourceString(@sdxTextureOak,'栎木');
  //tt8//'Oak');                                                                                                                                                                                                                
  cxSetResourceString(@sdxTextureStationary,'信纸');
  //tt8//'Stationary');                                                                                                                                                                                                  
  cxSetResourceString(@sdxTextureGranite,'花岗岩');
  //tt8//'Granite');                                                                                                                                                                                                      
  cxSetResourceString(@sdxTextureBouquet,'花束');
  //tt8//'Bouquet');                                                                                                                                                                                                        
  cxSetResourceString(@sdxTextureWonenMat,'编织物');
  //tt8//'Woven mat');                                                                                                                                                                                                   
  cxSetResourceString(@sdxTextureSand,'沙滩');
  //tt8//'Sand');                                                                                                                                                                                                              
  cxSetResourceString(@sdxTextureMediumWood,'深色木质');
  //tt8//'Medium wood');                                                                                                                                                                                             
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxFSPCaption,'图像预览');
  //tt8//'Picture Preview');                                                                                                                                                                                                
  cxSetResourceString(@sdxWidth,'宽度');
  //tt8//'Width');                                                                                                                                                                                                                   
  cxSetResourceString(@sdxHeight,'高度');
  //tt8//'Height');                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                          
  // Brush Dialog

  cxSetResourceString(@sdxBrushDlgCaption,'画笔属性');
  //tt8//'Brush properties');                                                                                                                                                                                          
  cxSetResourceString(@sdxStyle,'样式:');
  //tt8//'&Style:');                                                                                                                                                                                                                
                                                                                                                                                                                                                                                          
  // Enter New File Name dialog

  cxSetResourceString(@sdxENFNCaption,'选择新文件名称');
  //tt8//'Choose New File Name');                                                                                                                                                                                    
  cxSetResourceString(@sdxEnterNewFileName,'输入新文件名称');
  //tt8//'Enter New File Name');                                                                                                                                                                                
                                                                                                                                                                                                                                                          
  // Define styles dialog

  cxSetResourceString(@sdxDefinePrintStylesCaption,'定义打印样式');
  //tt8//'Define Print Styles');                                                                                                                                                                          
  cxSetResourceString(@sdxDefinePrintStylesTitle,'打印样式(&S)');
  //tt8//'Print &Styles');                                                                                                                                                                                  
  cxSetResourceString(@sdxDefinePrintStylesWarningDelete,'确认要删除 "%s" 吗?');
  //tt8//'Do you want to delete "%s" ?');                                                                                                                                                    
  cxSetResourceString(@sdxDefinePrintStylesWarningClear,'要删除所有非内置样式吗?');
  //tt8//'Do you want to delete all not built-in styles ?');                                                                                                                              
  cxSetResourceString(@sdxClear,'清除(&L)...');
  //tt8//'C&lear...');                                                                                                                                                                                                        

  // Print device

  cxSetResourceString(@sdxCustomSize,'自定义大小');
  //tt8//'Custom Size');                                                                                                                                                                                                  
  cxSetResourceString(@sdxDefaultTray,'默认纸盒');
  //tt8//'Default Tray');                                                                                                                                                                                                  
  cxSetResourceString(@sdxInvalidPrintDevice,'所选打印机无效');
  //tt8//'Printer selected is not valid');                                                                                                                                                                    
  cxSetResourceString(@sdxNotPrinting,'当前打印机不打印');
  //tt8//'Printer is not currently printing');                                                                                                                                                                     
  cxSetResourceString(@sdxPrinting,'正在打印');
  //tt8//'Printing in progress');                                                                                                                                                                                             
  cxSetResourceString(@sdxDeviceOnPort,'%s 在 %s');
  //tt8//'%s on %s');                                                                                                                                                                                                     
  cxSetResourceString(@sdxPrinterIndexError,'打印机索引超出范围');
  //tt8//'Printer index out of range');                                                                                                                                                                    
  cxSetResourceString(@sdxNoDefaultPrintDevice,'没有选择默认打印机');
  //tt8//'There is no default printer selected');                                                                                                                                                       
                                                                                                                                                                                                                                                          
  // Edit AutoText Entries Dialog

  cxSetResourceString(@sdxAutoTextDialogCaption,'编辑自动图文集');
  //tt8//'Edit AutoText Entries');                                                                                                                                                                         
  cxSetResourceString(@sdxEnterAutoTextEntriesHere,'输入自动图文集：');
  //tt8//' Enter A&utoText Entries Here: ');                                                                                                                                                          
                                                                                                                                                                                                                                                          
  // Print dialog

  cxSetResourceString(@sdxPrintDialogCaption,'打印');
  //tt8//'Print');                                                                                                                                                                                                      
  cxSetResourceString(@sdxPrintDialogPrinter,'打印机');
  //tt8//' Printer ');                                                                                                                                                                                                
  cxSetResourceString(@sdxPrintDialogName,'名称(&N):');
  //tt8//'&Name:');                                                                                                                                                                                                   
  cxSetResourceString(@sdxPrintDialogStatus,'状态:');
  //tt8//'Status:');                                                                                                                                                                                                    
  cxSetResourceString(@sdxPrintDialogType,'类型:');
  //tt8//'Type:');                                                                                                                                                                                                        
  cxSetResourceString(@sdxPrintDialogWhere,'位置:');
  //tt8//'Where:');                                                                                                                                                                                                      
  cxSetResourceString(@sdxPrintDialogComment,'备注:');
  //tt8//'Comment:');                                                                                                                                                                                                  
  cxSetResourceString(@sdxPrintDialogPrintToFile,'打印到文件(&F)');
  //tt8//'Print to &File');                                                                                                                                                                               
  cxSetResourceString(@sdxPrintDialogPageRange,' 页面范围 ');
  //tt8//' Page range ');                                                                                                                                                                                       
  cxSetResourceString(@sdxPrintDialogAll,'全部(&A)');
  //tt8//'&All');                                                                                                                                                                                                       
  cxSetResourceString(@sdxPrintDialogCurrentPage,'当前页(&E)');
  //tt8//'Curr&ent Page');                                                                                                                                                                                    
  cxSetResourceString(@sdxPrintDialogSelection,'所选内容(&S)');
  //tt8//'&Selection');                                                                                                                                                                                       
  cxSetResourceString(@sdxPrintDialogPages,'页码范围:');
  //tt8//'&Pages:');                                                                                                                                                                                                 
  cxSetResourceString(@sdxPrintDialogRangeLegend,'请键入页码和/或用逗号分隔的页码范围'+#10#13+
  //tt8//'Enter page number and/or page ranges' + #10#13 +                                                                                                                   
    'separated by commas. For example: 1,3,5-12.');                                                                                                                                                                                                        
  cxSetResourceString(@sdxPrintDialogCopies,' 副本');
  //tt8//' Copies ');                                                                                                                                                                                                   
  cxSetResourceString(@sdxPrintDialogNumberOfPages,'页数(&U):');
  //tt8//'N&umber of Pages:');                                                                                                                                                                               
  cxSetResourceString(@sdxPrintDialogNumberOfCopies,'份数(&C):');
  //tt8//'Number of &Copies:');                                                                                                                                                                             
  cxSetResourceString(@sdxPrintDialogCollateCopies,'逐份打印(&T)');
  //tt8//'Colla&te Copies');                                                                                                                                                                              
  cxSetResourceString(@sdxPrintDialogAllPages,'全部');
  //tt8//'All');                                                                                                                                                                                                       
  cxSetResourceString(@sdxPrintDialogEvenPages,'偶数页');
  //tt8//'Even');                                                                                                                                                                                                   
  cxSetResourceString(@sdxPrintDialogOddPages,'奇数页');
  //tt8//'Odd');                                                                                                                                                                                                     
  cxSetResourceString(@sdxPrintDialogPrintStyles,' 打印样式(&Y)');
  //tt8//' Print St&yles ');                                                                                                                                                                               
                                                                                                                                                                                                                                                          
  // PrintToFile Dialog

  cxSetResourceString(@sdxPrintDialogOpenDlgTitle,'选择文件名称');
  //tt8//'Choose File Name');                                                                                                                                                                              
  cxSetResourceString(@sdxPrintDialogOpenDlgAllFiles,'全部文件');
  //tt8//'All Files');                                                                                                                                                                                      
  cxSetResourceString(@sdxPrintDialogOpenDlgPrinterFiles,'打印机文件');
  //tt8//'Printer Files');                                                                                                                                                                            
  cxSetResourceString(@sdxPrintDialogPageNumbersOutOfRange,'页码超出范围 (%d - %d)');
  //tt8//'Page numbers out of range (%d - %d)');                                                                                                                                        
  cxSetResourceString(@sdxPrintDialogInvalidPageRanges,'无效的页码范围');
  //tt8//'Invalid page ranges');                                                                                                                                                                    
  cxSetResourceString(@sdxPrintDialogRequiredPageNumbers,'输入页码');
  //tt8//'Enter page numbers');                                                                                                                                                                         
  cxSetResourceString(@sdxPrintDialogNoPrinters,'没有安装打印机。 要安装打印机，'+
  //tt8//'No printers are installed. To install a printer, ' +                                                                                                                           
    'point to Settings on the Windows Start menu, click Printers, and then double-click Add Printer. ' +                                                                                                                                                  
    'Follow the instructions in the wizard.');                                                                                                                                                                                                             
  cxSetResourceString(@sdxPrintDialogInPrintingState,'打印机正在打印。'+#10#13+
  //tt8//'Printer is currently printing.' + #10#13 +                                                                                                                                        
    'Please wait.');                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                          
  // Printer State

  cxSetResourceString(@sdxPrintDialogPSPaused,'暂停');
  //tt8//'Paused');                                                                                                                                                                                                    
  cxSetResourceString(@sdxPrintDialogPSPendingDeletion,'正在删除');
  //tt8//'Pending Deletion');                                                                                                                                                                             
  cxSetResourceString(@sdxPrintDialogPSBusy,'繁忙');
  //tt8//'Busy');                                                                                                                                                                                                        
  cxSetResourceString(@sdxPrintDialogPSDoorOpen,'通道打开');
  //tt8//'Door Open');                                                                                                                                                                                           
  cxSetResourceString(@sdxPrintDialogPSError,'错误');
  //tt8//'Error');                                                                                                                                                                                                      
  cxSetResourceString(@sdxPrintDialogPSInitializing,'初始化');
  //tt8//'Initializing');                                                                                                                                                                                      
  cxSetResourceString(@sdxPrintDialogPSIOActive,'输入输出有效');
  //tt8//'IO Active');                                                                                                                                                                                       
  cxSetResourceString(@sdxPrintDialogPSManualFeed,'手工送纸');
  //tt8//'Manual Feed');                                                                                                                                                                                       
  cxSetResourceString(@sdxPrintDialogPSNoToner,'没有墨粉');
  //tt8//'No Toner');                                                                                                                                                                                             
  cxSetResourceString(@sdxPrintDialogPSNotAvailable,'不可用');
  //tt8//'Not Available');                                                                                                                                                                                     
  cxSetResourceString(@sdxPrintDialogPSOFFLine,'脱机');
  //tt8//'Offline');                                                                                                                                                                                                  
  cxSetResourceString(@sdxPrintDialogPSOutOfMemory,'内存溢出');
  //tt8//'Out of Memory');                                                                                                                                                                                    
  cxSetResourceString(@sdxPrintDialogPSOutBinFull,'输出储存器已满');
  //tt8//'Output Bin Full');                                                                                                                                                                             
  cxSetResourceString(@sdxPrintDialogPSPagePunt,'页平底');
  //tt8//'Page Punt');                                                                                                                                                                                             
  cxSetResourceString(@sdxPrintDialogPSPaperJam,'卡纸');
  //tt8//'Paper Jam');                                                                                                                                                                                               
  cxSetResourceString(@sdxPrintDialogPSPaperOut,'纸张跳出');
  //tt8//'Paper Out');                                                                                                                                                                                           
  cxSetResourceString(@sdxPrintDialogPSPaperProblem,'纸张问题');
  //tt8//'Paper Problem');                                                                                                                                                                                   
  cxSetResourceString(@sdxPrintDialogPSPrinting,'正在打印');
  //tt8//'Printing');                                                                                                                                                                                            
  cxSetResourceString(@sdxPrintDialogPSProcessing,'正在处理');
  //tt8//'Processing');                                                                                                                                                                                        
  cxSetResourceString(@sdxPrintDialogPSTonerLow,'墨粉较少');
  //tt8//'Toner Low');                                                                                                                                                                                           
  cxSetResourceString(@sdxPrintDialogPSUserIntervention,'需用户干涉');
  //tt8//'User Intervention');                                                                                                                                                                         
  cxSetResourceString(@sdxPrintDialogPSWaiting,'正在等待');
  //tt8//'Waiting');                                                                                                                                                                                              
  cxSetResourceString(@sdxPrintDialogPSWarningUp,'正在预热');
  //tt8//'Warming Up');                                                                                                                                                                                         
  cxSetResourceString(@sdxPrintDialogPSReady,'就绪');
  //tt8//'Ready');                                                                                                                                                                                                      
  cxSetResourceString(@sdxPrintDialogPSPrintingAndWaiting,'正在打印：%d document(s)  请等待');
  //tt8//'Printing: %d document(s) waiting');                                                                                                                                  
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxLeftMargin,'左边距');
  //tt8//'Left Margin');                                                                                                                                                                                                      
  cxSetResourceString(@sdxTopMargin,'上边距');
  //tt8//'Top Margin');                                                                                                                                                                                                        
  cxSetResourceString(@sdxRightMargin,'右边距');
  //tt8//'Right Margin');                                                                                                                                                                                                    
  cxSetResourceString(@sdxBottomMargin,'下边距');
  //tt8//'Bottom Margin');                                                                                                                                                                                                  
  cxSetResourceString(@sdxGutterMargin,'装订线');
  //tt8//'Gutter');                                                                                                                                                                                                         
  cxSetResourceString(@sdxHeaderMargin,'页眉');
  //tt8//'Header');                                                                                                                                                                                                           
  cxSetResourceString(@sdxFooterMargin,'页脚');
  //tt8//'Footer');                                                                                                                                                                                                           
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxUnitsInches,'"');
  //tt8//'"');                                                                                                                                                                                                                    
  cxSetResourceString(@sdxUnitsCentimeters,'厘米');
  //tt8//'cm');                                                                                                                                                                                                           
  cxSetResourceString(@sdxUnitsMillimeters,'毫米');
  //tt8//'mm');                                                                                                                                                                                                           
  cxSetResourceString(@sdxUnitsPoints,'磅');
  //tt8//'pt');                                                                                                                                                                                                                  
  cxSetResourceString(@sdxUnitsPicas,'像素');
  //tt8//'pi');                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxUnitsDefaultName,'默认');
  //tt8//'Default');                                                                                                                                                                                                      
  cxSetResourceString(@sdxUnitsInchesName,'英寸');
  //tt8//'Inches');                                                                                                                                                                                                        
  cxSetResourceString(@sdxUnitsCentimetersName,'厘米');
  //tt8//'Centimeters');                                                                                                                                                                                              
  cxSetResourceString(@sdxUnitsMillimetersName,'毫米');
  //tt8//'Millimeters');                                                                                                                                                                                              
  cxSetResourceString(@sdxUnitsPointsName,'磅');
  //tt8//'Points');                                                                                                                                                                                                          
  cxSetResourceString(@sdxUnitsPicasName,'派卡');
  //tt8//'Picas');                                                                                                                                                                                                          
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxPrintPreview,'打印预览');
  //tt8//'Print Preview');                                                                                                                                                                                                
  cxSetResourceString(@sdxReportDesignerCaption,'报表设计');
  //tt8//'Format Report');                                                                                                                                                                                       
  cxSetResourceString(@sdxCompositionDesignerCaption,'布局设计');
  //tt8//'Composition Editor');                                                                                                                                                                             
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxComponentNotSupportedByLink,'组件 "%s" 不被打印组件支持');
  //tt8//'Component "%s" not supported by TdxComponentPrinter');                                                                                                                         
  cxSetResourceString(@sdxComponentNotSupported,'组件 "%s" 不被打印组件支持');
  //tt8//'Component "%s" not supported by TdxComponentPrinter');                                                                                                                               
  cxSetResourceString(@sdxPrintDeviceNotReady,'打印机尚未安装或者没有就绪');
  //tt8//'Printer has not been installed or is not ready');                                                                                                                                      
  cxSetResourceString(@sdxUnableToGenerateReport,'不能产生报表');
  //tt8//'Unable to generate report');                                                                                                                                                                      
  cxSetResourceString(@sdxPreviewNotRegistered,'没有已注册的预览表单');
  //tt8//'There is no registered preview form');                                                                                                                                                      
  cxSetResourceString(@sdxComponentNotAssigned,'%s' + #13#10 + '没有指定组件属性');
  //tt8//'%s' + #13#10 + 'Not assigned "Component" property');                                                                                                                            
  cxSetResourceString(@sdxPrintDeviceIsBusy,'打印机正忙');
  //tt8//'Printer is busy');                                                                                                                                                                                       
  cxSetResourceString(@sdxPrintDeviceError,'打印机出错!');
  //tt8//'Printer has encountered error !');                                                                                                                                                                       
  cxSetResourceString(@sdxMissingComponent,'缺少组件属性');
  //tt8//'Missing "Component" property');                                                                                                                                                                         
  cxSetResourceString(@sdxDataProviderDontPresent,'在布局中没有指定连接的组件');
  //tt8//'There are no Links with Assigned Component in Composition');                                                                                                                       
  cxSetResourceString(@sdxBuildingReport,'构建报表：已完成 %d%%');
  //tt8//'Building report: Completed %d%%');                            // obsolete                                                                                                                        
  cxSetResourceString(@sdxPrintingReport,'正在打印报表：已完成 %d 页。 按ESC键中断...');
  //tt8//'Printing report: Completed %d page(s). Press Esc to cancel'); // obsolete                                                                                                  
  cxSetResourceString(@sdxDefinePrintStylesMenuItem,'定义打印样式(&S)...');
  //tt8//'Define Print &Styles...');                                                                                                                                                              
  cxSetResourceString(@sdxAbortPrinting,'要中断打印吗?');
  //tt8//'Abort printing ?');                                                                                                                                                                                       
  cxSetResourceString(@sdxStandardStyle,'标准样式');
  //tt8//'Standard Style');                                                                                                                                                                                              
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxFontStyleBold,'黑体');
  //tt8//'Bold');                                                                                                                                                                                                            
  cxSetResourceString(@sdxFontStyleItalic,'斜体');
  //tt8//'Italic');                                                                                                                                                                                                        
  cxSetResourceString(@sdxFontStyleUnderline,'下划线');
  //tt8//'Underline');                                                                                                                                                                                                
  cxSetResourceString(@sdxFontStyleStrikeOut,'删除线');
  //tt8//'StrikeOut');                                                                                                                                                                                                
  cxSetResourceString(@sdxPt,'磅');
  //tt8//'pt.');                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxNoPages,'[没有页面]');
  //tt8//'There are no pages to display');                                                                                                                                                                                   
  cxSetResourceString(@sdxPageWidth,'页宽');
  //tt8//'Page Width');                                                                                                                                                                                                          
  cxSetResourceString(@sdxWholePage,'整页');
  //tt8//'Whole Page');                                                                                                                                                                                                          
  cxSetResourceString(@sdxTwoPages,'两页');
  //tt8//'Two Pages');                                                                                                                                                                                                            
  cxSetResourceString(@sdxFourPages,'四页');
  //tt8//'Four Pages');                                                                                                                                                                                                          
  cxSetResourceString(@sdxWidenToSourceWidth,'原始宽度');
  //tt8//'Widen to Source Width');                                                                                                                                                                                  
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuBar,'菜单栏');
  //tt8//'MenuBar');                                                                                                                                                                                                             
  cxSetResourceString(@sdxStandardBar,'标准');
  //tt8//'Standard');                                                                                                                                                                                                          
  cxSetResourceString(@sdxHeaderFooterBar,'页眉和页脚');
  //tt8//'Header and Footer');                                                                                                                                                                                       
  cxSetResourceString(@sdxShortcutMenusBar,'快捷菜单');
  //tt8//'Shortcut Menus');                                                                                                                                                                                           
  cxSetResourceString(@sdxAutoTextBar,'自动图文集');
  //tt8//'AutoText');                                                                                                                                                                                                    
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuFile,'文件(&F)');
  //tt8//'&File');                                                                                                                                                                                                            
  cxSetResourceString(@sdxMenuFileDesign,'设计(&D)...');
  //tt8//'&Design...');                                                                                                                                                                                              
  cxSetResourceString(@sdxMenuFilePrint,'打印(&P)...');
  //tt8//'&Print...');                                                                                                                                                                                                
  cxSetResourceString(@sdxMenuFilePageSetup,'页面设置(&U)...');
  //tt8//'Page Set&up...');                                                                                                                                                                                   
  cxSetResourceString(@sdxMenuPrintStyles,'打印样式');
  //tt8//'Print Styles');                                                                                                                                                                                              
  cxSetResourceString(@sdxMenuFileExit,'关闭(&C)');
  //tt8//'&Close');                                                                                                                                                                                                       
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuEdit,'编辑(&E)');
  //tt8//'&Edit');                                                                                                                                                                                                            
  cxSetResourceString(@sdxMenuEditCut,'剪切(&T)');
  //tt8//'Cu&t');                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuEditCopy,'复制(&C)');
  //tt8//'&Copy');                                                                                                                                                                                                        
  cxSetResourceString(@sdxMenuEditPaste,'粘贴(&P)');
  //tt8//'&Paste');                                                                                                                                                                                                      
  cxSetResourceString(@sdxMenuEditDelete,'删除(&D)');
  //tt8//'&Delete');                                                                                                                                                                                                    
  cxSetResourceString(@sdxMenuEditFind,'查找(&F)...');
  //tt8//'&Find...');                                                                                                                                                                                                  
  cxSetResourceString(@sdxMenuEditFindNext,'查找下一个(&X)');
  //tt8//'Find Ne&xt');                                                                                                                                                                                         
  cxSetResourceString(@sdxMenuEditReplace,'替换(&R)...');
  //tt8//'&Replace...');                                                                                                                                                                                            
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuLoad,'加载(&L)...');
  //tt8//'&Load...');                                                                                                                                                                                                      
  cxSetResourceString(@sdxMenuPreview,'预览(&V)...');
  //tt8//'Pre&view...');                                                                                                                                                                                                
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuInsert,'插入(&I)');
  //tt8//'&Insert');                                                                                                                                                                                                        
  cxSetResourceString(@sdxMenuInsertAutoText,'自动图文集(&A)');
  //tt8//'&AutoText');                                                                                                                                                                                        
  cxSetResourceString(@sdxMenuInsertEditAutoTextEntries,'自动图文集(&X)...');
  //tt8//'AutoTe&xt...');                                                                                                                                                                       
  cxSetResourceString(@sdxMenuInsertAutoTextEntries,'自动图文集列表');
  //tt8//'List of AutoText Entries');                                                                                                                                                                  
  cxSetResourceString(@sdxMenuInsertAutoTextEntriesSubItem,'插入自动图文集(&S)');
  //tt8//'In&sert AutoText');                                                                                                                                                               
  cxSetResourceString(@sdxMenuInsertPageNumber,'页码(&P)');
  //tt8//'&Page Number');                                                                                                                                                                                         
  cxSetResourceString(@sdxMenuInsertTotalPages,'页数(&N)');
  //tt8//'&Number of Pages');                                                                                                                                                                                     
  cxSetResourceString(@sdxMenuInsertPageOfPages,'页面页码(&G)');
  //tt8//'Pa&ge Number of Pages');                                                                                                                                                                           
  cxSetResourceString(@sdxMenuInsertDateTime,'日期和时间');
  //tt8//'Date and Time');                                                                                                                                                                                        
  cxSetResourceString(@sdxMenuInsertDate,'日期(&D)');
  //tt8//'&Date');                                                                                                                                                                                                      
  cxSetResourceString(@sdxMenuInsertTime,'时间(&T)');
  //tt8//'&Time');                                                                                                                                                                                                      
  cxSetResourceString(@sdxMenuInsertUserName,'用户名称(&U)');
  //tt8//'&User Name');                                                                                                                                                                                         
  cxSetResourceString(@sdxMenuInsertMachineName,'机器名称(&M)');
  //tt8//'&Machine Name');                                                                                                                                                                                   
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuView,'视图(&V)');
  //tt8//'&View');                                                                                                                                                                                                            
  cxSetResourceString(@sdxMenuViewMargins,'页边距(&M)');
  //tt8//'&Margins');                                                                                                                                                                                                
  cxSetResourceString(@sdxMenuViewFlatToolBarButtons,'平面工具栏按钮');
  //tt8//'&Flat ToolBar Buttons');                                                                                                                                                                    
  cxSetResourceString(@sdxMenuViewLargeToolBarButtons,'大工具栏按钮');
  //tt8//'&Large ToolBar Buttons');                                                                                                                                                                    
  cxSetResourceString(@sdxMenuViewMarginsStatusBar,'页边距栏');
  //tt8//'M&argins Bar');                                                                                                                                                                                     
  cxSetResourceString(@sdxMenuViewPagesStatusBar,'状态栏');
  //tt8//'&Status Bar');                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuViewToolBars,'工具栏');
  //tt8//'&Toolbars');                                                                                                                                                                                                  
  cxSetResourceString(@sdxMenuViewPagesHeaders,'页眉');
  //tt8//'Page &Headers');                                                                                                                                                                                            
  cxSetResourceString(@sdxMenuViewPagesFooters,'页脚');
  //tt8//'Page Foote&rs');                                                                                                                                                                                            
  cxSetResourceString(@sdxMenuViewSwitchToLeftPart,'切换到左部');
  //tt8//'Switch to Left Part');                                                                                                                                                                            
  cxSetResourceString(@sdxMenuViewSwitchToRightPart,'切换到右部');
  //tt8//'Switch to Right Part');                                                                                                                                                                          
  cxSetResourceString(@sdxMenuViewSwitchToCenterPart,'切换到中部');
  //tt8//'Switch to Center Part');                                                                                                                                                                        
  cxSetResourceString(@sdxMenuViewHFSwitchHeaderFooter,'显示页眉/页脚(&S)');
  //tt8//'&Show Header/Footer');                                                                                                                                                                 
  cxSetResourceString(@sdxMenuViewHFClose,'关闭(&C)');
  //tt8//'&Close');                                                                                                                                                                                                    
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuZoom,'缩放(&Z)');
  //tt8//'&Zoom');                                                                                                                                                                                                            
  cxSetResourceString(@sdxMenuZoomPercent100,'百分&100');
  //tt8//'Percent &100');                                                                                                                                                                                           
  cxSetResourceString(@sdxMenuZoomPageWidth,'页宽(&W)');
  //tt8//'Page &Width');                                                                                                                                                                                             
  cxSetResourceString(@sdxMenuZoomWholePage,'整页(&H)');
  //tt8//'W&hole Page');                                                                                                                                                                                             
  cxSetResourceString(@sdxMenuZoomTwoPages,'两页(&T)');
  //tt8//'&Two Pages');                                                                                                                                                                                               
  cxSetResourceString(@sdxMenuZoomFourPages,'四页(&F)');
  //tt8//'&Four Pages');                                                                                                                                                                                             
  cxSetResourceString(@sdxMenuZoomMultiplyPages,'多页(&M)');
  //tt8//'&Multiple Pages');                                                                                                                                                                                     
  cxSetResourceString(@sdxMenuZoomWidenToSourceWidth,'扩展到原始宽度');
  //tt8//'Widen To S&ource Width');                                                                                                                                                                   
  cxSetResourceString(@sdxMenuZoomSetup,'设置(&S)...');
  //tt8//'&Setup...');                                                                                                                                                                                                
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuPages,'页面(&P)');
  //tt8//'&Pages');                                                                                                                                                                                                          
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuGotoPage,'转到(&G)');
  //tt8//'&Go');                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuGotoPageFirst,'首页(&F)');
  //tt8//'&First Page');                                                                                                                                                                                             
  cxSetResourceString(@sdxMenuGotoPagePrev,'前一页(&P)');
  //tt8//'&Previous Page');                                                                                                                                                                                         
  cxSetResourceString(@sdxMenuGotoPageNext,'下一页(&N)');
  //tt8//'&Next Page');                                                                                                                                                                                             
  cxSetResourceString(@sdxMenuGotoPageLast,'尾页(&L)');
  //tt8//'&Last Page');                                                                                                                                                                                               
  cxSetResourceString(@sdxMenuActivePage,'当前页(&A):');
  //tt8//'&Active Page:');                                                                                                                                                                                           
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuFormat,'格式(&O)');
  //tt8//'F&ormat');                                                                                                                                                                                                        
  cxSetResourceString(@sdxMenuFormatHeaderAndFooter,'页眉和页脚');
  //tt8//'&Header and Footer');                                                                                                                                                                            
  cxSetResourceString(@sdxMenuFormatAutoTextEntries,'自动图文集(&A)...');
  //tt8//'&Auto Text Entries...');                                                                                                                                                                  
  cxSetResourceString(@sdxMenuFormatDateTime,'日期和时间(&T)...');
  //tt8//'Date And &Time...');                                                                                                                                                                             
  cxSetResourceString(@sdxMenuFormatPageNumbering,'页码(&N)...');
  //tt8//'Page &Numbering...');                                                                                                                                                                             
  cxSetResourceString(@sdxMenuFormatPageBackground,'背景(&K)...');
  //tt8//'Bac&kground...');                                                                                                                                                                                
  cxSetResourceString(@sdxMenuFormatShrinkToPage,'缩小适合页面(&F)');
  //tt8//'&Fit to Page');                                                                                                                                                                               
  cxSetResourceString(@sdxMenuShowEmptyPages,'显示空白页(&E)');
  //tt8//'Show &Empty Pages');                                                                                                                                                                                
  cxSetResourceString(@sdxMenuFormatHFBackground,'页眉/页脚背景...');
  //tt8//'Header/Footer Background...');                                                                                                                                                                
  cxSetResourceString(@sdxMenuFormatHFClear,'清除文本');
  //tt8//'Clear Text');                                                                                                                                                                                              
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuTools,'工具(&T)');
  //tt8//'&Tools');                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuToolsCustomize,'自定义(&C)...');
  //tt8//'&Customize...');                                                                                                                                                                                     
  cxSetResourceString(@sdxMenuToolsOptions,'选项(&O)...');
  //tt8//'&Options...');                                                                                                                                                                                           
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuHelp,'帮助(&H)');
  //tt8//'&Help');                                                                                                                                                                                                            
  cxSetResourceString(@sdxMenuHelpTopics,'帮助主题(&T)...');
  //tt8//'Help &Topics...');                                                                                                                                                                                     
  cxSetResourceString(@sdxMenuHelpAbout,'关于(&A)...');
  //tt8//'&About...');                                                                                                                                                                                                
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuShortcutPreview,'预览');
  //tt8//'Preview');                                                                                                                                                                                                   
  cxSetResourceString(@sdxMenuShortcutAutoText,'自动图文集');
  //tt8//'AutoText');                                                                                                                                                                                           
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuBuiltInMenus,'内置菜单');
  //tt8//'Built-in Menus');                                                                                                                                                                                           
  cxSetResourceString(@sdxMenuShortCutMenus,'快捷菜单');
  //tt8//'Shortcut Menus');                                                                                                                                                                                          
  cxSetResourceString(@sdxMenuNewMenu,'新建菜单');
  //tt8//'New Menu');                                                                                                                                                                                                      
                                                                                                                                                                                                                                                          
  // Hints

  cxSetResourceString(@sdxHintFileDesign,'设计报表');
  //tt8//'Design Report');                                                                                                                                                                                              
  cxSetResourceString(@sdxHintFilePrint,'打印');
  //tt8//'Print');                                                                                                                                                                                                           
  cxSetResourceString(@sdxHintFilePrintDialog,'打印对话框');
  //tt8//'Print Dialog');                                                                                                                                                                                        
  cxSetResourceString(@sdxHintFilePageSetup,'页面设置');
  //tt8//'Page Setup');                                                                                                                                                                                              
  cxSetResourceString(@sdxHintFileExit,'关闭预览');
  //tt8//'Close Preview');                                                                                                                                                                                                
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxHintEditFind,'查找');
  //tt8//'Find');                                                                                                                                                                                                             
  cxSetResourceString(@sdxHintEditFindNext,'查找下一个');
  //tt8//'Find Next');                                                                                                                                                                                              
  cxSetResourceString(@sdxHintEditReplace,'替换');
  //tt8//'Replace');                                                                                                                                                                                                       
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxHintInsertEditAutoTextEntries,'编辑自动图文集');
  //tt8//'Edit AutoText Entries');                                                                                                                                                                 
  cxSetResourceString(@sdxHintInsertPageNumber,'插入页码');
  //tt8//'Insert Page Number');                                                                                                                                                                                   
  cxSetResourceString(@sdxHintInsertTotalPages,'插入页数');
  //tt8//'Insert Number of Pages');                                                                                                                                                                               
  cxSetResourceString(@sdxHintInsertPageOfPages,'插入页范');
  //tt8//'Insert Page Number of Pages');                                                                                                                                                                         
  cxSetResourceString(@sdxHintInsertDateTime,'插入日期和时间');
  //tt8//'Insert Date and Time');                                                                                                                                                                             
  cxSetResourceString(@sdxHintInsertDate,'插入日期');
  //tt8//'Insert Date');                                                                                                                                                                                                
  cxSetResourceString(@sdxHintInsertTime,'插入时间');
  //tt8//'Insert Time');                                                                                                                                                                                                
  cxSetResourceString(@sdxHintInsertUserName,'插入用户名称');
  //tt8//'Insert User Name');                                                                                                                                                                                   
  cxSetResourceString(@sdxHintInsertMachineName,'插入机器名称');
  //tt8//'Insert Machine Name');                                                                                                                                                                             
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxHintViewMargins,'查看页边距');
  //tt8//'View Margins');                                                                                                                                                                                            
  cxSetResourceString(@sdxHintViewLargeButtons,'查看大按钮');
  //tt8//'View Large Buttons');                                                                                                                                                                                 
  cxSetResourceString(@sdxHintViewMarginsStatusBar,'查看页边距状态栏');
  //tt8//'View Margins Status Bar');                                                                                                                                                                  
  cxSetResourceString(@sdxHintViewPagesStatusBar,'查看页面状态栏');
  //tt8//'View Page Status Bar');                                                                                                                                                                         
  cxSetResourceString(@sdxHintViewPagesHeaders,'查看页眉');
  //tt8//'View Page Header');                                                                                                                                                                                     
  cxSetResourceString(@sdxHintViewPagesFooters,'查看页脚');
  //tt8//'View Page Footer');                                                                                                                                                                                     
  cxSetResourceString(@sdxHintViewSwitchToLeftPart,'切换到左边的页眉/页脚');
  //tt8//'Switch to Left Header/Footer Part');                                                                                                                                                   
  cxSetResourceString(@sdxHintViewSwitchToRightPart,'切换到右边的页眉/页脚');
  //tt8//'Switch to Right Header/Footer Part');                                                                                                                                                 
  cxSetResourceString(@sdxHintViewSwitchToCenterPart,'切换到中间的页眉/页脚');
  //tt8//'Switch to Center Header/Footer Part');                                                                                                                                               
  cxSetResourceString(@sdxHintViewHFSwitchHeaderFooter,'在页眉和页脚之间切换');
  //tt8//'Switch Between Header and Footer');                                                                                                                                                 
  cxSetResourceString(@sdxHintViewHFClose,'关闭');
  //tt8//'Close');                                                                                                                                                                                                         
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxHintViewZoom,'缩放');
  //tt8//'Zoom');                                                                                                                                                                                                             
  cxSetResourceString(@sdxHintZoomPercent100,'百分100%');
  //tt8//'Zoom 100%');                                                                                                                                                                                              
  cxSetResourceString(@sdxHintZoomPageWidth,'页宽');
  //tt8//'Zoom Page Width');                                                                                                                                                                                             
  cxSetResourceString(@sdxHintZoomWholePage,'整页');
  //tt8//'Whole Page');                                                                                                                                                                                                  
  cxSetResourceString(@sdxHintZoomTwoPages,'两页');
  //tt8//'Two Pages');                                                                                                                                                                                                    
  cxSetResourceString(@sdxHintZoomFourPages,'四页');
  //tt8//'Four Pages');                                                                                                                                                                                                  
  cxSetResourceString(@sdxHintZoomMultiplyPages,'多页');
  //tt8//'Multiple Pages');                                                                                                                                                                                          
  cxSetResourceString(@sdxHintZoomWidenToSourceWidth,'扩展到原始宽度');
  //tt8//'Widen To Source Width');                                                                                                                                                                    
  cxSetResourceString(@sdxHintZoomSetup,'设置缩放比例');
  //tt8//'Setup Zoom Factor');                                                                                                                                                                                       
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxHintFormatDateTime,'格式化日期和时间');
  //tt8//'Format Date and Time');                                                                                                                                                                           
  cxSetResourceString(@sdxHintFormatPageNumbering,'格式化页码');
  //tt8//'Format Page Number');                                                                                                                                                                              
  cxSetResourceString(@sdxHintFormatPageBackground,'背景');
  //tt8//'Background');                                                                                                                                                                                           
  cxSetResourceString(@sdxHintFormatShrinkToPage,'缩小适合页面');
  //tt8//'Shrink To Page');                                                                                                                                                                                 
  cxSetResourceString(@sdxHintFormatHFBackground,'页眉/页脚背景');
  //tt8//'Header/Footer Background');                                                                                                                                                                      
  cxSetResourceString(@sdxHintFormatHFClear,'清除页眉/页脚文本');
  //tt8//'Clear Header/Footer Text');                                                                                                                                                                       
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxHintGotoPageFirst,'首页');
  //tt8//'First Page');                                                                                                                                                                                                  
  cxSetResourceString(@sdxHintGotoPagePrev,'前一页');
  //tt8//'Previous Page');                                                                                                                                                                                              
  cxSetResourceString(@sdxHintGotoPageNext,'下一页');
  //tt8//'Next Page');                                                                                                                                                                                                  
  cxSetResourceString(@sdxHintGotoPageLast,'尾页');
  //tt8//'Last Page');                                                                                                                                                                                                    
  cxSetResourceString(@sdxHintActivePage,'当前页');
  //tt8//'Active Page');                                                                                                                                                                                                  
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxHintToolsCustomize,'自定义工具栏');
  //tt8//'Customize Toolbars');                                                                                                                                                                                 
  cxSetResourceString(@sdxHintToolsOptions,'选项');
  //tt8//'Options');                                                                                                                                                                                                      
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxHintHelpTopics,'帮助主题');
  //tt8//'Help Topics');                                                                                                                                                                                                
  cxSetResourceString(@sdxHintHelpAbout,'关于');
  //tt8//'About');                                                                                                                                                                                                           
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxPopupMenuLargeButtons,'大按钮');
  //tt8//'&Large Buttons');                                                                                                                                                                                        
  cxSetResourceString(@sdxPopupMenuFlatButtons,'平面按钮');
  //tt8//'&Flat Buttons');                                                                                                                                                                                        
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxPaperSize,'纸张大小');
  //tt8//'Paper Size:');                                                                                                                                                                                                     
  cxSetResourceString(@sdxStatus,'状态');
  //tt8//'Status:');                                                                                                                                                                                                                
  cxSetResourceString(@sdxStatusReady,'就绪');
  //tt8//'Ready');                                                                                                                                                                                                             
  cxSetResourceString(@sdxStatusPrinting,'正在打印。已完成 %d 页');
  //tt8//'Printing. Completed %d page(s)');                                                                                                                                                               
  cxSetResourceString(@sdxStatusGenerateReport,'创建报表。已完成 %d%%');
  //tt8//'Generating Report. Completed %d%%');                                                                                                                                                       
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxHintDoubleClickForChangePaperSize,'双击改变纸张大小');
  //tt8//'Double Click for Change Paper Size');                                                                                                                                              
  cxSetResourceString(@sdxHintDoubleClickForChangeMargins,'双击改变页边距');
  //tt8//'Double Click for Change Margins');                                                                                                                                                     
                                                                                                                                                                                                                                                          
  // Date&Time Formats Dialog

  cxSetResourceString(@sdxDTFormatsCaption,'日期与时间');
  //tt8//'Date and Time');                                                                                                                                                                                          
  cxSetResourceString(@sdxDTFormatsAvailableDateFormats,'有效的日期格式:');
  //tt8//'&Available Date Formats:');                                                                                                                                                             
  cxSetResourceString(@sdxDTFormatsAvailableTimeFormats,'有效的时间格式:');
  //tt8//'Available &Time Formats:');                                                                                                                                                             
  cxSetResourceString(@sdxDTFormatsAutoUpdate,'自动更新');
  //tt8//'&Update Automatically');                                                                                                                                                                                 
  cxSetResourceString(@sdxDTFormatsChangeDefaultFormat,                                                                                                                                                                                                                      
    'Do you want to change the default date and time formats to match "%s"  - "%s" ?');                                                                                                                                                                    
                                                                                                                                                                                                                                                          
  // PageNumber Formats Dialog

  cxSetResourceString(@sdxPNFormatsCaption,'页码格式');
  //tt8//'Page Number Format');                                                                                                                                                                                       
  cxSetResourceString(@sdxPageNumbering,'页码');
  //tt8//'Page Numbering');                                                                                                                                                                                                  
  cxSetResourceString(@sdxPNFormatsNumberFormat,'数字格式(&F):');
  //tt8//'Number &Format:');                                                                                                                                                                                
  cxSetResourceString(@sdxPNFormatsContinueFromPrevious,'续前节(&C)');
  //tt8//'&Continue from Previous Section');                                                                                                                                                           
  cxSetResourceString(@sdxPNFormatsStartAt,'起始页码:');
  //tt8//'Start &At:');                                                                                                                                                                                              
  cxSetResourceString(@sdxPNFormatsChangeDefaultFormat,                                                                                                                                                                                                                      
    'Do you want to change the default Page numbering format to match "%s" ?');                                                                                                                                                                            
                                                                                                                                                                                                                                                          
  // Zoom Dialog

  cxSetResourceString(@sdxZoomDlgCaption,'缩放');
  //tt8//'Zoom');                                                                                                                                                                                                           
  cxSetResourceString(@sdxZoomDlgZoomTo,' 缩放至 ');
  //tt8//' Zoom To ');                                                                                                                                                                                                   
  cxSetResourceString(@sdxZoomDlgPageWidth,'页宽(&W)');
  //tt8//'Page &Width');                                                                                                                                                                                              
  cxSetResourceString(@sdxZoomDlgWholePage,'整页(&H)');
  //tt8//'W&hole Page');                                                                                                                                                                                              
  cxSetResourceString(@sdxZoomDlgTwoPages,'两页(&T)');
  //tt8//'&Two Pages');                                                                                                                                                                                                
  cxSetResourceString(@sdxZoomDlgFourPages,'四页(&F)');
  //tt8//'&Four Pages');                                                                                                                                                                                              
  cxSetResourceString(@sdxZoomDlgManyPages,'多页(&M):');
  //tt8//'&Many Pages:');                                                                                                                                                                                            
  cxSetResourceString(@sdxZoomDlgPercent,'比例:(&E)');
  //tt8//'P&ercent:');                                                                                                                                                                                                 
  cxSetResourceString(@sdxZoomDlgPreview,'预览');
  //tt8//' Preview ');                                                                                                                                                                                                      
  cxSetResourceString(@sdxZoomDlgFontPreview,' 12pt Times New Roman ');
  //tt8//' 12pt Times New Roman ');                                                                                                                                                                   
  cxSetResourceString(@sdxZoomDlgFontPreviewString,'xypxy@163.net');
  //tt8//'AaBbCcDdEeXxYyZz');                                                                                                                                                                            
                                                                                                                                                                                                                                                          
  // Select page X x Y

  cxSetResourceString(@sdxPages,'页');
  //tt8//'Pages');                                                                                                                                                                                                                     
  cxSetResourceString(@sdxCancel,'取消');
  //tt8//'Cancel');                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                          
  // preferences dialog

  cxSetResourceString(@sdxPreferenceDlgCaption,'选项');
  //tt8//'Options');                                                                                                                                                                                                  
  cxSetResourceString(@sdxPreferenceDlgTab1,'常规(&G)');
  //tt8//'&General');                                                                                                                                                                                                
  cxSetResourceString(@sdxPreferenceDlgTab2,'');
  //tt8//'');                                                                                                                                                                                                                
  cxSetResourceString(@sdxPreferenceDlgTab3,'');
  //tt8//'');                                                                                                                                                                                                                
  cxSetResourceString(@sdxPreferenceDlgTab4,'');
  //tt8//'');                                                                                                                                                                                                                
  cxSetResourceString(@sdxPreferenceDlgTab5,'');
  //tt8//'');                                                                                                                                                                                                                
  cxSetResourceString(@sdxPreferenceDlgTab6,'');
  //tt8//'');                                                                                                                                                                                                                
  cxSetResourceString(@sdxPreferenceDlgTab7,'');
  //tt8//'');                                                                                                                                                                                                                
  cxSetResourceString(@sdxPreferenceDlgTab8,'');
  //tt8//'');                                                                                                                                                                                                                
  cxSetResourceString(@sdxPreferenceDlgTab9,'');
  //tt8//'');                                                                                                                                                                                                                
  cxSetResourceString(@sdxPreferenceDlgTab10,'');
  //tt8//'');                                                                                                                                                                                                               
  cxSetResourceString(@sdxPreferenceDlgShow,'显示(&S)');
  //tt8//' &Show ');                                                                                                                                                                                                 
  cxSetResourceString(@sdxPreferenceDlgMargins,'页边距(&M)');
  //tt8//'&Margins ');                                                                                                                                                                                          
  cxSetResourceString(@sdxPreferenceDlgMarginsHints,'页边距提示(&H)');
  //tt8//'Margins &Hints');                                                                                                                                                                            
  cxSetResourceString(@sdxPreferenceDlgMargingWhileDragging,'当拖曳时显示页边距提示(&D)');
  //tt8//'Margins Hints While &Dragging');                                                                                                                                         
  cxSetResourceString(@sdxPreferenceDlgLargeBtns,'大工具栏按钮(&L)');
  //tt8//'&Large Toolbar Buttons');                                                                                                                                                                     
  cxSetResourceString(@sdxPreferenceDlgFlatBtns,'平面工具栏按钮(&F)');
  //tt8//'&Flat Toolbar Buttons');                                                                                                                                                                     
  cxSetResourceString(@sdxPreferenceDlgMarginsColor,'页边距颜色(&C):');
  //tt8//'Margins &Color:');                                                                                                                                                                          
  cxSetResourceString(@sdxPreferenceDlgMeasurementUnits,'度量单位(&U):');
  //tt8//'Measurement &Units:');                                                                                                                                                                    
  cxSetResourceString(@sdxPreferenceDlgSaveForRunTimeToo,'保存设置(&R)');
  //tt8//'Save for &RunTime too');                                                                                                                                                                  
  cxSetResourceString(@sdxPreferenceDlgZoomScroll,'鼠标滚轮控制缩放(&Z)');
  //tt8//'&Zoom on roll with IntelliMouse');                                                                                                                                                       
  cxSetResourceString(@sdxPreferenceDlgZoomStep,'缩放比例(&P):');
  //tt8//'Zoom Ste&p:');                                                                                                                                                                                    
                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                          
  // Page Setup

  cxSetResourceString(@sdxCloneStyleCaptionPrefix,'副本 (%d) / ');
  //tt8//'Copy (%d) of ');                                                                                                                                                                                 
  cxSetResourceString(@sdxInvalideStyleCaption,'样式名称 "%s" 已经存在。 请提供另一个名称。');
  //tt8//'The style name "%s" already exists. Please supply another name.');                                                                                                   
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxPageSetupCaption,'页面设置');
  //tt8//'Page Setup');                                                                                                                                                                                               
  cxSetResourceString(@sdxStyleName,'样式名称(&N):');
  //tt8//'Style &Name:');                                                                                                                                                                                               
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxPage,'页面(&P)');
  //tt8//'&Page');                                                                                                                                                                                                                
  cxSetResourceString(@sdxMargins,'页边距(&M)');
  //tt8//'&Margins');                                                                                                                                                                                                        
  cxSetResourceString(@sdxHeaderFooter,'页眉/页脚 (&H)');
  //tt8//'&Header\Footer');                                                                                                                                                                                         
  cxSetResourceString(@sdxScaling,'比例(&S)');
  //tt8//'&Scaling');                                                                                                                                                                                                          
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxPaper,' 纸张 ');
  //tt8//' Paper ');                                                                                                                                                                                                               
  cxSetResourceString(@sdxPaperType,'纸型(&Y)');
  //tt8//'T&ype');                                                                                                                                                                                                           
  cxSetResourceString(@sdxPaperDimension,'尺寸(&S)');
  //tt8//'Dimension');                                                                                                                                                                                                  
  cxSetResourceString(@sdxPaperWidth,'宽度(&W):');
  //tt8//'&Width:');                                                                                                                                                                                                       
  cxSetResourceString(@sdxPaperHeight,'高度(&E):');
  //tt8//'H&eight:');                                                                                                                                                                                                     
  cxSetResourceString(@sdxPaperSource,'纸张来源(&U)');
  //tt8//'Paper so&urce');                                                                                                                                                                                             
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxOrientation,' 方向');
  //tt8//' Orientation ');                                                                                                                                                                                                    
  cxSetResourceString(@sdxPortrait,'纵向(&O)');
  //tt8//'P&ortrait');                                                                                                                                                                                                        
  cxSetResourceString(@sdxLandscape,'横向(&L)');
  //tt8//'&Landscape');                                                                                                                                                                                                      
  cxSetResourceString(@sdxPrintOrder,' 打印次序');
  //tt8//' Print Order ');                                                                                                                                                                                                 
  cxSetResourceString(@sdxDownThenOver,'先列后行(&D)');
  //tt8//'&Down, then over');                                                                                                                                                                                         
  cxSetResourceString(@sdxOverThenDown,'先行后列(&V)');
  //tt8//'O&ver, then down');                                                                                                                                                                                         
  cxSetResourceString(@sdxShading,' 阴影 ');
  //tt8//' Shading ');                                                                                                                                                                                                           
  cxSetResourceString(@sdxPrintUsingGrayShading,'使用灰色阴影打印(&G)');
  //tt8//'Print using &gray shading');                                                                                                                                                               
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxCenterOnPage,'居中方式');
  //tt8//'Center on page');                                                                                                                                                                                               
  cxSetResourceString(@sdxHorizontally,'水平(&Z)');
  //tt8//'Hori&zontally');                                                                                                                                                                                                
  cxSetResourceString(@sdxVertically,'垂直(&V)');
  //tt8//'&Vertically');                                                                                                                                                                                                    
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxHeader,'页眉 ');
  //tt8//'Header ');                                                                                                                                                                                                               
  cxSetResourceString(@sdxBtnHeaderFont,'字体(&F)...');
  //tt8//'&Font...');                                                                                                                                                                                                 
  cxSetResourceString(@sdxBtnHeaderBackground,'背景(&B)');
  //tt8//'&Background');                                                                                                                                                                                           
  cxSetResourceString(@sdxFooter,'页脚 ');
  //tt8//'Footer ');                                                                                                                                                                                                               
  cxSetResourceString(@sdxBtnFooterFont,'字体(&N)...');
  //tt8//'Fo&nt...');                                                                                                                                                                                                 
  cxSetResourceString(@sdxBtnFooterBackground,'背景(&G)');
  //tt8//'Back&ground');                                                                                                                                                                                           
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxTop,'上(&T):');
  //tt8//'&Top:');                                                                                                                                                                                                                  
  cxSetResourceString(@sdxLeft,'左(&L):');
  //tt8//'&Left:');                                                                                                                                                                                                                
  cxSetResourceString(@sdxRight,'右(&G):');
  //tt8//'Ri&ght:');                                                                                                                                                                                                              
  cxSetResourceString(@sdxBottom,'下(&B):');
  //tt8//'&Bottom:');                                                                                                                                                                                                            
  cxSetResourceString(@sdxHeader2,'页眉(&E):');
  //tt8//'H&eader:');                                                                                                                                                                                                         
  cxSetResourceString(@sdxFooter2,'页脚(&R):');
  //tt8//'Foote&r:');                                                                                                                                                                                                         
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxAlignment,'对齐方式');
  //tt8//'Alignment');                                                                                                                                                                                                       
  cxSetResourceString(@sdxVertAlignment,' 垂直对齐');
  //tt8//' Vertical Alignment ');                                                                                                                                                                                       
  cxSetResourceString(@sdxReverseOnEvenPages,'偶页相反(&R)');
  //tt8//'&Reverse on even pages');                                                                                                                                                                             
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxAdjustTo,'调整到:');
  //tt8//'&Adjust To:');                                                                                                                                                                                                       
  cxSetResourceString(@sdxFitTo,'适合:');
  //tt8//'&Fit To:');                                                                                                                                                                                                               
  cxSetResourceString(@sdxPercentOfNormalSize,'% 正常大小');
  //tt8//'% normal size');                                                                                                                                                                                       
  cxSetResourceString(@sdxPagesWideBy,'页宽(&W)');
  //tt8//'page(s) &wide by');                                                                                                                                                                                              
  cxSetResourceString(@sdxTall,'页高(&T)');
  //tt8//'&tall');                                                                                                                                                                                                                
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxOf,'共');
  //tt8//'Of');                                                                                                                                                                                                                           
  cxSetResourceString(@sdxLastPrinted,'上次打印时间 ');
  //tt8//'Last Printed ');                                                                                                                                                                                            
  cxSetResourceString(@sdxFileName,'文件名称 ');
  //tt8//'Filename ');                                                                                                                                                                                                       
  cxSetResourceString(@sdxFileNameAndPath,'文件名称和路径 ');
  //tt8//'Filename and path ');                                                                                                                                                                                 
  cxSetResourceString(@sdxPrintedBy,'打印由 ');
  //tt8//'Printed By ');                                                                                                                                                                                                      
  cxSetResourceString(@sdxPrintedOn,'打印在 ');
  //tt8//'Printed On ');                                                                                                                                                                                                      
  cxSetResourceString(@sdxCreatedBy,'创建由 ');
  //tt8//'Created By ');                                                                                                                                                                                                      
  cxSetResourceString(@sdxCreatedOn,'创建在 ');
  //tt8//'Created On ');                                                                                                                                                                                                      
  cxSetResourceString(@sdxConfidential,'机密');
  //tt8//'Confidential'); 

  // HF function

  cxSetResourceString(@sdxHFFunctionNameUnknown,'Unknown');
  cxSetResourceString(@sdxHFFunctionNamePageNumber,'Page Number');
  cxSetResourceString(@sdxHFFunctionNameTotalPages,'Total Pages');
  cxSetResourceString(@sdxHFFunctionNamePageOfPages,'Page # of Pages #');
  cxSetResourceString(@sdxHFFunctionNameDateTime,'Date and Time');
  cxSetResourceString(@sdxHFFunctionNameDate,'Date');
  cxSetResourceString(@sdxHFFunctionNameTime,'Time');
  cxSetResourceString(@sdxHFFunctionNameUserName,'User Name');
  cxSetResourceString(@sdxHFFunctionNameMachineName,'Machine Name');

  cxSetResourceString(@sdxHFFunctionHintPageNumber,'Page Number');
  cxSetResourceString(@sdxHFFunctionHintTotalPages,'Total Pages');
  cxSetResourceString(@sdxHFFunctionHintPageOfPages,'Page # of Pages #');
  cxSetResourceString(@sdxHFFunctionHintDateTime,'Date and Time Printed');
  cxSetResourceString(@sdxHFFunctionHintDate,'Date Printed');
  cxSetResourceString(@sdxHFFunctionHintTime,'Time Printed');
  cxSetResourceString(@sdxHFFunctionHintUserName,'User Name');
  cxSetResourceString(@sdxHFFunctionHintMachineName,'Machine Name');

  cxSetResourceString(@sdxHFFunctionTemplatePageNumber,'Page #');
  cxSetResourceString(@sdxHFFunctionTemplateTotalPages,'Total Pages');
  cxSetResourceString(@sdxHFFunctionTemplatePageOfPages,'Page # of Pages #');
  cxSetResourceString(@sdxHFFunctionTemplateDateTime,'Date & Time Printed');
  cxSetResourceString(@sdxHFFunctionTemplateDate,'Date Printed');
  cxSetResourceString(@sdxHFFunctionTemplateTime,'Time Printed');
  cxSetResourceString(@sdxHFFunctionTemplateUserName,'User Name');
  cxSetResourceString(@sdxHFFunctionTemplateMachineName,'Machine Name');

  // Designer strings

  // Months

  cxSetResourceString(@sdxJanuary,'一月');
  //tt8//'January');                                                                                                                                                                                                               
  cxSetResourceString(@sdxFebruary,'二月');
  //tt8//'February');                                                                                                                                                                                                             
  cxSetResourceString(@sdxMarch,'三月');
  //tt8//'March');                                                                                                                                                                                                                   
  cxSetResourceString(@sdxApril,'四月');
  //tt8//'April');                                                                                                                                                                                                                   
  cxSetResourceString(@sdxMay,'五月');
  //tt8//'May');                                                                                                                                                                                                                       
  cxSetResourceString(@sdxJune,'六月');
  //tt8//'June');                                                                                                                                                                                                                     
  cxSetResourceString(@sdxJuly,'七月');
  //tt8//'July');                                                                                                                                                                                                                     
  cxSetResourceString(@sdxAugust,'八月');
  //tt8//'August');                                                                                                                                                                                                                 
  cxSetResourceString(@sdxSeptember,'九月');
  //tt8//'September');                                                                                                                                                                                                           
  cxSetResourceString(@sdxOctober,'十月');
  //tt8//'October');                                                                                                                                                                                                               
  cxSetResourceString(@sdxNovember,'十一月');
  //tt8//'November');                                                                                                                                                                                                           
  cxSetResourceString(@sdxDecember,'十二月');
  //tt8//'December');                                                                                                                                                                                                           
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxEast,'东方');
  //tt8//'East');                                                                                                                                                                                                                     
  cxSetResourceString(@sdxWest,'西方');
  //tt8//'West');                                                                                                                                                                                                                     
  cxSetResourceString(@sdxSouth,'南方');
  //tt8//'South');                                                                                                                                                                                                                   
  cxSetResourceString(@sdxNorth,'北方');
  //tt8//'North');                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxTotal,'合计');
  //tt8//'Total');                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                          
  // dxFlowChart

  cxSetResourceString(@sdxPlan,'设计图');
  //tt8//'Plan');                                                                                                                                                                                                                   
  cxSetResourceString(@sdxSwimmingPool,'游泳池');
  //tt8//'Swimming-pool');                                                                                                                                                                                                  
  cxSetResourceString(@sdxAdministration,'管理员');
  //tt8//'Administration');                                                                                                                                                                                               
  cxSetResourceString(@sdxPark,'公园');
  //tt8//'Park');                                                                                                                                                                                                                     
  cxSetResourceString(@sdxCarParking,'停车场');
  //tt8//'Car-Parking');                                                                                                                                                                                                      
                                                                                                                                                                                                                                                          
  // dxOrgChart

  cxSetResourceString(@sdxCorporateHeadquarters,'公司'+#13#10+'总部');
  //tt8//'Corporate' + #13#10 + 'Headquarters');                                                                                                                                                       
  cxSetResourceString(@sdxSalesAndMarketing,'销售部'+#13#10+'市场部');
  //tt8//'Sales and' + #13#10 + 'Marketing');                                                                                                                                                          
  cxSetResourceString(@sdxEngineering,'工程技术部');
  //tt8//'Engineering');                                                                                                                                                                                                 
  cxSetResourceString(@sdxFieldOfficeCanada,'办公室:'+#13#10+'加拿大');
  //tt8//'Field Office:' + #13#10 + 'Canada');                                                                                                                                                        
                                                                                                                                                                                                                                                          
  // dxMasterView

  cxSetResourceString(@sdxOrderNoCaption,'序号');
  //tt8//'OrderNo');                                                                                                                                                                                                        
  cxSetResourceString(@sdxNameCaption,'名称');
  //tt8//'Name');                                                                                                                                                                                                              
  cxSetResourceString(@sdxCountCaption,'数量');
  //tt8//'Count');                                                                                                                                                                                                            
  cxSetResourceString(@sdxCompanyCaption,'公司');
  //tt8//'Company');                                                                                                                                                                                                        
  cxSetResourceString(@sdxAddressCaption,'地址');
  //tt8//'Address');                                                                                                                                                                                                        
  cxSetResourceString(@sdxPriceCaption,'价格');
  //tt8//'Price');                                                                                                                                                                                                            
  cxSetResourceString(@sdxCashCaption,'现金');
  //tt8//'Cash');                                                                                                                                                                                                              
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxName1,'张三');
  //tt8//'Jennie Valentine');                                                                                                                                                                                                        
  cxSetResourceString(@sdxName2,'李四');
  //tt8//'Sam Hill');                                                                                                                                                                                                                
  cxSetResourceString(@sdxCompany1,'宇宙有限公司');
  //tt8//'Jennie Inc.');                                                                                                                                                                                                  
  cxSetResourceString(@sdxCompany2,'地球集团');
  //tt8//'Daimler-Chrysler AG');                                                                                                                                                                                              
  cxSetResourceString(@sdxAddress1,'123 Home Lane');
  //tt8//'123 Home Lane');                                                                                                                                                                                               
  cxSetResourceString(@sdxAddress2,'9333 Holmes Dr.');
  //tt8//'9333 Holmes Dr.');                                                                                                                                                                                           
                                                                                                                                                                                                                                                          
  // dxTreeList

  cxSetResourceString(@sdxCountIs,'数量：%d');
  //tt8//'Count is: %d');                                                                                                                                                                                                      
  cxSetResourceString(@sdxRegular,'常规');
  //tt8//'Regular');                                                                                                                                                                                                               
  cxSetResourceString(@sdxIrregular,'不规则');
  //tt8//'Irregular');                                                                                                                                                                                                         
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxTLBand,'项目数据');
  //tt8//'Item Data');                                                                                                                                                                                                          
  cxSetResourceString(@sdxTLColumnName,'名称');
  //tt8//'Name');                                                                                                                                                                                                             
  cxSetResourceString(@sdxTLColumnAxisymmetric,'轴对称');
  //tt8//'Axisymmetric');                                                                                                                                                                                           
  cxSetResourceString(@sdxTLColumnItemShape,'形状');
  //tt8//'Shape');                                                                                                                                                                                                       
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxItemShapeAsText,'(图形)');
  //tt8//'(Graphic)');                                                                                                                                                                                                   
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxItem1Name,'锥形物');
  //tt8//'Cylinder');                                                                                                                                                                                                          
  cxSetResourceString(@sdxItem2Name,'圆柱体');
  //tt8//'Cone');                                                                                                                                                                                                              
  cxSetResourceString(@sdxItem3Name,'棱锥');
  //tt8//'Pyramid');                                                                                                                                                                                                             
  cxSetResourceString(@sdxItem4Name,'盒子');
  //tt8//'Box');                                                                                                                                                                                                                 
  cxSetResourceString(@sdxItem5Name,'自由表面');
  //tt8//'Free Surface');                                                                                                                                                                                                    
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxItem1Description,'');
  //tt8//'');                                                                                                                                                                                                                 
  cxSetResourceString(@sdxItem2Description,'轴对称几何图形');
  //tt8//'Axisymmetric geometry figure');                                                                                                                                                                       
  cxSetResourceString(@sdxItem3Description,'轴对称几何图形');
  //tt8//'Axisymmetric geometry figure');                                                                                                                                                                       
  cxSetResourceString(@sdxItem4Description,'锐角几何图形');
  //tt8//'Acute-angled geometry figure');                                                                                                                                                                         
  cxSetResourceString(@sdxItem5Description,'');
  //tt8//'');                                                                                                                                                                                                                 
  cxSetResourceString(@sdxItem6Description,'');
  //tt8//'');                                                                                                                                                                                                                 
  cxSetResourceString(@sdxItem7Description,'简单突出表面');
  //tt8//'Simple extrusion surface');                                                                                                                                                                             
                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                          
  // PS 2.3

  // Patterns common

  cxSetResourceString(@sdxPatternIsNotRegistered,'模式 "%s" 没有注册');
  //tt8//'Pattern "%s" is not registered');

  // Excel edge patterns

  cxSetResourceString(@sdxSolidEdgePattern,'实线');
  //tt8//'Solid');                                                                                                                                                                                                        
  cxSetResourceString(@sdxThinSolidEdgePattern,'细实线');
  //tt8//'Medium Solid');                                                                                                                                                                                           
  cxSetResourceString(@sdxMediumSolidEdgePattern,'中实线');
  //tt8//'Medium Solid');                                                                                                                                                                                         
  cxSetResourceString(@sdxThickSolidEdgePattern,'粗实线');
  //tt8//'Thick Solid');                                                                                                                                                                                           
  cxSetResourceString(@sdxDottedEdgePattern,'圆点');
  //tt8//'Dotted');                                                                                                                                                                                                      
  cxSetResourceString(@sdxDashedEdgePattern,'短画线');
  //tt8//'Dashed');                                                                                                                                                                                                    
  cxSetResourceString(@sdxDashDotDotEdgePattern,'短画线-点-点');
  //tt8//'Dash Dot Dot');                                                                                                                                                                                    
  cxSetResourceString(@sdxDashDotEdgePattern,'短画线-点');
  //tt8//'Dash Dot');                                                                                                                                                                                              
  cxSetResourceString(@sdxSlantedDashDotEdgePattern,'斜短画线-点');
  //tt8//'Slanted Dash Dot');                                                                                                                                                                             
  cxSetResourceString(@sdxMediumDashDotDotEdgePattern,'中等短画线-点-点');
  //tt8//'Medium Dash Dot Dot');                                                                                                                                                                   
  cxSetResourceString(@sdxHairEdgePattern,'丝状');
  //tt8//'Hair');                                                                                                                                                                                                          
  cxSetResourceString(@sdxMediumDashDotEdgePattern,'中等短画线-点');
  //tt8//'Medium Dash Dot');                                                                                                                                                                             
  cxSetResourceString(@sdxMediumDashedEdgePattern,'中等短画线');
  //tt8//'Medium Dashed');                                                                                                                                                                                   
  cxSetResourceString(@sdxDoubleLineEdgePattern,'双线');
  //tt8//'Double Line');                                                                                                                                                                                             
                                                                                                                                                                                                                                                          
  // Excel fill patterns names

  cxSetResourceString(@sdxSolidFillPattern,'原色');
  //tt8//'Solid');                                                                                                                                                                                                        
  cxSetResourceString(@sdxGray75FillPattern,'75% 灰色');
  //tt8//'75% Gray');                                                                                                                                                                                                
  cxSetResourceString(@sdxGray50FillPattern,'50% 灰色');
  //tt8//'50% Gray');                                                                                                                                                                                                
  cxSetResourceString(@sdxGray25FillPattern,'25% 灰色');
  //tt8//'25% Gray');                                                                                                                                                                                                
  cxSetResourceString(@sdxGray125FillPattern,'12.5% 灰色');
  //tt8//'12.5% Gray');                                                                                                                                                                                           
  cxSetResourceString(@sdxGray625FillPattern,'6.25% 灰色');
  //tt8//'6.25% Gray');                                                                                                                                                                                           
  cxSetResourceString(@sdxHorizontalStripeFillPattern,'水平条纹');
  //tt8//'Horizontal Stripe');                                                                                                                                                                             
  cxSetResourceString(@sdxVerticalStripeFillPattern,'垂直条纹');
  //tt8//'Vertical Stripe');                                                                                                                                                                                 
  cxSetResourceString(@sdxReverseDiagonalStripeFillPattern,'逆对角线条纹');
  //tt8//'Reverse Diagonal Stripe');                                                                                                                                                              
  cxSetResourceString(@sdxDiagonalStripeFillPattern,'对角线条纹');
  //tt8//'Diagonal Stripe');                                                                                                                                                                               
  cxSetResourceString(@sdxDiagonalCrossHatchFillPattern,'对角线剖面线');
  //tt8//'Diagonal Cross Hatch');                                                                                                                                                                    
  cxSetResourceString(@sdxThickCrossHatchFillPattern,'粗对角线剖面线');
  //tt8//'Thick Cross Hatch');                                                                                                                                                                        
  cxSetResourceString(@sdxThinHorizontalStripeFillPattern,'细水平条纹');
  //tt8//'Thin Horizontal Stripe');                                                                                                                                                                  
  cxSetResourceString(@sdxThinVerticalStripeFillPattern,'细垂直条纹');
  //tt8//'Thin Vertical Stripe');                                                                                                                                                                      
  cxSetResourceString(@sdxThinReverseDiagonalStripeFillPattern,'Thin Reverse Diagonal Stripe');                                                                                                                                                                               
  cxSetResourceString(@sdxThinDiagonalStripeFillPattern,'细对角线条纹');
  //tt8//'Thin Diagonal Stripe');                                                                                                                                                                    
  cxSetResourceString(@sdxThinHorizontalCrossHatchFillPattern,'细水平剖面线');
  //tt8//'Thin Horizontal Cross Hatch');                                                                                                                                                       
  cxSetResourceString(@sdxThinDiagonalCrossHatchFillPattern,'细对角线剖面线');
  //tt8//'Thin Diagonal Cross Hatch');                                                                                                                                                         
                                                                                                                                                                                                                                                          
  // cxSpreadSheet

  cxSetResourceString(@sdxShowRowAndColumnHeadings,'行和列标题(&R)');
  //tt8//'&Row and Column Headings');                                                                                                                                                                   
  cxSetResourceString(@sdxShowGridLines,'网格行');
  //tt8//'GridLines');                                                                                                                                                                                                     
  cxSetResourceString(@sdxSuppressSourceFormats,'禁止源格式(&S)');
  //tt8//'&Suppress Source Formats');                                                                                                                                                                      
  cxSetResourceString(@sdxRepeatHeaderRowAtTop,'在顶端重复标题行');
  //tt8//'Repeat Header Row at Top');                                                                                                                                                                     
  cxSetResourceString(@sdxDataToPrintDoesNotExist,                                                                                                                                                                                                                           
    'Cannot activate ReportLink because PrintingSystem did not find anything to print.');                                                                                                                                                                  
                                                                                                                                                                                                                                                          
  // Designer strings

  // Short names of month

  cxSetResourceString(@sdxJanuaryShort,'一月');
  //tt8//'Jan');                                                                                                                                                                                                              
  cxSetResourceString(@sdxFebruaryShort,'二月');
  //tt8//'Feb');                                                                                                                                                                                                             
  cxSetResourceString(@sdxMarchShort,'三月');
  //tt8//'March');                                                                                                                                                                                                              
  cxSetResourceString(@sdxAprilShort,'四月');
  //tt8//'April');                                                                                                                                                                                                              
  cxSetResourceString(@sdxMayShort,'五月');
  //tt8//'May');                                                                                                                                                                                                                  
  cxSetResourceString(@sdxJuneShort,'六月');
  //tt8//'June');                                                                                                                                                                                                                
  cxSetResourceString(@sdxJulyShort,'七月');
  //tt8//'July');                                                                                                                                                                                                                
  cxSetResourceString(@sdxAugustShort,'八月');
  //tt8//'Aug');                                                                                                                                                                                                               
  cxSetResourceString(@sdxSeptemberShort,'九月');
  //tt8//'Sept');                                                                                                                                                                                                           
  cxSetResourceString(@sdxOctoberShort,'十月');
  //tt8//'Oct');                                                                                                                                                                                                              
  cxSetResourceString(@sdxNovemberShort,'十一月');
  //tt8//'Nov');                                                                                                                                                                                                           
  cxSetResourceString(@sdxDecemberShort,'十二月');
  //tt8//'Dec');                                                                                                                                                                                                           
                                                                                                                                                                                                                                                          
  // TreeView

  cxSetResourceString(@sdxTechnicalDepartment,'技术部门');
  //tt8//'Technical Department');                                                                                                                                                                                  
  cxSetResourceString(@sdxSoftwareDepartment,'软件部门');
  //tt8//'Software Department');                                                                                                                                                                                    
  cxSetResourceString(@sdxSystemProgrammers,'系统程序员');
  //tt8//'Core Developers');                                                                                                                                                                                       
  cxSetResourceString(@sdxEndUserProgrammers,'终端用户程序员');
  //tt8//'GUI Developers');                                                                                                                                                                                   
  cxSetResourceString(@sdxBetaTesters,'测试员');
  //tt8//'Beta Testers');                                                                                                                                                                                                    
  cxSetResourceString(@sdxHumanResourceDepartment,'人力资源部门');
  //tt8//'Human Resource Department');                                                                                                                                                                     
                                                                                                                                                                                                                                                          
  // misc.
  cxSetResourceString(@sdxTreeLines,'树线');
  //tt8//'&TreeLines');                                                                                                                                                                                                          
  cxSetResourceString(@sdxTreeLinesColor,'树线颜色:');
  //tt8//'T&ree Line Color:');                                                                                                                                                                                         
  cxSetResourceString(@sdxExpandButtons,'展开按钮');
  //tt8//'E&xpand Buttons');                                                                                                                                                                                             
  cxSetResourceString(@sdxCheckMarks,'检查标记');
  //tt8//'Check Marks');                                                                                                                                                                                                    
  cxSetResourceString(@sdxTreeEffects,'树效果');
  //tt8//'Tree Effects');                                                                                                                                                                                                    
  cxSetResourceString(@sdxAppearance,'外观');
  //tt8//'Appearance');                                                                                                                                                                                                         
                                                                                                                                                                                                                                                          
  // Designer previews

  // Localize if you want (they are used inside FormatReport dialog -> ReportPreview)

  cxSetResourceString(@sdxCarLevelCaption,'汽车');
  //tt8//'Cars');                                                                                                                                                                                                          
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxManufacturerBandCaption,'Manufacturer Data');                                                                                                                                                                                                       
  cxSetResourceString(@sdxModelBandCaption,'汽车数据');
  //tt8//'Car Data');                                                                                                                                                                                                 
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxManufacturerNameColumnCaption,'Name');                                                                                                                                                                                                              
  cxSetResourceString(@sdxManufacturerLogoColumnCaption,'Logo');                                                                                                                                                                                                              
  cxSetResourceString(@sdxManufacturerCountryColumnCaption,'Country');                                                                                                                                                                                                        
  cxSetResourceString(@sdxCarModelColumnCaption,'模型');
  //tt8//'Model');                                                                                                                                                                                                   
  cxSetResourceString(@sdxCarIsSUVColumnCaption,'SUV');
  //tt8//'SUV');                                                                                                                                                                                                      
  cxSetResourceString(@sdxCarPhotoColumnCaption,'照片');
  //tt8//'Photo');                                                                                                                                                                                                   
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxCarManufacturerName1,'BMW');                                                                                                                                                                                                                        
  cxSetResourceString(@sdxCarManufacturerName2,'Ford');                                                                                                                                                                                                                       
  cxSetResourceString(@sdxCarManufacturerName3,'Audi');                                                                                                                                                                                                                       
  cxSetResourceString(@sdxCarManufacturerName4,'Land Rover');                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxCarManufacturerCountry1,'Germany');                                                                                                                                                                                                                 
  cxSetResourceString(@sdxCarManufacturerCountry2,'United States');                                                                                                                                                                                                           
  cxSetResourceString(@sdxCarManufacturerCountry3,'Germany');                                                                                                                                                                                                                 
  cxSetResourceString(@sdxCarManufacturerCountry4,'United Kingdom');                                                                                                                                                                                                          
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxCarModel1,'X5 4WD');
  //tt8//'X5 4.6is');                                                                                                                                                                                                          
  cxSetResourceString(@sdxCarModel2,'旅行');
  //tt8//'Excursion');                                                                                                                                                                                                           
  cxSetResourceString(@sdxCarModel3,'S8 Quattro');
  //tt8//'S8 Quattro');                                                                                                                                                                                                    
  cxSetResourceString(@sdxCarModel4,'G4 挑战');
  //tt8//'G4 Challenge');                                                                                                                                                                                                     
                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxTrue,'真');
  //tt8//'True');                                                                                                                                                                                                                       
  cxSetResourceString(@sdxFalse,'假');
  //tt8//'False');                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                          
  // PS 2.4

  // dxPrnDev.pas

  cxSetResourceString(@sdxAuto,'自动');
  //tt8//'Auto');
  cxSetResourceString(@sdxCustom,'常规');
  //tt8//'Custom');
  cxSetResourceString(@sdxEnv,'Env');
  //tt8//'Env');

  // Grid 4
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxLookAndFeelFlat,'平面');
  //tt8//'Flat');                                                                                                                                                                                                          
  cxSetResourceString(@sdxLookAndFeelStandard,'标准');
  //tt8//'Standard');                                                                                                                                                                                                  
  cxSetResourceString(@sdxLookAndFeelUltraFlat,'超平面');
  //tt8//'UltraFlat');                                                                                                                                                                                              
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxViewTab,'视图');
  //tt8//'View');                                                                                                                                                                                                                  
  cxSetResourceString(@sdxBehaviorsTab,'性能');
  //tt8//'Behaviors');                                                                                                                                                                                                        
  cxSetResourceString(@sdxPreviewTab,'预览');
  //tt8//'Preview');                                                                                                                                                                                                            
  cxSetResourceString(@sdxCardsTab,'卡片');
  //tt8//'Cards');                                                                                                                                                                                                                
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxFormatting,'格式');
  //tt8//'Formatting');                                                                                                                                                                                                         
  cxSetResourceString(@sdxLookAndFeel,'外观');
  //tt8//'Look and Feel');                                                                                                                                                                                                     
  cxSetResourceString(@sdxLevelCaption,'标题');
  //tt8//'&Caption');                                                                                                                                                                                                         
  cxSetResourceString(@sdxFilterBar,'过滤器状态条');
  //tt8//'&Filter Bar');                                                                                                                                                                                                 
  cxSetResourceString(@sdxRefinements,'修正');
  //tt8//'Refinements');                                                                                                                                                                                                       
  cxSetResourceString(@sdxProcessSelection,'处理选择(&S)');
  //tt8//'Process &Selection');                                                                                                                                                                                   
  cxSetResourceString(@sdxProcessExactSelection,'处理精确选择(&X)');
  //tt8//'Process E&xact Selection');                                                                                                                                                                    
  cxSetResourceString(@sdxExpanding,'扩充');
  //tt8//'Expanding');                                                                                                                                                                                                           
  cxSetResourceString(@sdxGroups,'组(&G)');
  //tt8//'&Groups');                                                                                                                                                                                                              
  cxSetResourceString(@sdxDetails,'细节(&D)');
  //tt8//'&Details');                                                                                                                                                                                                          
  cxSetResourceString(@sdxStartFromActiveDetails,'从当前细节开始');
  //tt8//'Start from Active Details');                                                                                                                                                                    
  cxSetResourceString(@sdxOnlyActiveDetails,'仅含当前细节');
  //tt8//'Only Active Details');                                                                                                                                                                                 
  cxSetResourceString(@sdxVisible,'可见(&V)');
  //tt8//'&Visible');                                                                                                                                                                                                          
  cxSetResourceString(@sdxPreviewAutoHeight,'自动高度(&U)');
  //tt8//'A&uto Height');                                                                                                                                                                                        
  cxSetResourceString(@sdxPreviewMaxLineCount,'最大行计算(&M)： ');
  //tt8//'&Max Line Count: ');                                                                                                                                                                            
  cxSetResourceString(@sdxSizes,'大小');
  //tt8//'Sizes');                                                                                                                                                                                                                   
  cxSetResourceString(@sdxKeepSameWidth,'保持同样宽度(&K)');
  //tt8//'&Keep Same Width');                                                                                                                                                                                    
  cxSetResourceString(@sdxKeepSameHeight,'保持同样高度(&H)');
  //tt8//'Keep Same &Height');                                                                                                                                                                                  
  cxSetResourceString(@sdxFraming,'框架');
  //tt8//'Framing');                                                                                                                                                                                                               
  cxSetResourceString(@sdxSpacing,'间距');
  //tt8//'Spacing');                                                                                                                                                                                                               
  cxSetResourceString(@sdxShadow,'阴影');
  //tt8//'Shadow');                                                                                                                                                                                                                 
  cxSetResourceString(@sdxDepth,'浓度(&D):');
  //tt8//'&Depth:');                                                                                                                                                                                                            
  cxSetResourceString(@sdxPosition,'位置(&P)');
  //tt8//'&Position');                                                                                                                                                                                                        
  cxSetResourceString(@sdxPositioning,'位置');
  //tt8//'Positioning');                                                                                                                                                                                                       
  cxSetResourceString(@sdxHorizontal,'水平(&O):');
  //tt8//'H&orizontal:');                                                                                                                                                                                                  
  cxSetResourceString(@sdxVertical,'垂直(&E):');
  //tt8//'V&ertical:');                                                                                                                                                                                                      
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxSummaryFormat,'计数,0');
  //tt8//'Count,0');                                                                                                                                                                                                   
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxCannotUseOnEveryPageMode,'不能使用在每页方式'+#13#10+
  //tt8//'Cannot Use OnEveryPage Mode'+ #13#10 +                                                                                                                                            
    #13#10 +                                                                                                                                                                                                                                              
    'You should or(and)' + #13#10 +                                                                                                                                                                                                                       
    '  - Collapse all Master Records' + #13#10 +                                                                                                                                                                                                          
    '  - Toggle "Unwrap" Option off on "Behaviors" Tab');                                                                                                                                                                                                  
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxIncorrectBandHeadersState,'不能使用带区报头在每一页方式'+#13#10+
  //tt8//'Cannot Use BandHeaders OnEveryPage Mode' + #13#10 +                                                                                                                    
    #13#10 +                                                                                                                                                                                                                                              
    'You should either:' + #13#10 +                                                                                                                                                                                                                       
    '  - Set Caption OnEveryPage Option On' + #13#10 +                                                                                                                                                                                                    
    '  - Set Caption Visible Option Off');                                                                                                                                                                                                                 
  cxSetResourceString(@sdxIncorrectHeadersState,'不能使用报头在每一页方式'+#13#10+
  //tt8//'Cannot Use Headers OnEveryPage Mode' + #13#10 +                                                                                                                                
    #13#10 +                                                                                                                                                                                                                                              
    'You should either:' + #13#10 +                                                                                                                                                                                                                       
    '  - Set Caption and Band OnEveryPage Option On' + #13#10 +                                                                                                                                                                                           
    '  - Set Caption and Band Visible Option Off');                                                                                                                                                                                                        
  cxSetResourceString(@sdxIncorrectFootersState,'不能使用页脚在每一页方式'+#13#10+
  //tt8//'Cannot Use Footers OnEveryPage Mode' + #13#10 +                                                                                                                                
    #13#10 +                                                                                                                                                                                                                                              
    'You should either:' + #13#10 +                                                                                                                                                                                                                       
    '  - Set FilterBar OnEveryPage Option On' + #13#10 +                                                                                                                                                                                                  
    '  - Set FilterBar Visible Option Off');

  cxSetResourceString(@sdxCharts,'图表');
  //sdl//'Charts'
                                                                                                                                                                                                                                                          
  // PS 3

  cxSetResourceString(@sdxTPicture,'TPicture');
  cxSetResourceString(@sdxCopy,'&Copy');
  cxSetResourceString(@sdxSave,'&Save...');
  cxSetResourceString(@sdxBaseStyle,'Base Style');

  cxSetResourceString(@sdxComponentAlreadyExists,'Component named "%s" already exists');
  cxSetResourceString(@sdxInvalidComponentName,'"%s" is not a valid component name');

  // shapes
 
  cxSetResourceString(@sdxRectangle,'Rectangle');
  cxSetResourceString(@sdxSquare,'Square');
  cxSetResourceString(@sdxEllipse,'Ellipse');
  cxSetResourceString(@sdxCircle,'Circle');
  cxSetResourceString(@sdxRoundRect,'RoundRect');
  cxSetResourceString(@sdxRoundSquare,'RoundSquare');

  // standard pattern names

  cxSetResourceString(@sdxHorizontalFillPattern,'Horizontal');
  cxSetResourceString(@sdxVerticalFillPattern,'Vertical');
  cxSetResourceString(@sdxFDiagonalFillPattern,'FDiagonal');
  cxSetResourceString(@sdxBDiagonalFillPattern,'BDiagonal');
  cxSetResourceString(@sdxCrossFillPattern,'Cross');
  cxSetResourceString(@sdxDiagCrossFillPattern,'DiagCros');

  // explorer
                                                             
  cxSetResourceString(@sdxCyclicIDReferences,'Cyclic ID references %s and %s');
  cxSetResourceString(@sdxLoadReportDataToFileTitle,'Load Report');
  cxSetResourceString(@sdxSaveReportDataToFileTitle,'Save Report As');
  cxSetResourceString(@sdxInvalidExternalStorage,'Invalid External Storage');
  cxSetResourceString(@sdxLinkIsNotIncludedInUsesClause,
    'ReportFile contains ReportLink "%0:s"' + #13#10 + 
    'Unit with declaration of "%0:s" must be included in uses clause');
  cxSetResourceString(@sdxInvalidStorageVersion,'Invalid Storage Verison: %d');
  cxSetResourceString(@sdxPSReportFiles,'Report Files');
  cxSetResourceString(@sdxReportFileLoadError,
    'Cannot load Report File "%s".' + #13#10 + 
    'File is corrupted or is locked by another User or Application.' + #13#10 + 
    #13#10 +
    'Original Report will be restored.');
  
  cxSetResourceString(@sdxNone,'(None)');
  cxSetResourceString(@sdxReportDocumentIsCorrupted,'(File is not a ReportDocument or Corrupted)');
  
  cxSetResourceString(@sdxCloseExplorerHint,'Close Explorer');
  cxSetResourceString(@sdxExplorerCaption,'Explorer');
  cxSetResourceString(@sdxExplorerRootFolderCaption,'Root');
  cxSetResourceString(@sdxNewExplorerFolderItem,'New Folder');
  cxSetResourceString(@sdxCopyOfItem,'Copy of ');
  cxSetResourceString(@sdxReportExplorer,'Report Explorer');
                                
  cxSetResourceString(@sdxDataLoadErrorText,'Cannot load Report Data');
  cxSetResourceString(@sdxDBBasedExplorerItemDataLoadError,
    'Cannot load Report Data.' + #13#10 + 
    'Data are corrupted or are locked');
  cxSetResourceString(@sdxFileBasedExplorerItemDataLoadError,
    'Cannot load Report Data.' + #13#10 + 
    'File is corruted or is locked by another User or Application');
  cxSetResourceString(@sdxDeleteNonEmptyFolderMessageText,'Folder "%s" is not Empty. Delete anyway?');
  cxSetResourceString(@sdxDeleteFolderMessageText,'Delete Folder "%s" ?');
  cxSetResourceString(@sdxDeleteItemMessageText,'Delete Item "%s" ?');
  cxSetResourceString(@sdxCannotRenameFolderText,'Cannot rename folder "%s". A folder with name "%s" already exists. Specify a different name.');
  cxSetResourceString(@sdxCannotRenameItemText,'Cannot rename item "%s". An item with name "%s" already exists. Specify a different name.');
  cxSetResourceString(@sdxOverwriteFolderMessageText,
    'This folder "%s" already contains folder named "%s".' + #13#10 + 
    #13#10 + 
    'If the items in existing folder have the same name as items in the' + #13#10 + 
    'folder you are moving or copying, they will be replaced. Do you still?' + #13#10 +
    'want to move or copy the folder?');
  cxSetResourceString(@sdxOverwriteItemMessageText,
    'This Folder "%s" already contains item named "%s".' + #13#10 + 
    #13#10 + 
    'Would you like to overwrite existing item?');
  cxSetResourceString(@sdxSelectNewRoot,'Select new Root Directory where the Reports will be stored');
  cxSetResourceString(@sdxInvalidFolderName,'Invalid Folder Name "%s"');
  cxSetResourceString(@sdxInvalidReportName,'Invalid Report Name "%s"');
  
  cxSetResourceString(@sdxExplorerBar,'Explorer');

  cxSetResourceString(@sdxMenuFileSave,'&Save');
  cxSetResourceString(@sdxMenuFileSaveAs,'Save &As...');
  cxSetResourceString(@sdxMenuFileLoad,'&Load');
  cxSetResourceString(@sdxMenuFileClose,'U&nload');
  cxSetResourceString(@sdxHintFileSave,'Save Report');
  cxSetResourceString(@sdxHintFileSaveAs,'Save Report As');
  cxSetResourceString(@sdxHintFileLoad,'Load Report');
  cxSetResourceString(@sdxHintFileClose,'Unload Report');
  
  cxSetResourceString(@sdxMenuExplorer,'E&xplorer');
  cxSetResourceString(@sdxMenuExplorerCreateFolder,'Create &Folder');
  cxSetResourceString(@sdxMenuExplorerDelete,'&Delete...');
  cxSetResourceString(@sdxMenuExplorerRename,'Rena&me');
  cxSetResourceString(@sdxMenuExplorerProperties,'&Properties...');
  cxSetResourceString(@sdxMenuExplorerRefresh,'Refresh');
  cxSetResourceString(@sdxMenuExplorerChangeRootPath,'Change Root...');
  cxSetResourceString(@sdxMenuExplorerSetAsRoot,'Set As Root');
  cxSetResourceString(@sdxMenuExplorerGoToUpOneLevel,'Up One Level');

  cxSetResourceString(@sdxHintExplorerCreateFolder,'Create New Folder');
  cxSetResourceString(@sdxHintExplorerDelete,'Delete');
  cxSetResourceString(@sdxHintExplorerRename,'Rename');
  cxSetResourceString(@sdxHintExplorerProperties,'Properties');
  cxSetResourceString(@sdxHintExplorerRefresh,'Refresh');
  cxSetResourceString(@sdxHintExplorerChangeRootPath,'Change Root');
  cxSetResourceString(@sdxHintExplorerSetAsRoot,'Set Current Folder as Root');
  cxSetResourceString(@sdxHintExplorerGoToUpOneLevel,'Up One Level');
  
  cxSetResourceString(@sdxMenuViewExplorer,'E&xplorer');
  cxSetResourceString(@sdxHintViewExplorer,'Show Explorer');

  cxSetResourceString(@sdxSummary,'Summary');
  cxSetResourceString(@sdxCreator,'Creato&r:');
  cxSetResourceString(@sdxCreationDate,'Create&d:');
 
  cxSetResourceString(@sdxMenuViewThumbnails,'Th&umbnails');
  cxSetResourceString(@sdxMenuThumbnailsLarge,'&Large Thumbnails');
  cxSetResourceString(@sdxMenuThumbnailsSmall,'&Small Thumbnails');
  
  cxSetResourceString(@sdxHintViewThumbnails,'Show Thumbnails');
  cxSetResourceString(@sdxHintThumbnailsLarge,'Switch to large thumbnails');
  cxSetResourceString(@sdxHintThumbnailsSmall,'Switch to small thumbnails');
    
  cxSetResourceString(@sdxMenuFormatTitle,'T&itle...');
  cxSetResourceString(@sdxHintFormatTitle,'Format Report Title');

  cxSetResourceString(@sdxHalf,'一半');
  //tt8//'Half');                                                                                                                                                                                                                     
  cxSetResourceString(@sdxPredefinedFunctions,'预定义函数'); // dxPgsDlg.pas
  //tt8//' Predefined Functions '); // dxPgsDlg.pas                                                                                                                                              
  cxSetResourceString(@sdxZoomParameters,'缩放参数(&P)');          // dxPSPrvwOpt.pas
  //tt8//' Zoom &Parameters ');          // dxPSPrvwOpt.pas                                                                                                                             
                                                                                                                                                                                                                                                          
  cxSetResourceString(@sdxWrapData,'包装数据');
  //tt8//'&Wrap Data');

  {cxSetResourceString(@sdxMenuShortcutExplorer,'Explorer');
  cxSetResourceString(@sdxExplorerToolBar,'Explorer');

  cxSetResourceString(@sdxMenuShortcutThumbnails,'Thumbnails');
  
  { TreeView New}
  
 { cxSetResourceString(@sdxButtons,'Buttons');
  
  { ListView }

 { cxSetResourceString(@sdxBtnHeadersFont,'&Headers Font...');
  cxSetResourceString(@sdxHeadersTransparent,'Transparent &Headers');
  cxSetResourceString(@sdxHintListViewDesignerMessage,' Most Options Are Being Taken Into Account Only In Detailed View');
  cxSetResourceString(@sdxColumnHeaders,'&Column Headers');
  
  { Group LookAndFeel Names }

 { cxSetResourceString(@sdxReportGroupNullLookAndFeel,'Null');
  cxSetResourceString(@sdxReportGroupStandardLookAndFeel,'Standard');
  cxSetResourceString(@sdxReportGroupOfficeLookAndFeel,'Office');  
  cxSetResourceString(@sdxReportGroupWebLookAndFeel,'Web');
  
  cxSetResourceString(@sdxLayoutGroupDefaultCaption,'Layout Group');
  cxSetResourceString(@sdxLayoutItemDefaultCaption,'Layout Item');

  { Designer Previews}

  { Localize if you want (they are used inside FormatReport dialog -> ReportPreview) }
    
 { cxSetResourceString(@sdxCarManufacturerName5,'DaimlerChrysler AG');
  cxSetResourceString(@sdxCarManufacturerCountry5,'Germany');
  cxSetResourceString(@sdxCarModel5,'Maybach 62');

  cxSetResourceString(@sdxLuxurySedans,'Luxury Sedans');
  cxSetResourceString(@sdxCarManufacturer,'Manufacturer');
  cxSetResourceString(@sdxCarModel,'Model');
  cxSetResourceString(@sdxCarEngine,'Engine');
  cxSetResourceString(@sdxCarTransmission,'Transmission');
  cxSetResourceString(@sdxCarTires,'Tires');
  cxSetResourceString(@sdx760V12Manufacturer,'BMW');
  cxSetResourceString(@sdx760V12Model,'760Li V12');
  cxSetResourceString(@sdx760V12Engine,'6.0L DOHC V12 438 HP 48V DI Valvetronic 12-cylinder engine with 6.0-liter displacement, dual overhead cam valvetrain');
  cxSetResourceString(@sdx760V12Transmission,'Elec 6-Speed Automatic w/Steptronic');
  cxSetResourceString(@sdx760V12Tires,'P245/45R19 Fr - P275/40R19 Rr Performance. Low Profile tires with 245mm width, 19.0" rim');
      
  { Styles }

 { cxSetResourceString(@sdxBandHeaderStyle,'BandHeader');
  cxSetResourceString(@sdxCaptionStyle,'Caption');
  cxSetResourceString(@sdxCardCaptionRowStyle,'Card Caption Row');
  cxSetResourceString(@sdxCardRowCaptionStyle,'Card Row Caption');
  cxSetResourceString(@sdxCategoryStyle,'Category');
  cxSetResourceString(@sdxContentStyle,'Content');
  cxSetResourceString(@sdxContentEvenStyle,'Content Even Rows');
  cxSetResourceString(@sdxContentOddStyle,'Content Odd Rows');
  cxSetResourceString(@sdxFilterBarStyle,'Filter Bar');
  cxSetResourceString(@sdxFooterStyle,'Footer');
  cxSetResourceString(@sdxFooterRowStyle,'Footer Row');
  cxSetResourceString(@sdxGroupStyle,'Group');
  cxSetResourceString(@sdxHeaderStyle,'Header');
  cxSetResourceString(@sdxIndentStyle,'Indent');
  cxSetResourceString(@sdxPreviewStyle,'Preview');
  cxSetResourceString(@sdxSelectionStyle,'Selection');

  cxSetResourceString(@sdxStyles,'Styles');
  cxSetResourceString(@sdxStyleSheets,'Style Sheets');
  cxSetResourceString(@sdxBtnTexture,'&Texture...');
  cxSetResourceString(@sdxBtnTextureClear,'Cl&ear');
  cxSetResourceString(@sdxBtnColor,'Co&lor...');
  cxSetResourceString(@sdxBtnSaveAs,'Save &As...');
  cxSetResourceString(@sdxBtnRename,'&Rename...');
  
  cxSetResourceString(@sdxLoadBitmapDlgTitle,'Load Texture');
  
  cxSetResourceString(@sdxDeleteStyleSheet,'Delete StyleSheet Named "%s"?');
  cxSetResourceString(@sdxUnnamedStyleSheet,'Unnamed');
  cxSetResourceString(@sdxCreateNewStyleQueryNamePrompt,'Enter New StyleSheet Name: ');
  cxSetResourceString(@sdxStyleSheetNameAlreadyExists,'StyleSheet named "%s" Already Exists');

  cxSetResourceString(@sdxCannotLoadImage,'Cannot Load Image "%s"');
  cxSetResourceString(@sdxUseNativeStyles,'&Use Native Styles');
  cxSetResourceString(@sdxSuppressBackgroundBitmaps,'&Suppress Background Textures');
  cxSetResourceString(@sdxConsumeSelectionStyle,'Consume Selection Style');
  
  { Grid4 new }

 { cxSetResourceString(@sdxSize,'大小');
  //tt8//'Size'); 
  cxSetResourceString(@sdxLevels,'Levels');
  cxSetResourceString(@sdxUnwrap,'&Unwrap');
  cxSetResourceString(@sdxUnwrapTopLevel,'Un&wrap Top Level');
  cxSetResourceString(@sdxRiseActiveToTop,'Rise Active Level onto Top');
  cxSetResourceString(@sdxCannotUseOnEveryPageModeInAggregatedState,
    'Cannot Use OnEveryPage Mode'+ #13#10 + 
    'While Performing in Aggregated State');

  cxSetResourceString(@sdxPagination,'Pagination');
  cxSetResourceString(@sdxByBands,'By Bands');
  cxSetResourceString(@sdxByColumns,'By Columns');
  cxSetResourceString(@sdxByRows,'By &Rows');
  cxSetResourceString(@sdxByTopLevelGroups,'By TopLevel Groups'); 
  cxSetResourceString(@sdxOneGroupPerPage,'One Group Per Page'); 

  {* For those who will translate *}
  {* You should also check "cxSetResourceString(@sdxCannotUseOnEveryPageMode" resource string - see above *}
  {* It was changed to "- Toggle "Unwrap" Option off on "Behaviors" Tab"*}
   
  { TL 4 }
 { cxSetResourceString(@sdxBorders,'Borders');
  cxSetResourceString(@sdxExplicitlyExpandNodes,'Explicitly Expand Nodes');
  cxSetResourceString(@sdxNodes,'&Nodes');
  cxSetResourceString(@sdxSeparators,'Separators');
  cxSetResourceString(@sdxThickness,'Thickness');
  cxSetResourceString(@sdxTLIncorrectHeadersState,
    'Cannot Use Headers OnEveryPage Mode' + #13#10 + 
    #13#10 +
    'You should either:' + #13#10 +
    '  - Set Band OnEveryPage Option On' + #13#10 +
    '  - Set Band Visible Option Off');

  { cxVerticalGrid }

 { cxSetResourceString(@sdxRows,'&Rows');

  cxSetResourceString(@sdxMultipleRecords,'&Multiple Records');
  cxSetResourceString(@sdxBestFit,'&Best Fit');
  cxSetResourceString(@sdxKeepSameRecordWidths,'&Keep Same Record Widths');
  cxSetResourceString(@sdxWrapRecords,'&Wrap Records');

  cxSetResourceString(@sdxByWrapping,'By &Wrapping');
  cxSetResourceString(@sdxOneWrappingPerPage,'&One Wrapping Per Page');

  {new in 3.01}
 { cxSetResourceString(@sdxCurrentRecord,'Current Record');
  cxSetResourceString(@sdxLoadedRecords,'Loaded Records');
  cxSetResourceString(@sdxAllRecords,'All Records');
  
  { Container Designer }
  
 { cxSetResourceString(@sdxPaginateByControlDetails,'Control Details');
  cxSetResourceString(@sdxPaginateByControls,'Controls');
  cxSetResourceString(@sdxPaginateByGroups,'Groups');
  cxSetResourceString(@sdxPaginateByItems,'Items');
  
  cxSetResourceString(@sdxControlsPlace,'Controls Place');
  cxSetResourceString(@sdxExpandHeight,'Expand Height');
  cxSetResourceString(@sdxExpandWidth,'Expand Width');
  cxSetResourceString(@sdxShrinkHeight,'Shrink Height');
  cxSetResourceString(@sdxShrinkWidth,'Shrink Width');
  
  cxSetResourceString(@sdxCheckAll,'Check &All');
  cxSetResourceString(@sdxCheckAllChildren,'Check All &Children');
  cxSetResourceString(@sdxControlsTab,'Controls');
  cxSetResourceString(@sdxExpandAll,'E&xpand All');
  cxSetResourceString(@sdxHiddenControlsTab,'Hidden Controls');
  cxSetResourceString(@sdxReportLinksTab,'Aggregated Designers');
  cxSetResourceString(@sdxAvailableLinks,'&Available Links:');
  cxSetResourceString(@sdxAggregatedLinks,'A&ggregated Links:');
  cxSetResourceString(@sdxTransparents,'Transparents');
  cxSetResourceString(@sdxUncheckAllChildren,'Uncheck &All Children');
  
  cxSetResourceString(@sdxRoot,'&Root');
  cxSetResourceString(@sdxRootBorders,'Root &Borders');
  cxSetResourceString(@sdxControls,'&Controls');
  cxSetResourceString(@sdxContainers,'C&ontainers');

  cxSetResourceString(@sdxHideCustomContainers,'&Hide Custom Containers');

  { General }
  
  // FileSize abbreviation

  {cxSetResourceString(@sdxBytes,'Bytes');
  cxSetResourceString(@sdxKiloBytes,'KB');
  cxSetResourceString(@sdxMegaBytes,'MB');
  cxSetResourceString(@sdxGigaBytes,'GB');

  // Misc.

  cxSetResourceString(@sdxThereIsNoPictureToDisplay,'There is no picture to display');
  cxSetResourceString(@sdxInvalidRootDirectory,'Directory "%s" does not exists. Continue selection ?');
  cxSetResourceString(@sdxPressEscToCancel,'Press Esc to cancel');
  cxSetResourceString(@sdxMenuFileRebuild,'&Rebuild');
  cxSetResourceString(@sdxBuildingReportStatusText,'Building report - Press Esc to cancel');
  cxSetResourceString(@sdxPrintingReportStatusText,'Printing report - Press Esc to cancel');
  
  cxSetResourceString(@sdxBuiltIn,'[BuiltIn]');
  cxSetResourceString(@sdxUserDefined,'[User Defined]');
  cxSetResourceString(@sdxNewStyleRepositoryWasCreated,'New StyleRepository "%s" was created and assigned');

  { new in PS 3.1}
 { cxSetResourceString(@sdxLineSpacing,'&Line spacing:');
  cxSetResourceString(@sdxTextAlignJustified,'Justified');
  cxSetResourceString(@sdxSampleText,'Sample Text Sample Text');
  
  cxSetResourceString(@sdxCardsRows,'&Cards');
  cxSetResourceString(@sdxTransparentRichEdits,'Transparent &RichEdit Content');

  cxSetResourceString(@sdxIncorrectFilterBarState,
    'Cannot Use FilterBar OnEveryPage Mode' + #13#10 +
    #13#10 +
    'You should either:' + #13#10 +  
    '  - Set Caption OnEveryPage Option On' + #13#10 +
    '  - Set Caption Visible Option Off');
  cxSetResourceString(@sdxIncorrectBandHeadersState2,
    'Cannot Use BandHeaders OnEveryPage Mode' + #13#10 +
    #13#10 +
    'You should either:' + #13#10 +  
    '  - Set Caption and FilterBar OnEveryPage Option On' + #13#10 + 
    '  - Set Caption and FilterBar Visible Option Off');
  cxSetResourceString(@sdxIncorrectHeadersState2,
    'Cannot Use Headers OnEveryPage Mode' + #13#10 +
    #13#10 +
    'You should either:' + #13#10 +  
    '  - Set Caption, FilterBar and Band OnEveryPage Option On' + #13#10 +
    '  - Set Caption, FilterBar and Band Visible Option Off');

 { new in PS 3.2}   
{  cxSetResourceString(@sdxAvailableReportLinks,'Available ReportLinks');
  cxSetResourceString(@sdxBtnRemoveInconsistents,'Remove Unneeded');
  cxSetResourceString(@sdxColumnHeadersOnEveryPage,'Column &Headers');
  
 { Scheduler }   

 { cxSetResourceString(@sdxNotes,'Notes');
  cxSetResourceString(@sdxTaskPad,'TaskPad');
  cxSetResourceString(@sdxPrimaryTimeZone,'Primary');
  cxSetResourceString(@sdxSecondaryTimeZone,'Secondary');
  
  cxSetResourceString(@sdxDay,'Day');
  cxSetResourceString(@sdxWeek,'Week');
  cxSetResourceString(@sdxMonth,'Month');
  
  cxSetResourceString(@sdxSchedulerSchedulerHeader,'Scheduler Header');
  cxSetResourceString(@sdxSchedulerContent,'Content');
  cxSetResourceString(@sdxSchedulerDateNavigatorContent,'DateNavigator Content');
  cxSetResourceString(@sdxSchedulerDateNavigatorHeader,'DateNavigator Header');
  cxSetResourceString(@sdxSchedulerDayHeader,'Day Header');
  cxSetResourceString(@sdxSchedulerEvent,'Event');
  cxSetResourceString(@sdxSchedulerResourceHeader,'Resource Header');
  cxSetResourceString(@sdxSchedulerNotesAreaBlank,'Notes Area (Blank)');
  cxSetResourceString(@sdxSchedulerNotesAreaLined,'Notes Area (Lined)');
  cxSetResourceString(@sdxSchedulerTaskPad,'TaskPad');
  cxSetResourceString(@sdxSchedulerTimeRuler,'Time Ruler');
  
  cxSetResourceString(@sdxPrintStyleNameDaily,'Daily');
  cxSetResourceString(@sdxPrintStyleNameWeekly,'Weekly');
  cxSetResourceString(@sdxPrintStyleNameMonthly,'Monthly');
  cxSetResourceString(@sdxPrintStyleNameDetails,'Details');
  cxSetResourceString(@sdxPrintStyleNameMemo,'Memo');
  cxSetResourceString(@sdxPrintStyleNameTrifold,'Trifold');
  
  cxSetResourceString(@sdxPrintStyleCaptionDaily,'Daily Style');
  cxSetResourceString(@sdxPrintStyleCaptionWeekly,'Weekly Style');
  cxSetResourceString(@sdxPrintStyleCaptionMonthly,'Monthly Style');
  cxSetResourceString(@sdxPrintStyleCaptionDetails,'Calendar Details Style');
  cxSetResourceString(@sdxPrintStyleCaptionMemo,'Memo Style');
  cxSetResourceString(@sdxPrintStyleCaptionTrifold,'Tri-fold Style');

  cxSetResourceString(@sdxTabPrintStyles,'Print Styles');
  
  cxSetResourceString(@sdxPrintStyleDontPrintWeekEnds,'&Don''t Print Weekends');
  cxSetResourceString(@sdxPrintStyleInclude,'Include:');
  cxSetResourceString(@sdxPrintStyleIncludeTaskPad,'Task&Pad');
  cxSetResourceString(@sdxPrintStyleIncludeNotesAreaBlank,'Notes Area (&Blank)');
  cxSetResourceString(@sdxPrintStyleIncludeNotesAreaLined,'Notes Area (&Lined)');
  cxSetResourceString(@sdxPrintStyleLayout,'&Layout:');
  cxSetResourceString(@sdxPrintStylePrintFrom,'Print &From:');
  cxSetResourceString(@sdxPrintStylePrintTo,'Print &To:');
  
  cxSetResourceString(@sdxPrintStyleDailyLayout1PPD,'1 page/day');
  cxSetResourceString(@sdxPrintStyleDailyLayout2PPD,'2 pages/day');
  
  cxSetResourceString(@sdxPrintStyleWeeklyArrange,'&Arrange:');
  cxSetResourceString(@sdxPrintStyleWeeklyArrangeT2B,'Top to bottom');
  cxSetResourceString(@sdxPrintStyleWeeklyArrangeL2R,'Left to right');
  cxSetResourceString(@sdxPrintStyleWeeklyLayout1PPW,'1 page/week');
  cxSetResourceString(@sdxPrintStyleWeeklyLayout2PPW,'2 pages/week');

  cxSetResourceString(@sdxPrintStyleMemoPrintOnlySelectedEvents,'Print Only Selected Events');

  cxSetResourceString(@sdxPrintStyleMonthlyLayout1PPM,'1 page/month');
  cxSetResourceString(@sdxPrintStyleMonthlyLayout2PPM,'2 pages/month');
  cxSetResourceString(@sdxPrintStyleMonthlyPrintExactly1MPP,'Print &Exactly One Month Per Page');
  
  cxSetResourceString(@sdxPrintStyleTrifoldSectionModeDailyCalendar,'Daily Calendar');
  cxSetResourceString(@sdxPrintStyleTrifoldSectionModeWeeklyCalendar,'Weekly Calendar');
  cxSetResourceString(@sdxPrintStyleTrifoldSectionModeMonthlyCalendar,'Monthly Calendar');
  cxSetResourceString(@sdxPrintStyleTrifoldSectionModeTaskPad,'TaskPad');
  cxSetResourceString(@sdxPrintStyleTrifoldSectionModeNotesBlank,'Notes (Blank)');
  cxSetResourceString(@sdxPrintStyleTrifoldSectionModeNotesLined,'Notes (Lined)');
  cxSetResourceString(@sdxPrintStyleTrifoldSectionLeft,'&Left Section:');
  cxSetResourceString(@sdxPrintStyleTrifoldSectionMiddle,'&Monthly Section:');
  cxSetResourceString(@sdxPrintStyleTrifoldSectionRight,'&Right Section:');

  cxSetResourceString(@sdxPrintStyleDetailsStartNewPageEach,'Start a New Page Each:');

  cxSetResourceString(@sdxSuppressContentColoration,'Suppress &Content Coloration');
  cxSetResourceString(@sdxOneResourcePerPage,'One &Resource Per Page');

  cxSetResourceString(@sdxPrintRanges,'Print Ranges');
  cxSetResourceString(@sdxPrintRangeStart,'&Start:');
  cxSetResourceString(@sdxPrintRangeEnd,'&End:');
  cxSetResourceString(@sdxHideDetailsOfPrivateAppointments,'&Hide Details of Private Appointments');
  cxSetResourceString(@sdxResourceCountPerPage,'&Resources/Page:');

  cxSetResourceString(@sdxSubjectLabelCaption,'Subject:');
  cxSetResourceString(@sdxLocationLabelCaption,'Location:');
  cxSetResourceString(@sdxStartLabelCaption,'Start:');
  cxSetResourceString(@sdxFinishLabelCaption,'End:');
  cxSetResourceString(@sdxShowTimeAsLabelCaption,'Show Time As:');
  cxSetResourceString(@sdxRecurrenceLabelCaption,'Recurrence:');
  cxSetResourceString(@sdxRecurrencePatternLabelCaption,'Recurrence Pattern:');

  //messages
  cxSetResourceString(@sdxSeeAboveMessage,'Please See Above');
  cxSetResourceString(@sdxAllDayMessage,'All Day');
  cxSetResourceString(@sdxContinuedMessage,'Continued');
  cxSetResourceString(@sdxShowTimeAsFreeMessage,'Free');
  cxSetResourceString(@sdxShowTimeAsTentativeMessage,'Tentative');
  cxSetResourceString(@sdxShowTimeAsOutOfOfficeMessage,'Out of Office');

  cxSetResourceString(@sdxRecurrenceNoneMessage,'(none)');
  cxSetResourceString(@scxRecurrenceDailyMessage,'Daily');
  cxSetResourceString(@scxRecurrenceWeeklyMessage,'Weekly');
  cxSetResourceString(@scxRecurrenceMonthlyMessage,'Monthly');
  cxSetResourceString(@scxRecurrenceYearlyMessage,'Yearly');

  //error messages
  cxSetResourceString(@sdxInconsistentTrifoldStyle,'The Tri-fold style requires at least one calendar section. ' +
    'Select Daily, Weekly, or Monthly Calendar for one of section under Options.');
  cxSetResourceString(@sdxBadTimePrintRange,'The hours to print are not valid. The start time must precede the end time.');
  cxSetResourceString(@sdxBadDatePrintRange,'The date in the End box cannot be prior to the date in the Start box.');
  cxSetResourceString(@sdxCannotPrintNoSelectedItems,'Cannot print unless an item is selected. Select an item, and then try to print again.');
  cxSetResourceString(@sdxCannotPrintNoItemsAvailable,'No items available within the specified print range.');      }
  //-------------------------------------------------------------------------//}
end;

initialization
  toChinese;

finalization

end.


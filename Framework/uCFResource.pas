//Common Framework
//资源定义单元
unit uCFResource;

interface

ResourceString
  SError                        = '错误';
  SConfirm                      = '确认';
  SWarning                      = '警告';
  SInformation                  = '信息';

  SOK                           = '确定';
  SCancel                       = '取消';
  SAdd                          = '新增';
  SEdit                         = '编辑';
  SDelete                       = '删除';

  SEFactoryCreateInterfaceFail  = '错误: 从接口工厂获取接口失败.';
  SECantCreateInstance          = '不能创建实例.';
  SEPoolsFull                   = '池已满.';
  SERecordNotFound              = '记录未找到';
  SEFileNotExists               = '文件不存在';

  SEParam                       = '参数错误.';
  SEDisconnected                = '连接已断开.';

  SEAuthenticationFailed        = '验证失败.';
  SEPermissionDenied            = '没有权限.';
  SEUserNotExists               = '用户不存在!';
  SEUserDisabled                = '验证失败.';//'用户被禁用!';

implementation

end.

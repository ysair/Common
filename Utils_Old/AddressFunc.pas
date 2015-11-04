unit AddressFunc;

interface
uses Windows, Forms, Classes, SysUtils, Controls ;

function NewAddress(AForm: TForm; var AUser, AMobile, AEmail: String): Boolean;
function EditAddress(AForm: TForm; var AUser, AMobile, AEmail: String): Boolean;

implementation
uses AddressProFrm ;


//新建一个地址，返回结果是这个地址在数据库里面的ID
function NewAddress(AForm: TForm; var AUser, AMobile, AEmail: String): Boolean;
begin
  with TAddressProForm.Create(AForm) do
  begin
    EditType := edtNew ;
    edtName.Text   := AUser;
    edtMobile.Text := AMobile;
    edtEmail.Text  := AEmail;

    Result := ShowModal = mrOK;
    if Result then
    begin
      AUser   := edtName.Text;
      AMobile := edtMobile.Text;
      AEmail  := edtEmail.Text;
    end;

    Free;
  end;
end;

//编辑一个地址，返回结果是用户是否点击了“确定”
function EditAddress(AForm: TForm; var AUser, AMobile, AEmail: String): Boolean;
begin
  with TAddressProForm.Create(AForm) do
  begin
    EditType := edtModify;
    edtName.Text   := AUser;
    edtMobile.Text := AMobile;
    edtEmail.Text  := AEmail;

    Result := ShowModal = mrOK;
    if Result then
    begin
      AUser   := edtName.Text;
      AMobile := edtMobile.Text;
      AEmail  := edtEmail.Text;
    end;

    Free;
  end;
end;

end.

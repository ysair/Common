//******************************************************************************
// Nom           : WinDns.pas
// Utilisation   : Fonction et Type pour l'acc¨¨s ¨¤ DnsApi.dll
// Auteur        : uncle_khemi@hotmail.com
// Date          : 27 Ao?t 2003
//
// Modifications :
// Date          :
//******************************************************************************

unit WinDNS;

interface

uses
  Windows, Classes, SysUtils, StrUtils;

const
  //  Options for DnsQuery
  DNS_QUERY_STANDARD      = $0;
  DNS_QUERY_ACCEPT_TRUNCATED_RESPONSE = $1;
  DNS_QUERY_USE_TCP_ONLY  = $2;
  DNS_QUERY_NO_RECURSION  = $4;
  DNS_QUERY_BYPASS_CACHE  = $8;
  DNS_QUERY_NO_WIRE_QUERY = $10;
  DNS_QUERY_NO_LOCAL_NAME = $20;
  DNS_QUERY_NO_HOSTS_FILE = $40;
  DNS_QUERY_NO_NETBT      = $80;
  DNS_QUERY_WIRE_ONLY     = $100;
  DNS_QUERY_TREAT_AS_FQDN = $1000;

  //autres
  DNS_ATMA_MAX_ADDR_LENGTH = 20;
  DNS_ATMA_AESA_ADDR_LENGTH = 20;

  DNS_TYPE_A      = $1;
  DNS_TYPE_NS     = $2;
  DNS_TYPE_CNAME  = $5;
  DNS_TYPE_PTR    = $C;
  DNS_TYPE_MX     = $F;
  DNS_TYPE_ADDRS  = $F8;

  DNS_UPDATE_SECURITY_USE_DEFAULT = 0;

type
  IP6_ADDRESS = array[0..3] of dword;
  //IP4_ADDRESS = DWORD;
  IP4_ADDRESS = record
    case Integer of
      0 : (VData : DWORD);
      1 : (VArray : array[0..3] of Byte);
  end;
  DNS_A_DATA = IP4_ADDRESS;
  DNS_PTR_DATA = PChar;
  DNS_PTR_DATAA = DNS_PTR_DATA;
  DNS_PTR_DATAW = DNS_PTR_DATA;
  DNS_AAAA_DATA = IP6_ADDRESS;
  DNS_STATUS = LongInt;

  //validation d'un nom DNS
  DNS_NAME_FORMAT = (DnsNameDomain,
    DnsNameDomainLabel,
    DnsNameHostnameFull,
    DnsNameHostnameLabel,
    DnsNameWildcard,
    DnsNameSrvRecord);

  //d¨¦finie le type de lib¨¦ration pour avec DnsFreeRecordList
  DNS_FREE_TYPE = (
    DnsFreeFlat,
    DnsFreeRecordList,
    DnsFreeParsedMessageFields
    );

  //tableau d'adresse IP
  PIP4_ARRAY = ^IP4_ARRAY;
  IP4_ARRAY = record
    AddrCount: DWORD;
    AddrArray: array[0..0] of IP4_ADDRESS;                 //[0..10]
  end;

  DNS_SRV_DATAA = record
    pNameTarget: PChar;
    wPriority: Word;
    wWeighty: Word;
    wPorty: Word;
    Pady: Word;                                             // keep ptrs DWORD aligned
  end;

  DNS_TSIG_DATAA = record
    pNameAlgorithm: PChar;
    pAlgorithmPacket: ^Byte;
    pSignature: ^Byte;
    pOtherData: ^Byte;
    i64CreateTime: longlong;
    wFudgeTime: Word;
    wOriginalXid: Word;
    wError: Word;
    wSigLength: Word;
    wOtherLength: Word;
    cAlgNameLength: UCHAR;
    bPacketPointers: Boolean;
  end;

  DNS_NXT_DATAA = record
    pNameNext: PChar;
    wNumTypes: Word;
    wTypes: array[0..1] of Word;
  end;

  DNS_WINSR_DATA = record
    dwMappingFlag: DWORD;
    dwLookupTimeout: DWORD;
    dwCacheTimeout: DWORD;
    pNameResultDomain: PWideChar;
  end;

  DNS_WINSR_DATAA = record
    dwMappingFlag: DWORD;
    dwLookupTimeout: DWORD;
    dwCacheTimeout: DWORD;
    pNameResultDomain: PChar;
  end;

  DNS_RECORD_FLAGS = record
    Section: DWORD;                                         //DWORD   Section     : 2;
    Delete: DWORD;                                          //DWORD   Delete      : 1;
    CharSet: DWORD;                                         //DWORD   CharSet     : 2;
    Unused: DWORD;                                          //DWORD  Unused      : 3;
    Reserved: DWORD;                                        //DWORD  Reserved    : 24;
  end;

  DNS_TXT_DATAA = record
    dwStringCount: DWORD;
    pStringArray: array[0..10] of PChar;
  end;

  DNS_NULL_DATA = record
    dwByteCount: DWORD;
    Data: array[0..10] of Byte;
  end;

  DNS_KEY_DATA = record
    wFlags: Word;
    chProtocol: Byte;
    chAlgorithm: Byte;
    Key: array[0..0] of Byte;
  end;

  DNS_SIG_DATAA = record
    pNameSigner: PChar;
    wTypeCovered: Word;
    chAlgorithm: Byte;
    chLabelCount: Byte;
    dwOriginalTtl: DWORD;
    dwExpiration: DWORD;
    dwTimeSigned: DWORD;
    wKeyTag: Word;
    Pad: Word;                                              // keep Byte field aligned
    Signature: array[0..0] of Byte;
  end;

  DNS_ATMA_DATA = record
    AddressType: Byte;
    Address: array[0..(DNS_ATMA_MAX_ADDR_LENGTH - 1)] of Byte;
  end;

  DNS_WKS_DATA = record
    IpAddress: IP4_ADDRESS;
    chProtocol: UCHAR;
    BitMask: array[0..0] of Byte;                           // BitMask[1];
  end;

  DNS_MX_DATAA = record
    pNameExchange: PChar;
    wPreference: Word;
    Pad: Word;
  end;

  DNS_MINFO_DATAA = record
    pNameMailbox: PChar;
    pNameErrorsMailbox: PChar;
  end;

  DNS_WINS_DATA = record
    dwMappingFlag: DWORD;
    dwLookupTimeout: DWORD;
    dwCacheTimeout: DWORD;
    cWinsServerCount: DWORD;
    WinsServers: array[0..0] of IP4_ADDRESS;
  end;

  DNS_TKEY_DATAA = record
    pNameAlgorithm: PChar;
    pAlgorithmPacket: ^Byte;
    pKey: ^Byte;
    pOtherData: ^Byte;
    dwCreateTime: DWORD;
    dwExpireTime: DWORD;
    wMode: Word;
    wError: Word;
    wKeyLength: Word;
    wOtherLength: Word;
    cAlgNameLength: UCHAR;
    bPacketPointers: Boolean;
  end;

  DNS_SOA_DATAA = record
    pNamePrimaryServer: PChar;
    pNameAdministrator: PChar;
    dwSerialNo: DWORD;
    dwRefresh: DWORD;
    dwRetry: DWORD;
    dwExpire: DWORD;
    dwDefaultTtl: DWORD;
  end;

  //probl¨¨me non r¨¦solu lorsqu'on utilise les flags de type S
  TFlags = record
    case Integer of
      1: (DW: DWORD);                                       // flags as DWORD
      2: (S: ^DNS_RECORD_FLAGS);                            // flags as structure   ???
  end;

  TDataA = record
    case Integer of
      1: (A: DNS_A_DATA);                                   //    A;
      2: (SOA: DNS_SOA_DATAA);                              //   SOA, Soa;
      3: (PTR: DNS_PTR_DATAA);                              //PTR, Ptr, NS, Ns, CNAME, Cname, MB, Mb, MD, Md, MF, Mf, MG, Mg, MR, Mr;
      4: (MINFO: DNS_MINFO_DATAA);                          //MINFO, Minfo,    RP, Rp;
      5: (MX: DNS_MX_DATAA);                                //MX, Mx,         AFSDB, Afsdb,             RT, Rt;
      6: (HINFO: DNS_TXT_DATAA);                            //HINFO, Hinfo,        ISDN, Isdn,        TXT, Txt,          X25;
      7: (Null: DNS_NULL_DATA);                             //Null;
      8: (WKS: DNS_WKS_DATA);                               //WKS, Wks;
      9: (AAAA: DNS_AAAA_DATA);                             //AAAA;
      10: (KEY: DNS_KEY_DATA);                              //KEY, Key;
      11: (SIG: DNS_SIG_DATAA);                             //SIG, Sig;
      12: (ATMA: DNS_ATMA_DATA);                            //ATMA, Atma;
      13: (NXT: DNS_NXT_DATAA);                             //NXT, Nxt;
      14: (SRV: DNS_SRV_DATAA);                             //SRV, Srv;
      15: (TKEY: DNS_TKEY_DATAA);                           //TKEY, Tkey;
      16: (TSIG: DNS_TSIG_DATAA);                           //TSIG, Tsig;
      17: (DWINS: DNS_WINS_DATA);                           //WINS, Wins;
      18: (WINSR: DNS_WINSR_DATA);                          //WINSR, WinsR, NBSTAT, Nbstat;
  end;

  PDNS_RECORDA = ^DNS_RECORDA;
  DNS_RECORDA = record
    pnext: PDNS_RECORDA;                                    //  struct _DnsRecordW *
    pName: PChar;                                           //PSTR
    wType: Word;                                            //WORD                                              //WORD                    wType;
    wDataLength: Word;                                      //WORD
    flags: TFlags;                                          //
    dwTtl: DWORD;                                           //DWORD;
    dwReserved: DWORD;                                      //DWORD;
    Data: TDataA;
  end;

  //------------------------------------------------------------------------------
  //Fonctions
  //------------------------------------------------------------------------------

  //------------------------------------------------------------------------------
  //voir un enregistrement
function DnsQuery_A(
  pszName: PChar;
  wType: Word;
  Options: DWORD;
  aipServers: PIP4_ARRAY;
  ppQueryResults: Pointer;
  pReserved: Pointer
  ): DNS_STATUS; stdcall; external 'dnsapi.dll';
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//ajouter, modifier et supprimer un enregistrement
function DnsModifyRecordsInSet_A(
  pAddRecords: PDNS_RECORDA;
  pDeleteRecords: PDNS_RECORDA;
  Options: DWORD;
  hContext: Hwnd;
  pServerList: PIP4_ARRAY;
  pReserved: Pointer
  ): DNS_STATUS; stdcall; external 'dnsapi.dll';
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//verifie si un nom DNS est correct
function DnsValidateName_A(
  pszName: PChar;
  Format: DNS_NAME_FORMAT
  ): DNS_STATUS; stdcall; external 'dnsapi.dll';
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//supprime la m¨¦moire alou¨¦ pour la reponse par un DNS_QUERY
procedure DnsRecordListFree(
  pRecordList: PDNS_RECORDA;
  FreeType: DNS_FREE_TYPE
  ); stdcall; external 'dnsapi.dll';
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//supprime la m¨¦moire alou¨¦ pour la reponse par un DNS_QUERY
//procedure DnsFreeRecordListDeep(
//  pRecordList: PDNS_RECORDA;
//  FreeType: DNS_FREE_TYPE
//  ); stdcall; external 'dnsapi.dll';
//------------------------------------------------------------------------------

function ResolveHost(
    const DNSServer : string;
    const HostName  : string;
    const IPList    : TStrings
    ):Boolean; overload;

function ResolveHost(
    const DNSServerList : TStrings;
    const HostName  : string;
    const IPList    : TStrings
    ):Boolean; overload;

implementation

function StrToIP4_ADDRESS(
    const AIP : string
    ): IP4_ADDRESS;
var
  s : string;
  p : Integer;
begin
  ZeroMemory(@Result, SizeOf(Result));
  s :=  AIP;
  p :=  Pos('.', s);
  Result.VArray[0]  :=  StrToIntDef(LeftBStr(s, p - 1), 0);
  Delete(s, 1, p);
  p :=  Pos('.', s);
  Result.VArray[1]  :=  StrToIntDef(LeftBStr(s, p - 1), 0);
  Delete(s, 1, p);
  p :=  Pos('.', s);
  Result.VArray[2]  :=  StrToIntDef(LeftBStr(s, p - 1), 0);
  Delete(s, 1, p);
  Result.VArray[3]  :=  StrToIntDef(s, 0);
end;

function IP4_ADDRESSToStr(
    const IP  : IP4_ADDRESS
    ):string;
begin
  Result  :=  IntToStr(IP.VArray[0]) + '.' +
      IntToStr(IP.VArray[1]) + '.' +
      IntToStr(IP.VArray[2]) + '.' +
      IntToStr(IP.VArray[3]);
end;

function ResolveHost(
    const DNSServer : string;
    const HostName  : string;
    const IPList    : TStrings
    ):Boolean;
var
  pQueryResultsSet : PDNS_RECORDA;
  pSet  : PDNS_RECORDA;
  rep : LongWord;
  psHost    : array[0..MAXBYTE] of Char;
  ipServers : IP4_array;
  f : Boolean;
  ip  : string;
begin
  try
    pQueryResultsSet :=  nil;
    StrPCopy(psHost, Hostname);
    ipServers.AddrCount := 1;
    ipServers.AddrArray[0]  :=  StrToIP4_ADDRESS(DNSServer);
    rep := DnsQuery_A(
        psHost,
        DNS_TYPE_A,
        DNS_QUERY_BYPASS_CACHE + DNS_QUERY_NO_HOSTS_FILE,
        @ipServers,
        @pQueryResultsSet,
        nil
        );
    Result  :=  rep = 0;
    if Result then
    begin
      pSet  :=  pQueryResultsSet;
      f :=  False;
      while Assigned(pQueryResultsSet) do
      begin
        if pQueryResultsSet.wType = DNS_TYPE_A then
        begin
          ip  :=  IP4_ADDRESSToStr(pQueryResultsSet.Data.A);
          if IPList.IndexOf(ip) = -1 then
            IPList.Add(ip);
          f :=  True;
        end
        else
          if f then
            break;
        pQueryResultsSet  :=  pQueryResultsSet.pnext;
      end;
      DnsRecordListFree(pSet, DnsFreeRecordList);
    end;
  except
    Result  :=  False;
  end;
end;

function ResolveHost(
    const DNSServerList : TStrings;
    const HostName  : string;
    const IPList    : TStrings
    ):Boolean; overload;
var
  pQueryResultsSet : PDNS_RECORDA;
  pSet  : PDNS_RECORDA;
  rep : LongWord;
  psHost    : array[0..MAXBYTE] of Char;
  pipServers : PIP4_array;
  f : Boolean;
  i : Integer;
  ip  : string;
begin
  try
    pQueryResultsSet :=  nil;
    StrPCopy(psHost, Hostname);
    GetMem(pipServers, SizeOf(DWORD) + SizeOf(IP4_ADDRESS) * DNSServerList.Count);
    try
      pipServers.AddrCount := DNSServerList.Count;
      for i := 0 to DNSServerList.Count - 1 do
        pipServers.AddrArray[i]  :=  StrToIP4_ADDRESS(DNSServerList[i]);
      rep := DnsQuery_A(
          psHost,
          DNS_TYPE_A,
          DNS_QUERY_BYPASS_CACHE + DNS_QUERY_NO_HOSTS_FILE,
          pipServers,
          @pQueryResultsSet,
          nil
          );
      Result  :=  rep = 0;
      if Result then
      begin
        pSet  :=  pQueryResultsSet;
        f :=  False;
        while Assigned(pQueryResultsSet) do
        begin
          if pQueryResultsSet.wType = DNS_TYPE_A then
          begin
            ip  :=  IP4_ADDRESSToStr(pQueryResultsSet.Data.A);
            if IPList.IndexOf(ip) = -1 then
              IPList.Add(ip);
            f :=  True;
          end
          else
            if f then
              break;
          pQueryResultsSet  :=  pQueryResultsSet.pnext;
        end;
        DnsRecordListFree(pSet, DnsFreeRecordList);
      end;

    finally
      FreeMem(pipServers, SizeOf(DWORD) + SizeOf(IP4_ADDRESS) * DNSServerList.Count);
    end;
  except
    Result  :=  False;
  end;
end;

end.


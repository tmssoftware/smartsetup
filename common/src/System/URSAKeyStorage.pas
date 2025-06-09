unit URSAKeyStorage;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs;

type
  IRSAKeyStorage = interface
  ['{747F49FD-A072-48BC-8DA1-DBB55DF17F4A}']
    function PrivateKey(const KeyId: string): TBytes;
    function PublicKey(const KeyId: string): TBytes;
  end;

  TKeyPair = record
    PrivateKey: TBytes;
    PublicKey: TBytes;
  end;

  TRSAKeyStorage = class(TInterfacedObject, IRSAKeyStorage)
  strict private
    FKeysFolder: string;
    FPrivateKeyFileExtension: string;
    FPublicKeyFileExtension: string;
    FKeys: TDictionary<string, TKeyPair>;
    FCriticalSection: TCriticalSection;
    function LoadKeyPair(const KeyId: string): TKeyPair;
  public
    constructor Create(const AKeysFolder: string;
      const APrivateKeyFileExtension: string = '.key';
      const APublicKeyFileExtension: string = '.pub');
    destructor Destroy; override;
    function PrivateKey(const KeyId: string): TBytes;
    function PublicKey(const KeyId: string): TBytes;
  end;

  ERSAKeyStorageException = class(Exception);

  ERSAKeyNotFound = class(ERSAKeyStorageException);

var
  RSAKeyStorage: IRSAKeyStorage;

implementation

uses
  System.IOUtils;

{ TRSAKeyStorage }

constructor TRSAKeyStorage.Create(const AKeysFolder, APrivateKeyFileExtension, APublicKeyFileExtension: string);
begin
  FKeysFolder := AKeysFolder;
  FPrivateKeyFileExtension := APrivateKeyFileExtension;
  FPublicKeyFileExtension := APublicKeyFileExtension;
  FKeys := TDictionary<string, TKeyPair>.Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TRSAKeyStorage.Destroy;
begin
  FCriticalSection.Free;
  FKeys.Free;
  inherited;
end;

function TRSAKeyStorage.LoadKeyPair(const KeyId: string): TKeyPair;

  function GetKeyFileName(const KeyFileExtension: string): string;
  begin
    Result := TPath.ChangeExtension(TPath.Combine(FKeysFolder, KeyId), KeyFileExtension);
  end;

  function GetKey(const KeyFileName: string): TBytes;
  begin
    if TFile.Exists(KeyFileName) then
      Result := TFile.ReadAllBytes(KeyFileName)
    else
      Result := [];
  end;

begin
  FCriticalSection.Acquire;
  try
    if FKeys.ContainsKey(KeyId) then
      Exit(FKeys[KeyId]);
    Result.PrivateKey := GetKey(GetKeyFileName(FPrivateKeyFileExtension));
    Result.PublicKey := GetKey(GetKeyFileName(FPublicKeyFileExtension));
    FKeys.AddOrSetValue(KeyId, Result);
  finally
    FCriticalSection.Release;
  end;
end;

function TRSAKeyStorage.PrivateKey(const KeyId: string): TBytes;
var
  KeyPair: TKeyPair;
begin
  KeyPair := LoadKeyPair(KeyId);
  Result := KeyPair.PrivateKey;
end;

function TRSAKeyStorage.PublicKey(const KeyId: string): TBytes;
var
  KeyPair: TKeyPair;
begin
  KeyPair := LoadKeyPair(KeyId);
  Result := KeyPair.PublicKey;
end;

initialization

finalization
  RSAKeyStorage := nil;

end.

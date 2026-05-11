unit Commands.SelfUpdate.Verify;

interface

procedure VerifySelfUpdateBundle(const CurrentExePath, BundleFolder: string);

implementation
uses System.SysUtils, System.IOUtils, System.Classes, System.Zip,
     Commands.GlobalConfig, UMultiLogger, ULogger, UTmsBuildSystemUtils,
     TMSSystem.Signatures
     {$IFDEF DEBUG}, Testing.Globals{$ENDIF};

procedure VerifySelfUpdateFile(const CurrentExePath, StagedExePath: string);
begin
  {$IFDEF DEBUG}
  if TestParameters.SkipSelfUpdateSignatureVerification then
  begin
    Logger.Trace('Self-update signature verification skipped (test mode).');
    Exit;
  end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  // The bundle and its `file_hash` come from the same server, so the hash
  // alone is not a trust anchor. Anchor instead on the Authenticode signature
  // of the new tms.exe and pin to the same publisher that signed the
  // currently running binary (a TOFU model — whoever signed the binary you
  // are running today is who is allowed to ship updates).
  var ExeName := TPath.GetFileName(CurrentExePath);

    // 1. Authenticode signature must be valid (file unmodified, chain trusted).
    try
      CheckIsCodeSigned(StagedExePath);
    except on ex: Exception do
      raise Exception.CreateFmt(
        'Self-update aborted: the new "%s" does not have a valid Authenticode signature: %s',
        [ExeName, ex.Message]);
    end;

    // 2. Pin to the same publisher that signed the currently-running binary.
    var CurrentSigner: string;
    try
      CurrentSigner := GetSigningCertificateSubject(CurrentExePath);
    except on ex: Exception do
      raise Exception.CreateFmt(
        'Self-update aborted: cannot read signing certificate of currently-running "%s" ' +
        '(typically because the binary is not Authenticode-signed): %s',
        [ExeName, ex.Message]);
    end;
    if CurrentSigner = '' then
      raise Exception.Create(
        'Self-update aborted: the currently-running tms.exe has no signing-certificate subject; ' +
        'refusing to install an update from an unverifiable source.');

    var NewSigner := GetSigningCertificateSubject(StagedExePath);
    if not SameText(NewSigner, CurrentSigner) then
      raise Exception.CreateFmt(
        'Self-update aborted: the new "%s" was signed by "%s", but the current binary was ' +
        'signed by "%s". This is likely because our signing certificate has changed. Please download the latest smartsetup version from %s.',
        [ExeName, NewSigner, CurrentSigner, 'https://github.com/tmssoftware/smartsetup/releases/latest/download/tmssmartsetup.zip']);
  {$ELSE}
  // Authenticode is Windows-only. Until we add a cross-platform signing
  // mechanism (e.g. a detached minisign signature on the bundle, with the
  // public key embedded in the binary), self-update on non-Windows runs
  // without signature verification. Surface the gap as a warning rather
  // than refuse the update outright, so existing non-Windows users aren't
  // broken.
  Logger.Trace(
    'WARNING: self-update signature verification is only implemented on Windows. ' +
    'The downloaded bundle has not been verified.');
  {$ENDIF}
end;

function AllowedExtension(const ext: string): boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := SameText(ext, '.exe') or SameText(ext, '.dll');
  {$ELSE}
  Result := ext = '';
  {$ENDIF}
end;

procedure VerifySelfUpdateBundle(const CurrentExePath, BundleFolder: string);
begin
  var Files := TDirectory.GetFiles(BundleFolder, '*.*', TSearchOption.soTopDirectoryOnly);
  for var f in Files do
  begin
    // If we allow arbitrary extensions, a malicious update could say provide a tms.bat file instead of tms.exe, which we can't validate
    // Even if we validate tms.exe, the file could contain many other files that we can't verify and could be malign. So we will only accept exes in the zip.
    if not AllowedExtension(TPath.GetExtension(f))
      then raise Exception.Create('The update contains unknown extensions and can''t be validated. Please download the latest smartsetup version from ' +
           'https://github.com/tmssoftware/smartsetup/releases/latest/download/tmssmartsetup.zip');

    VerifySelfUpdateFile(CurrentExePath, f);

  end;

end;


end.

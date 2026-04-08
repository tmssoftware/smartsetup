unit VCS.Sanitizer;
{$I ../../tmssetup.inc}

interface

/// <summary>
/// Validates that a VCS reference (tag, branch, commit hash) contains only
/// safe characters. Allowed: letters, digits, dots, hyphens, underscores,
/// slashes, plus signs, and non-ASCII unicode (e.g. Chinese, Arabic).
/// Raises an exception if any other character is found.
/// </summary>
procedure ValidateVCSVersion(const aVersion: string);

/// <summary>
/// Validates that a VCS URL contains only characters expected in a
/// repository URL. Allowed: letters, digits, the set . - _ / + : @ ~ % ? = #
/// and non-ASCII unicode.
/// Raises an exception if any other character is found.
/// </summary>
procedure ValidateVCSUrl(const aURL: string);

/// <summary>
/// Validates that a file path contains only characters expected in a
/// file system path. Allowed: letters, digits, the set . - _ / \ : ( ) + ~ @ , # spaces,
/// and non-ASCII unicode.
/// Raises an exception if any other character is found.
/// </summary>
procedure ValidateVCSFilePath(const aPath: string);

implementation
uses SysUtils;

function IsAsciiLetterOrDigit(const ch: Char): boolean; inline;
begin
  Result := ((ch >= 'a') and (ch <= 'z'))
         or ((ch >= 'A') and (ch <= 'Z'))
         or ((ch >= '0') and (ch <= '9'));
end;

function IsNonAscii(const ch: Char): boolean; inline;
begin
  // Unicode characters above ASCII are safe — shell metacharacters are all ASCII.
  Result := Ord(ch) > $7F;
end;

function IsAllowedVersionChar(const ch: Char): boolean; inline;
begin
  // letters, digits, . - _ / + and non-ASCII unicode
  Result := IsAsciiLetterOrDigit(ch)
         or (ch = '.') or (ch = '-') or (ch = '_')
         or (ch = '/') or (ch = '+')
         or IsNonAscii(ch);
end;

function IsAllowedUrlChar(const ch: Char): boolean; inline;
begin
  // letters, digits, . - _ / + : @ ~ % ? = # and non-ASCII unicode
  Result := IsAsciiLetterOrDigit(ch)
         or (ch = '.') or (ch = '-') or (ch = '_')
         or (ch = '/') or (ch = '+') or (ch = ':')
         or (ch = '@') or (ch = '~') or (ch = '%')
         or (ch = '?') or (ch = '=') or (ch = '#')
         or IsNonAscii(ch);
end;

function IsAllowedPathChar(const ch: Char): boolean; inline;
begin
  // letters, digits, . - _ / \ : ( ) + ~ @ , space and non-ASCII unicode
  Result := IsAsciiLetterOrDigit(ch)
         or (ch = '.') or (ch = '-') or (ch = '_')
         or (ch = '/') or (ch = '\') or (ch = ':')
         or (ch = '(') or (ch = ')') or (ch = ' ')
         or (ch = '+') or (ch = '~') or (ch = '@')
         or (ch = ',') or (ch = '#')
         or IsNonAscii(ch);
end;

procedure ValidateVCSVersion(const aVersion: string);
begin
  if aVersion.Trim = '' then exit;
  if aVersion.Trim = '*' then exit;

  for var i := 1 to Length(aVersion) do
  begin
    if not IsAllowedVersionChar(aVersion[i]) then
      raise Exception.CreateFmt(
        'Invalid character in version "%s": ''%s'' (0x%s) is not allowed. ' +
        'Only letters, digits, dots, hyphens, underscores, slashes and plus signs are permitted.',
        [aVersion, aVersion[i], IntToHex(Ord(aVersion[i]), 2)]);
  end;
end;

procedure ValidateVCSUrl(const aURL: string);
begin
  if aURL.Trim = '' then exit;

  for var i := 1 to Length(aURL) do
  begin
    if not IsAllowedUrlChar(aURL[i]) then
      raise Exception.CreateFmt(
        'Invalid character in repository URL "%s": ''%s'' (0x%s) is not allowed. ' +
        'Only letters, digits and the characters . - _ / + : @ ~ %% ? = # are permitted.',
        [aURL, aURL[i], IntToHex(Ord(aURL[i]), 2)]);
  end;
end;

procedure ValidateVCSFilePath(const aPath: string);
begin
  if aPath.Trim = '' then exit;

  for var i := 1 to Length(aPath) do
  begin
    if not IsAllowedPathChar(aPath[i]) then
      raise Exception.CreateFmt(
        'Invalid character in file path "%s": ''%s'' (0x%s) is not allowed. ' +
        'Only letters, digits and the characters . - _ / \ : ( ) and spaces are permitted.',
        [aPath, aPath[i], IntToHex(Ord(aPath[i]), 2)]);
  end;
end;

end.
